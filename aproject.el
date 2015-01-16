;;; aproject.el --- A simple project framework for Emacs

;; Copyright (C) 2015 Vietor Liu

;; Author: Vietor Liu <vietor.liu@gmail.com>
;; Keywords: environment project
;; URL: https://github.com/vietor/aproject
;; Version: DEV

;; This file is not part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library allows the user to use Emacs on multiple projects.
;; Each project has it's ".aproject" dirctory for store some files,
;; like: bookmarks, desktop, etc.

;;; Code:

(defconst aproject-dirname ".aproject")

(defvar aproject-project nil
  "The flag for aproject initialize as *PROJECT*.")
(defvar aproject-rootdir nil
  "The aproject's working directory.")
(defvar aproject-storedir nil
  "The aproject's store directory.")
(defvar aproject-init-hook nil
  "Hooks to run in aproject init.")
(defvar aproject-before-change-hook nil
  "Hooks to run before aproject changed.")
(defvar aproject-after-change-hook nil
  "Hooks to run after aproject changed.")

(defun aproject-root-file (name)
  "Get the aproject's workding directory NAME file."
  (expand-file-name name aproject-rootdir))

(defun aproject-store-file (name)
  "Get the aproject's store directory NAME file."
  (expand-file-name name aproject-storedir))

(defmacro add-aproject-init (&rest body)
  "Add hook to aproject-init-hook, in BODY."
  `(add-hook 'aproject-init-hook (lambda () ,@body)))

(defmacro before-aproject-change (&rest body)
  "Add hook to aproject-before-change-hook, in BODY."
  `(add-hook 'aproject-before-change-hook (lambda () ,@body)))

(defmacro after-aproject-change (&rest body)
  "Add hook to aproject-after-change-hook, in BODY."
  `(add-hook 'aproject-after-change-hook (lambda () ,@body)))

(defun aproject--expand-dirname (name &optional parent)
  "Convert directory NAME for aproject usage, PARENT start with if NAME is relative."
  (directory-file-name (expand-file-name name parent)))

(defun aproject--change-rootdir (rootdir &optional force)
  "Change aproject's ROOTDIR directory, initialize as *PROJECT* when FORCE is t."
  (unless (file-directory-p rootdir)
    (error "%s is not directory" rootdir))
  (let ((storedir-exists nil)
        (storedir (aproject--expand-dirname aproject-dirname rootdir))
        (storedir-global (aproject--expand-dirname aproject-dirname (getenv "HOME"))))
    (setq storedir-exists (file-directory-p storedir))
    (when (and (not force) (not storedir-exists))
      (setq storedir storedir-global)
      (setq storedir-exists (file-directory-p storedir)))
    (unless storedir-exists
      (make-directory storedir t))
    (when (stringp aproject-storedir)
      (run-hooks 'aproject-before-change-hook)
      ;; Kill all buffers and switch scratch buffer
      (delete-other-windows)
      (switch-to-buffer (get-buffer-create "*scratch*"))
      (mapc 'kill-buffer (cdr (buffer-list (current-buffer)))))
    (cd rootdir)
    (setq aproject-project
          (not (eq
                t
                (compare-strings
                 storedir nil nil
                 storedir-global nil nil t))))
    (setq aproject-rootdir rootdir)
    (setq aproject-storedir storedir)
    (run-hooks 'aproject-after-change-hook)))

(defun aproject-change-project ()
  "Change current project."
  (interactive)
  (let (rootdir)
    (setq rootdir (read-directory-name "Change to project directory: " aproject-rootdir))
    (when (or (equal "" rootdir)
              (not (file-accessible-directory-p rootdir)))
      (error "You not have permission to open directory"))
    (setq rootdir (aproject--expand-dirname rootdir))
    (let ((len (length aproject-rootdir)))
      (when (and aproject-project
                 (eq t (compare-strings aproject-rootdir 0 len rootdir 0 len t)))
        (error "You need change to difference project directory")))
    (aproject--change-rootdir rootdir t)))

(defun aproject--initialize ()
  "Initialize aproject environment."
  (let ((aproject--init-switch nil)
        (aproject--init-rootdir nil))
    ;; Parse parameter from command line
    (when (member "-project" command-line-args)
      (setq aproject--init-switch t)
      (setq command-line-args (delete "-project" command-line-args)))
    (when (> (length command-line-args) 1)
      ;; Ignore multiple dirname from command line
      (let ((dir (elt command-line-args (- (length command-line-args) 1))))
        (when (file-directory-p dir)
          (setq aproject--init-rootdir dir)
          (setq command-line-args (delete dir command-line-args)))))
    (unless aproject--init-rootdir
      (setq aproject--init-rootdir (or  (getenv "PWD") (getenv "HOME"))))
    (setq aproject--init-rootdir
          (aproject--expand-dirname aproject--init-rootdir))
    (run-hooks 'aproject-init-hook)
    (aproject--change-rootdir aproject--init-rootdir
                              aproject--init-switch)))

(add-hook 'after-init-hook 'aproject--initialize)

;;; Plugins:

;; For bookmark
;;
(require 'bookmark)

(defvar aproject-plugin-bookmark t
  "Plugin for bookmark.")

(before-aproject-change
 (when aproject-plugin-bookmark
   (bookmark-save)))

(after-aproject-change
 (when aproject-plugin-bookmark
   (setq bookmark-default-file
         (aproject-store-file "bookmarks"))
   (ignore-errors
     (bookmark-load bookmark-default-file t t))))

;; For recentf
;;
(require 'recentf)

(defvar aproject-plugin-recentf t
  "Plugin for recentf.")

(add-aproject-init
 (when aproject-plugin-recentf
   (recentf-mode 1)
   (setq recentf-max-menu-items 64)
   (setq recentf-exclude (list ".*\.aproject.*"))))

(before-aproject-change
 (when aproject-plugin-recentf
   (recentf-save-list)))

(after-aproject-change
 (when aproject-plugin-recentf
   (setq recentf-save-file
         (aproject-store-file "recentf"))
   (recentf-load-list)))

;; For ido
;;
(require 'ido)

(defvar aproject-plugin-ido t
  "Plugin for ido.")

(before-aproject-change
 (when aproject-plugin-ido
   (ido-save-history)))

(after-aproject-change
 (when aproject-plugin-ido
   (setq ido-save-directory-list-file
         (aproject-store-file "ido"))
   (ido-load-history t)))

;; For desktop
;;
(require 'desktop)

(defvar aproject-plugin-desktop t
  "Plugin for desktop.")

(add-aproject-init
 (when aproject-plugin-desktop
   (setq desktop-save t
         desktop-load-locked-desktop t
         desktop-base-file-name "desktop")
   (desktop-save-mode 1)))

(before-aproject-change
 (when aproject-plugin-desktop
   (desktop-save aproject-storedir)))

(after-aproject-change
 (when aproject-plugin-desktop
   (setq desktop-path
         (list aproject-storedir))
   (desktop-read)))

(provide 'aproject)
;;; aproject.el ends here
