;;; aproject-environ.el --- Environment plugin for aproject

;; Copyright (C) 2015 Vietor Liu

;; Author: Vietor Liu <vietor.liu@gmail.com>
;; Package: aproject

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

;; This library implements Environment plugin for aproject.

;;; Code:

(require 'aproject)

(defcustom aproject-plugin-environ nil
  "Plugin for Environment."
  :group 'aproject)

(defvar aproject-environ-change-hook nil
  "Hooks to run after aproject environ changed.")

(defvar aproject--origin-exec-path ""
  "Stored origin exec path.")
(defvar aproject--origin-environment '()
  "Stored origin environment.")

(defun aproject--process-environment ()
  "Get process environment as alist."
  (let ((retval-alist '()))
    (dolist (line process-environment)
      (when (string-match-p "=" line 1)
        (string-match "=" line 1)
        (let ((left (match-beginning 0))
              (right (match-end 0)))
          (add-to-list 'retval-alist `(,(substring line 0 left) . ,(substring line right))))))
    retval-alist))

(defun aproject--restore-environ ()
  "Restore environment from init."
  (setq exec-path aproject--origin-exec-path)
  (let ((current-environment (aproject--process-environment)))
    (dolist (item aproject--origin-environment)
      (setenv (car item) (cdr item)))
    (dolist (item current-environment)
      (when (not (assoc (car item) aproject--origin-environment))
        (setenv (car item) nil)))))

(add-hook 'aproject-after-init-hook
          (lambda ()
            (setq aproject--origin-exec-path exec-path)
            (setq aproject--origin-environment (aproject--process-environment))))

(add-hook 'aproject-after-before-change-hook
          (lambda ()
            (when aproject-plugin-environ
              (aproject--restore-environ))))

(add-hook 'aproject-after-after-change-hook
          (lambda ()
            (when aproject-plugin-environ
              (run-hooks 'aproject-environ-change-hook))))

(provide 'aproject-environ)
;;; aproject-environ.el ends here
