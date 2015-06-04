;;; aproject-recentf.el --- Recentf plugin for aproject

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

;; This library implements recentf plugin for aproject.

;;; Code:

(require 'aproject)
(eval-when-compile
  (require 'ido)
  (require 'recentf))

(defcustom aproject-plugin-recentf t
  "Plugin for recentf."
  :group 'aproject)

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

(defalias 'aproject-recentf-view
  (symbol-function 'recentf-open-files))

;;;###autoload
(defun aproject-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file."
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

(provide 'aproject-recentf)
;;; aproject-recentf.el ends here
