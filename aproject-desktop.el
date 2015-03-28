;;; aproject-desktop.el --- Desktop plugin for aproject

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

;; This library implements desktop plugin for aproject.

;;; Code:

(require 'aproject)
(require 'desktop)

(defcustom aproject-plugin-desktop t
  "Plugin for desktop."
  :group 'aproject)

(add-aproject-init
 (when aproject-plugin-desktop
   (setq desktop-save t
         desktop-restore-frames nil
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

(provide 'aproject-desktop)
;;; aproject-desktop.el ends here
