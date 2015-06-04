;;; aproject-ido.el --- Ido plugin for aproject

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

;; This library implements ido plugin for aproject.

;;; Code:

(require 'aproject)
(eval-when-compile
  (require 'ido))

(defcustom aproject-plugin-ido t
  "Plugin for ido."
  :group 'aproject)

(before-aproject-change
 (when aproject-plugin-ido
   (ido-save-history)))

(after-aproject-change
 (when aproject-plugin-ido
   (setq ido-save-directory-list-file
         (aproject-store-file "ido"))
   (ido-load-history t)))

(provide 'aproject-ido)
;;; aproject-ido.el ends here
