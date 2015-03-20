;;; aproject-bookmark.el --- Bookmark plugin for aproject

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

;; This library implements bookmark plugin for aproject.

;;; Code:

(require 'aproject)
(require 'bookmark)

(defcustom aproject-plugin-bookmark t
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

(provide 'aproject-bookmark)
;;; aproject-bookmark.el ends here
