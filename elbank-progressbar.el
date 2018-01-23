;;; elbank-progressbar.el --- Progressbar widget used by elbank  -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2018  Nicolas Petton

;; Author: Nicolas Petton <nicolas@petton.fr>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'seq)

(require 'elbank-common)

(defgroup elbank-progressbar nil
  "Elbank progressbar."
  :group 'elbank)

(defface elbank-progressbar-box-face '((t . (:box 1)))
  "Face used to represent the progressbar."
  :group 'elbank-progressbar)

(defface elbank-progressbar-fill-face '((t . (:background "dark green" :weight bold)))
  "Face used to fill the progressbar."
  :group 'elbank-progressbar)

(defface elbank-progressbar-overflow-face '((t . (:background "firebrick" :weight bold)))
  "Face used as a warning for overflowing progressbars."
  :group 'elbank-progressbar)

(defun elbank-insert-progressbar (percent &optional width)
  "Insert a progressbar at point with PERCENT filled.
The of the progressbar is set by WIDTH and default to 50.

If PERCENT is greater than 100, fill the progressbar in red."
  (let* ((width (or width 50))
	 (filled-width (min width (* width (/ percent 100.0))))
	 (label (format "%s%%" percent))
	 (label-padding (/ (- filled-width (seq-length label)) 2.0))
	 (beg (point))
	 (col (current-column)))
    (dotimes (_ label-padding)
      (insert " "))
    (insert label)
    (dotimes (_ (min label-padding
		     (- width (- (current-column) col))))
      (insert " "))
    (when (not (zerop percent))
      (put-text-property beg (point)
			 'face (if (> percent 100)
				   'elbank-progressbar-overflow-face
				 'elbank-progressbar-fill-face)))
    (dotimes (_ (- width (- (current-column) col)))
      (insert " "))
    (let ((box (make-overlay beg (point))))
      (overlay-put box 'face 'elbank-progressbar-box-face))))

(provide 'elbank-progressbar)
;;; elbank-progressbar.el ends here
