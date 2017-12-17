;;; elbank-transaction.el --- Major mode for displaying a transaction  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Nicolas Petton

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
(require 'map)

(require 'elbank-common)

(defvar elbank-transaction-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'elbank-quit)
    map)
  "Keymap for `elbank-transaction-mode'.")

(define-derived-mode elbank-transaction-mode nil "Elbank Transaction"
  "Major mode for viewing a single transaction.

\\{elbank-transaction-mode-map}"
  (read-only-mode))

;;;###autoload
(defun elbank-show-transaction (transaction)
  "Show the details of TRANSACTION in a separate buffer."
  (let ((buf (get-buffer-create "*Elbank transaction*")))
    (pop-to-buffer buf)
    (elbank-transaction-mode)
    (elbank-transaction--refresh transaction)))

(defun elbank-transaction--refresh (transaction)
  "Populate the current buffer with the details of TRANSACTION."
  (let ((inhibit-read-only t)
	(width (1+ (seq-reduce (lambda (acc elt)
				 (max acc (seq-length (symbol-name elt))))
			       elbank-report-available-columns
			       0))))
    (erase-buffer)
    (seq-doseq (key elbank-report-available-columns)
      (let ((label (capitalize (format "%s:" key))))
	(dotimes (_ (- width (seq-length label)))
	  (insert " "))
	(insert label)
	(put-text-property (point-at-bol) (point)
			   'face
			   'font-lock-keyword-face)
	(insert " ")
	(insert (format "%s" (elbank-transaction-elt transaction key)))
	(insert "\n")))))

(provide 'elbank-transaction)
;;; elbank-transaction.el ends here
