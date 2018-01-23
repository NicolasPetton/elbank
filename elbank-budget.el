;;; elbank-budget.el --- Elbank budgeting functionality  -*- lexical-binding: t; -*-

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
(require 'map)
(require 'cl-lib)

(require 'elbank-common)
(require 'elbank-progressbar)

(declare-function elbank-report "elbank-report.el")

;;;###autoload
(defgroup elbank-budget nil
  "Elbank budget settings"
  :group 'elbank)

;;;###autoload
(defcustom elbank-budget nil
  "Monthly budget by category of transactions.

Keys are category names as defined in `elbank-categories'."
  :type '(alist :key-type (string :tag "Category name")
		:value-type (number :tag "Monthly budget")))

(defvar elbank-budget-mode-map
  (copy-keymap elbank-base-report-mode-map)
  "Keymap for `elbank-budget-mode'.")

(define-derived-mode elbank-budget-mode elbank-base-report-mode "Elbank Budget"
  "Major mode for viewing a monthly budget.

\\{elbank-budget-mode-map}"
  (add-hook 'elbank-base-report-refresh-hook 'elbank-budget-refresh nil t))

;;;###autoload
(defun elbank-budget-report ()
  "Build a budget report for the last month.

Return the report buffer."
  (interactive)
  (let ((buf (get-buffer-create "*elbank budget report*")))
    (pop-to-buffer buf)
    (elbank-budget-mode)
    (setq elbank-report-period
	  `(month ,(car (last (elbank-transaction-months)))))
    (elbank-budget-refresh)
    buf))

(defun elbank-budget-refresh ()
  "Update the budget report."
  (let ((inhibit-read-only t)
	(elbank-budget-data (elbank-budget--get-data)))
    (erase-buffer)
    (elbank-budget--insert-header)
    (elbank-budget--insert-customize-link)
    (seq-do #'elbank-budget--insert-line
	    (seq-sort (lambda (a b)
			(string< (car a) (car b)))
		      elbank-budget-data))
    (elbank-budget--insert-footer elbank-budget-data)))

(defun elbank-budget--insert-header ()
  "Insert the header for the budget buffer."
      (insert (format "Budget report for %s"
		    (elbank-format-period elbank-report-period)))
    (put-text-property (point-at-bol) (point)
		       'face 'elbank-header-face)
    (insert "\n\n"))

(defun elbank-budget--insert-footer (data)
  "Insert the footer with a summary of the budget DATA."
  (insert "\n")
  (insert " Total: ")
  (let ((beg (point)))
    (insert (format "%.2f"
		    (seq-reduce #'+
				(seq-map #'cadr data)
				0)))
    (put-text-property beg (point) 'face 'bold))
  (insert " of ")
  (let ((beg (point)))
    (insert (format "%.2f"
		    (seq-reduce #'+
				(seq-map #'cl-caddr data)
				0)))
    (put-text-property beg (point) 'face 'bold))
  (insert " budgeted."))

(defun elbank-budget--insert-customize-link ()
  "Insert a button to customize `elbank-budget'."
  (insert "[Customize budgets]")
  (make-text-button (point-at-bol) (point)
		    'follow-link t
		    'action (lambda (&rest _)
			      (customize-variable 'elbank-budget)))
  (insert "\n\n"))

(defun elbank-budget--insert-line (budget-entry)
  "Insert the value for BUDGET-ENTRY with a progress bar."
  (let* ((label (car budget-entry))
	 (spent (cadr budget-entry))
	 (budgeted (cl-caddr budget-entry))
	 (percentage (round (* 100 (/ spent budgeted)))))
    (elbank-budget--insert-line-header label)
    (elbank-insert-progressbar percentage 40)
    (insert (format " %.2f of %.2f budgeted" spent budgeted))
    (insert "\n\n")))

(defun elbank-budget--insert-line-header (label)
  "Insert a header with LABEL for a budget category."
  (let ((width (1+ (seq-reduce (lambda (acc cat)
				 (max (seq-length (car cat)) acc))
			       elbank-budget
			       0))))
    (dotimes (_ (- width (seq-length label)))
      (insert " "))
    (let ((beg (point)))
      (insert (format "%s" label))
      (make-text-button beg (point)
			'follow-link t
			'action (lambda (&rest _)
				  (elbank-report :period elbank-report-period
						 :category label))))
    (insert " ")))

(defun elbank-budget--get-data ()
  "Return an assocation list of budget data.

Keys are budgeted categories.
Values are lists of spent amounts and budgeted amounts for a category."
  (let ((data (map-apply (lambda (key val)
			   `(,key ,(- (elbank-sum-transactions val))
				  ,(map-elt elbank-budget key)))
			 (elbank-budget--transactions-by-budget))))
    ;; Add budgeted categories without transaction.
    (seq-do (lambda (budget)
	      (unless (map-elt data (car budget))
		(map-put data (car budget) `(0 ,(cdr budget)))))
	    elbank-budget)
    data))


(defun elbank-budget--transactions-by-budget ()
  "Return an assocation list of all transactions grouped by budget category."
  (seq-filter
   (lambda (elt)
     (not (null (car elt))))
   (seq-group-by
    (lambda (trans)
      (seq-some
       (lambda (cat)
	 (when (elbank-transaction-in-category-p trans cat)
	   cat))
       (seq-map #'car elbank-budget)))
    (elbank-filter-transactions (elbank-all-transactions)
				:period elbank-report-period))))

(provide 'elbank-budget)
;;; elbank-budget.el ends here
