;;; elbank-report.el --- Elbank report functionality  -*- lexical-binding: t; -*-

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
(require 'subr-x)
(require 'button)

(require 'elbank-common)
(require 'elbank-transaction)

;;;###autoload
(defgroup elbank-report nil
  "Elbank report settings"
  :prefix "elbank-report-"
  :group 'elbank)

;;;###autoload
(defcustom elbank-report-columns '(date label category amount)
  "List of transaction columns to print in reports."
  :type '(repeat (symbol :tag "Key")))

;;;###autoload
(defcustom elbank-saved-monthly-reports nil
  "Saved report filters for monthly reports.

\"Group by\" can be either one of the available columns or nil.

\"Sort by\" can be either one of the available columns or nil.
When nil, transactions are sorted using the first column of the
report.

\"Category\" can be any string (or empty for no category filter).

Available columns:
- `date'
- `rdate' (real date)
- `label'
- `raw' (raw transaction text)
- `category'
- `amount'."
    :type `(repeat (list (string :tag "Name")
		       (string :tag "Category")
		       (symbol :tag "Group by")
		       (symbol :tag "Sort by" :value date)
		       (repeat :tag "Columns"
			       :value ,elbank-report-columns
			       (symbol :tag "Column"))
		       (boolean :tag "Reverse sort"))))

;;;###autoload
(defcustom elbank-saved-yearly-reports nil
  "Saved report filters for yearly reports.

\"Group by\" can be either one of the available columns or nil.

\"Sort by\" can be either one of the available columns or nil.
When nil, transactions are sorted using the first column of the
report.

\"Category\" can be any string (or empty for no category filter).

Available columns:
- `date'
- `rdate' (real date)
- `label'
- `raw' (raw transaction text)
- `category'
- `amount'."
  :type `(repeat (list (string :tag "Name")
		       (string :tag "Category")
		       (symbol :tag "Group by")
		       (symbol :tag "Sort by" :value date)
		       (repeat :tag "Columns"
			       :value ,elbank-report-columns
			       (symbol :tag "Column"))
		       (boolean :tag "Reverse sort"))))

(defvar elbank-report-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'elbank-quit)
    (define-key map (kbd "f c") #'elbank-report-filter-category)
    (define-key map (kbd "f a") #'elbank-report-filter-account)
    (define-key map (kbd "f p") #'elbank-report-filter-period)
    (define-key map (kbd "G") #'elbank-report-group-by)
    (define-key map (kbd "S") #'elbank-report-sort-by)
    (define-key map (kbd "s") #'elbank-report-sort-reverse)
    (define-key map (kbd "n") #'forward-button)
    (define-key map (kbd "p") #'backward-button)
    (define-key map [tab] #'forward-button)
    (define-key map [backtab] #'backward-button)
    (define-key map (kbd "M-n") #'elbank-report-forward-period)
    (define-key map (kbd "M-p") #'elbank-report-backward-period)
    (define-key map (kbd "g") #'elbank-report-refresh)
    map)
  "Keymap for `elbank-report-mode'.")

(define-derived-mode elbank-report-mode nil "Elbank Report"
  "Major mode for viewing a report.

\\{elbank-report-mode-map}"
  (setq-local truncate-lines nil)
  (read-only-mode))

(defvar elbank-report-amount-columns '(amount)
  "List of columns for which values are numbers.")
(make-variable-buffer-local 'elbank-report-amount-columns)

(defvar elbank-report-group-by nil
  "Column by which transactions are grouped.")
(make-variable-buffer-local 'elbank-report-group-by)

(defvar elbank-report-sort-by nil
  "Column uses for sorting transactions.")
(make-variable-buffer-local 'elbank-report-sort-by)

(defvar elbank-report-sort-reversed nil
  "Reverse the sorting order when non-nil.")
(make-variable-buffer-local 'elbank-report-sort-reversed)

(defvar elbank-report-column-widths nil
  "List of column widths required to correctly display a report.")
(make-variable-buffer-local 'elbank-report-widths)

(defvar elbank-report-account-id nil
  "Account filter used in a report buffer.")
(make-variable-buffer-local 'elbank-report-account-id)

(defvar elbank-report-period nil
  "Period filter used in a report buffer.")
(make-variable-buffer-local 'elbank-report-period)

(defvar elbank-report-category nil
  "Category filter used in a report buffer.")
(make-variable-buffer-local 'elbank-report-category)

(defvar elbank-report-inhibit-update nil
  "When non-nil, do not perform a report update after setting a filter.")

;;;###autoload
(cl-defun elbank-report (&key account-id period category group-by sort-by reverse-sort columns)
  "Build a report for transactions matching ACCOUNT-ID PERIOD and CATEGORY.

When called interactively, prompt for ACCOUNT-ID, PERIOD and CATEGORY.

Build the report for COLUMNS when non-nil,
`elbank-report-columns' otherwise.

Transactions are grouped by the GROUP-BY column when non-nil.

Transactions are sorted by the SORT-BY column, or by the first
column if nil.

When a PERIOD is provided, append a sum row to the report.

Return the report buffer."
  (interactive)
  (let ((buf (generate-new-buffer "*elbank report*")))
    (pop-to-buffer buf)
    (elbank-report-mode)
    (setq elbank-report-category category)
    (setq elbank-report-period period)
    (setq elbank-report-account-id account-id)
    (setq elbank-report-group-by group-by)
    (setq elbank-report-sort-by sort-by)
    (setq elbank-report-sort-reversed reverse-sort)
    (when columns
      (setq-local elbank-report-columns columns))
    (when (called-interactively-p 'interactive)
        (let ((elbank-report-inhibit-update t))
	  (elbank-report-filter-account)
	  (elbank-report-filter-period)
	  (elbank-report-filter-category)))
    (elbank-report-refresh)
    buf))

(defun elbank-report-filter-category ()
  "Prompt for a category and update the report buffer."
  (interactive)
  (setq elbank-report-category
	(completing-read "Category: " (map-keys elbank-categories)
			 nil
			 nil
			 (or elbank-report-category "")))
  (elbank-report-refresh))

(defun elbank-report-filter-account ()
  "Prompt for an account and update the report buffer."
  (interactive)
  (let* ((accounts (map-elt elbank-data 'accounts))
	 (labels (seq-map (lambda (account)
			    (map-elt account 'label))
			  accounts))
	 (label (completing-read "Select account: " labels)))
    (setq elbank-report-account-id
	  (when-let ((position (seq-position labels label)))
	    (intern (map-elt (seq-elt accounts position) 'id)))))
  (elbank-report-refresh))

(defun elbank-report-filter-period ()
  "Prompt for a period to select for the current report."
  (interactive)
  (let ((type (completing-read "Period type: " '(month year))))
    (pcase type
      ("year" (elbank-report-filter-year))
      ("month" (elbank-report-filter-month))
      (_ (setq elbank-report-period nil)
	 (elbank-report-refresh)))))

(defun elbank-report-filter-year ()
  "Prompt for a year to select for the current report."
  (interactive)
  (let* ((years (seq-reverse (elbank-transaction-years)))
	 (labels (seq-map (lambda (year)
			    (format-time-string "%Y" year))
			  years))
	 (label (completing-read "Select year: " labels)))
    (setq elbank-report-period
	  (when-let ((position (seq-position labels label)))
	    `(year ,(seq-elt years position))))
    (elbank-report-refresh)))

(defun elbank-report-filter-month ()
  "Prompt for a month to select for the current report."
  (interactive)
  (let* ((months (seq-reverse (elbank-transaction-months)))
	 (labels (seq-map (lambda (month)
			    (format-time-string "%B %Y" month))
			  months))
	 (label (completing-read "Select month: " labels)))
    (setq elbank-report-period
	  (when-let ((position (seq-position labels label)))
	    (setq elbank-report-period `(month ,(seq-elt months position)))))
    (elbank-report-refresh)))

(defun elbank-report-group-by (column-name)
  "Prompt for a COLUMN-NAME to group transactions."
  (interactive (list (completing-read "Group by: "
				      elbank-report-available-columns)))
  (setq-local elbank-report-group-by
	      (if (string-empty-p column-name)
		  nil
		(intern column-name)))
  (elbank-report-refresh))

(defun elbank-report-sort-by (column-name)
  "Prompt for a COLUMN-NAME to sort the current report."
  (interactive (list (completing-read "Sort by: "
				      elbank-report-available-columns)))
  (setq-local elbank-report-sort-by
	      	      (if (string-empty-p column-name)
		  nil
		  (intern column-name)))
  (elbank-report-refresh))

(defun elbank-report-sort-reverse ()
  "Reverse the sort order of the current report."
  (interactive)
  (setq-local elbank-report-sort-reversed
	      (not elbank-report-sort-reversed))
  (elbank-report-refresh))

(defun elbank-report-forward-period (&optional n)
  "Select the next N period and update the current report.
If there is no period filter, signal an error."
  (interactive "p")
  (unless elbank-report-period
    (user-error "No period filter for the current report"))
  (let* ((periods (pcase (car elbank-report-period)
		    (`year (elbank-transaction-years))
		    (`month (elbank-transaction-months))))
	 (cur-index (seq-position periods (cadr elbank-report-period)))
	 (new-index (+ n cur-index))
	 (period (seq-elt periods new-index)))
    (if period
	(progn
	  (setq elbank-report-period (list (car elbank-report-period)
					   period))
	  (elbank-report-refresh))
      (user-error "No more periods"))))

(defun elbank-report-backward-period (&optional n)
  "Select the previous N period and update the current report.
If there is no period filter, signal an error."
  (interactive "p")
  (elbank-report-forward-period (- n)))

(defun elbank-report-refresh ()
  "Update the report in the current buffer.
When `elbank-report-inhibit-update' is non-nil, do not update."
  (interactive)
  (unless elbank-report-inhibit-update
    (let ((inhibit-read-only t)
	  (transactions (elbank-report--with-categories
			 (elbank-filter-transactions
			  :account-id elbank-report-account-id
			  :category elbank-report-category
			  :period elbank-report-period)))
	  (inhibit-read-only t))
      (save-excursion
	(erase-buffer)
	(elbank-report--update-column-widths transactions)
	(elbank-report--insert-preambule)
	(elbank-report--insert-column-titles)
	(elbank-report--insert-separator "═")
	(if elbank-report-group-by
	    (elbank-report--insert-groups transactions)
	  (elbank-report--insert-transactions transactions))
	(when elbank-report-period
	  (elbank-report--insert-separator "═")
	  (elbank-report--insert-sum transactions))))))

(defun elbank-report--with-categories (transactions)
  "Return the list TRANSACTIONS with categories.
Lookup and a `category' to each item of TRANSACTIONS."
  (seq-map (lambda (trans)
	     (cons (cons 'category
			 (elbank-transaction-category trans))
		   trans))
	   transactions))

(defun elbank-report--insert-preambule ()
  "Display the report filters in the current buffer."
  (if (or elbank-report-account-id
	  elbank-report-period
	  elbank-report-category)
      (progn
	(seq-do (lambda (filter)
		  (when (cdr filter)
		    (insert (car filter))
		    (put-text-property (point-at-bol) (point)
				       'face 'elbank-subheader-face)
		    (insert " ")
		    (insert (format "%s" (cdr filter)))
		    (insert "\n")))
		`(("Account:" . ,(and elbank-report-account-id
				      (map-elt (elbank-account elbank-report-account-id)
					       'label)))
		  ("Period:" . ,(and elbank-report-period
				     (elbank-format-period elbank-report-period)))
		  ("Category:" . ,elbank-report-category))))
    (progn
      (insert "All transactions")
      (put-text-property (point-at-bol) (point)
			 'face 'elbank-subheader-face)
      (insert "\n")))
  (insert "\n"))

(defun elbank-report--update-column-widths (transactions)
  "Locally set report columns widths needed to print TRANSACTIONS."
  (setq elbank-report-column-widths
	(seq-map-indexed
	 (lambda (col index)
	   (let ((row-max-width
		  (seq-reduce (lambda (acc trans)
				(max acc
				     (seq-length
				      (map-elt trans col ""))))
			      transactions
			      0)))
	     (+ 2 (max row-max-width
		       (seq-length (symbol-name
				    (seq-elt elbank-report-columns
					     index)))))))
	 elbank-report-columns)))

(defun elbank-report--cell (transaction column)
  "Return the text for the cell for TRANSACTION at COLUMN."
  (or (pcase column
	(`label (elbank-report--transaction-link transaction))
	(_ (map-elt transaction column "")))
      ""))

(defun elbank-report--transaction-link (transaction)
  "Return a button text with the label of TRANSACTION.
When clicking the button, jump to the transaction."
  (with-temp-buffer
    (insert (map-elt transaction 'label))
    (make-text-button (point-at-bol) (point)
		      'follow-link t
		      'action
		      (lambda (&rest _)
			(elbank-show-transaction transaction)))
    (buffer-string)))

(defun elbank-report--insert-column-titles ()
  "Insert the report headers into the current buffer."
  (elbank-report--insert-title-row
   (seq-map (lambda (col) (capitalize (symbol-name col)))
	    elbank-report-columns)))

(defun elbank-report--insert-transactions (transactions)
  "Insert TRANSACTIONS rows the current buffer."
  (seq-do (lambda (trans)
	    (elbank-report--insert-row
	     (seq-map (lambda (col)
			(format "%s"
				(elbank-report--cell trans col)))
		      elbank-report-columns)
	     t))
	  (elbank-report--sort-transactions transactions)))

(defun elbank-report--insert-groups (transactions)
  "Insert TRANSACTIONS grouped by a property.
The grouping property is defined by `elbank-report-group-by'."
  (seq-do (lambda (group)
	    (elbank-report--insert-separator " ")
	    (elbank-report--insert-title-row (list (or (car group) "None")))
	    (elbank-report--insert-separator)
	    (elbank-report--insert-transactions (cdr group))
	    (elbank-report--insert-separator)
	    (elbank-report--insert-sum (cdr group)))
	  (elbank-report--sort-groups
	   (seq-group-by (lambda (trans)
			   (map-elt trans elbank-report-group-by ""))
			 transactions))))

(defun elbank-report--insert-sum (transactions)
  "Insert the sum row for TRANSACTIONS the current buffer."
  (elbank-report--insert-row
   (seq-map (lambda (col)
	      (if (seq-contains elbank-report-amount-columns col)
		  (elbank--propertize-amount (elbank-sum-transactions transactions))
		""))
	    elbank-report-columns)))

(defun elbank-report--insert-row (row &optional propertize-amounts spacer)
  "Insert each element of ROW in the current buffer.

When PROPERTIZE-AMOUNTS is non-nil, insert amounts using
`elbank--propertize-amount'.  SPACER is used for padding if
non-nil."
  (let ((spacer (or spacer " ")))
    (seq-map-indexed
     (lambda (col index)
       (let* ((amount (seq-contains elbank-report-amount-columns col))
	      (raw-item (or (seq-elt row index) ""))
	      (item (if (and amount propertize-amounts)
			(elbank--propertize-amount raw-item)
		      raw-item))
	      (width (seq-elt elbank-report-column-widths index))
	      (padding (- width (seq-length item))))
	 (unless amount
	   (insert (format "%s%s%s" spacer item spacer)))
	 (dotimes (_ padding)
	   (insert spacer))
	 (when amount
	   (insert (format "%s%s%s" spacer item spacer)))))
     elbank-report-columns))
  (insert "\n"))

(defun elbank-report--insert-title-row (row)
  "Insert ROW as a title row.

Unlike `elbank-report--insert-row', elements of ROW are displayed
in bold."
  (let ((beg (point)))
    (elbank-report--insert-row row)
       (add-text-properties beg (point)
			    '(face bold))))

(defun elbank-report--insert-separator (&optional separator)
  "Insert a separator line in the current buffer.
Use SEPARATOR if non-nil,\"─\" otherwise."
  (elbank-report--insert-row (seq-map (lambda (_) "") elbank-report-columns)
			     nil
			     (or separator "─")))

(defun elbank-report--sort-transactions (transactions)
  "Sort TRANSACTIONS.

Transactions are sorted by `elbank-report-sort-by' if
non-nil, or by the first column if nil."
  (let ((sort-column (or elbank-report-sort-by
			 (car elbank-report-columns))))
    (elbank-report--sort transactions
			 (lambda (trans)
			   (map-elt trans sort-column ""))
			 (seq-contains elbank-report-amount-columns sort-column))))

(defun elbank-report--sort-groups (groups)
  "Sort GROUPS.

If the sorting column is an amount, GROUPS are sorted by summing
their transactions."
  (let* ((sort-column (or elbank-report-sort-by
			  (car elbank-report-columns)))
	 (amounts (seq-contains elbank-report-amount-columns sort-column)))
    (elbank-report--sort
     groups
     (lambda (group)
       (if amounts
	   (elbank-sum-transactions (cdr group))
	 (car group)))
     amounts)))

(defun elbank-report--sort (collection accessor &optional amounts)
  "Sort COLLECTION by ACCESSOR.

If AMOUNTS is non-nil, the sort is done by comparing numeric
values, converting items of collection to numbers if needed."
  (let ((sort-fn (if amounts
		     (lambda (a b)
		       (< (if (numberp a) a (string-to-number a))
			  (if (numberp b) b (string-to-number b))))
		   #'string-lessp)))
    (seq-sort (lambda (a b)
		(let ((sort (funcall sort-fn
				     (funcall accessor a)
				     (funcall accessor b))))
		  (if elbank-report-sort-reversed
		      (not sort)
		    sort)))
	      collection)))

(provide 'elbank-report)
;;; elbank-report.el ends here
