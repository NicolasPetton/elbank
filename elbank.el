;;; elbank.el --- Personal finances reporting application  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Nicolas Petton

;; Author: Nicolas Petton <nicolas@petton.fr>
;; Version: 0.1
;; Keywords: tools, personal-finances
;; Package-Requires: ((emacs "25") (seq "2.16"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; Elbank is a personal finances reporting (and soon budgeting) application.
;; It uses Weboob (https://weboob.org) for scraping data.
;;
;; Data is stored as JSON in `elbank-data-file' which defaults to
;; `$HOME/.emacs.d/elbank-data.json'.
;;
;; Transactions are automatically categorized with `elbank-categories', an
;; association list of the form:
;;
;;   '(("category1" . ("regexp1" "regexp2"))
;;     (("category2" . ("regexp")))
;;
;; TODO:
;; - Add unit tests
;; - Implement budgeting

;;; Code:

(require 'json)
(require 'seq)
(require 'map)
(require 'subr-x)
(require 'button)
(require 'tabulated-list)
(eval-and-compile (require 'cl-lib))

(defgroup elbank nil
  "Elbank"
  :prefix "elbank-"
  :group 'tools)

(defcustom elbank-data-file (locate-user-emacs-file "elbank-data.json")
  "Location of the file used to store elbank data."
  :type '(file))

(defcustom elbank-boobank-executable "boobank"
  "Boobank executable."
  :type '(file))

(defcustom elbank-categories nil
  "Alist of categories of transactions.

Each category has an associated list of regular expressions.
A transaction's category is found by testing each regexp in order.

Example of categories

 (setq elbank-categories
   \\='((\"Expenses:Groceries\" . (\"walmart\" \"city market\"))
     (\"Income:Salary\" . (\"paycheck\"))))"
  :type '(alist :key-type (string :tag "Category name")
		:value-type (repeat (string :tag "Regexp"))))

(defface elbank-header-face '((t . (:inherit font-lock-keyword-face
					     :height 1.3)))
  "Face for displaying header in elbank."
  :group 'elbank)

(defface elbank-subheader-face '((t . (:weight bold
				       :height 1.1)))
  "Face for displaying sub headers in elbank."
  :group 'elbank)

(defface elbank-positive-amount-face '((t . (:inherit success :weight normal)))
  "Face for displaying positive amounts."
  :group 'elbank)

(defface elbank-negative-amount-face '((t . (:inherit error :weight normal)))
  "Face for displaying positive amounts."
  :group 'elbank)

(defface elbank-entry-face '((t . ()))
  "Face for displaying entries in elbank."
  :group 'elbank)

(defgroup elbank-report nil
  "Elbank report settings"
  :prefix "elbank-report-"
  :group 'elbank)

(defvar elbank-report-available-columns '(date rdate label raw category amount)
  "List of all available columns in reports.")

(defcustom elbank-report-columns '(date label category amount)
  "List of transaction columns to print in reports."
  :type '(repeat (symbol :tag "Key")))

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

(defvar elbank-data nil
  "Alist of all accounts and transactions.")


(defvar elbank-overview-buffer-name "*elbank overview*"
  "Name of the elbank overview buffer.")

(defvar elbank-overview-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'elbank-overview-update-buffer)
    (define-key map (kbd "u") #'elbank-overview-update-data)
    (define-key map (kbd "r") #'elbank-report)
    (define-key map (kbd "n") #'forward-button)
    (define-key map (kbd "p") #'backward-button)
    (define-key map [tab] #'forward-button)
    (define-key map [backtab] #'backward-button)
    (define-key map (kbd "q") #'elbank-quit)
    map)
  "Keymap for `elbank-overview-mode'.")

(define-derived-mode elbank-overview-mode nil "Elbank Overview"
  "Major mode for Elbank overview.

\\{elbank-overview-mode-map}"
  (read-only-mode))

;;;###autoload
(defun elbank-overview ()
  "Show an overview of all accounts."
  (interactive)
  (elbank--read-data)
  (unless elbank-data
    (when (yes-or-no-p "No data found, import from weboob?")
      (elbank-overview-update-data)))
  (if-let ((buf (get-buffer elbank-overview-buffer-name)))
      (progn
	(switch-to-buffer buf)
	(elbank-overview-update-buffer))
    (let ((buf (get-buffer-create elbank-overview-buffer-name)))
      (pop-to-buffer buf)
      (elbank-overview-mode)
      (elbank-overview-update-buffer))))

(defun elbank-quit ()
  "Kill the current buffer."
  (interactive)
  (quit-window t))

;;;###autoload
(defun elbank-overview-update-buffer ()
  "Update the overview buffer with the latest data."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (remove-overlays (point-min) (point-max))
    (insert "Bank accounts overview")
    (put-text-property (point-at-bol) (point)
		       'face 'elbank-header-face)
    (insert "\n\n")
    (elbank-overview--insert-accounts)
    (elbank-overview--insert-hr)
    (insert "\n")
    (insert "Saved reports")
    (put-text-property (point-at-bol) (point)
		       'face 'elbank-header-face)
    (insert " ")
    (let ((beg (point)))
      (insert "[Customize]")
      (make-text-button beg (point)
			'follow-link t
			'action (lambda (&rest _)
				  (customize-group 'elbank-report))))
    (insert "\n\n")
    (elbank-overview--insert-saved-reports)
    (goto-char (point-min))))

(defun elbank-overview-update-data ()
  "Read new data from boobank and update the buffer."
  (interactive)
  (elbank--fetch-data)
  (elbank-overview-update-buffer))

(defun elbank-overview--insert-hr ()
  "Insert a horizontal rule."
    ;; End line
    (let ((p (1+ (point))))
      (insert "\n\n")
      (put-text-property p (1+ p) 'face '(:underline t :inherit border))
      (overlay-put (make-overlay p (1+ p))
		   'before-string
		   (propertize "\n" 'face '(:underline t)
			       'display '(space :align-to 999)))))

(defun elbank-overview--insert-accounts ()
  "Insert all accounts informations in the current buffer."
  (seq-do (lambda (group)
	    (elbank-overview--insert-bank (car group))
	    (seq-map #'elbank-overview--insert-account
	    	     (cdr group))
	    (insert "\n"))
	  (seq-group-by (lambda (account)
			  (cadr (split-string (map-elt account 'id) "@")))
			(map-elt elbank-data 'accounts))))

(defun elbank-overview--insert-bank (bankname)
  "Insert BANKNAME into the current buffer as a header."
  (insert (format "%s" bankname))
  (put-text-property (point-at-bol) (point)
		     'face
		     'elbank-subheader-face)
  (insert "\n"))

(defun elbank-overview--insert-account (account)
  "Insert ACCOUNT informations in the current buffer."
  (insert (format "%s" (map-elt account 'label)))
  (make-text-button (point-at-bol) (point)
		    'follow-link t
		    'action (lambda (&rest _)
			      (elbank-list-transactions account)))
  (let* ((balance (format "%s"
			  (map-elt account 'balance)))
	 (fill-width (+ (- (seq-length (elbank--longest-account-label))
			   (current-column))
			(- 25 (seq-length balance)))))
    (dotimes (_ fill-width)
      (insert " "))
    (elbank--insert-amount balance (map-elt account 'currency))
    (insert "\n")))

(defun elbank-overview--insert-saved-reports ()
  "Insert links to saved monthly and yearly reports."
  (seq-doseq (type '(month year))
    (elbank-overview--insert-saved-reports-type type)
    (insert "\n")))

(defun elbank-overview--insert-saved-reports-type (type)
  "Insert links to saved reports of period TYPE."
  (let ((label) (reports) (time))
    (pcase type
      (`month (setq label "Monthly reports")
	      (setq reports elbank-saved-monthly-reports)
	      (setq time (car (last (elbank-transaction-months)))))
      (`year (setq label "Yearly reports")
	     (setq reports elbank-saved-yearly-reports)
	     (setq time (car (last (elbank-transaction-years))))))
    (when reports
      (insert label)
      (put-text-property (point-at-bol) (point)
			 'face 'elbank-subheader-face)
      (insert " ")
      (insert "\n")
      (seq-doseq (report (seq-sort (lambda (a b)
				     (string< (car a) (car b)))
				   reports))
	(insert "- ")
	(let ((beg (point)))
	  (insert (car report))
	  (make-button beg (point)
			    'action
			    (lambda (&rest _)
			      (elbank-report :category (seq-elt report 1)
					     :group-by (seq-elt report 2)
					     :sort-by (seq-elt report 3)
					     :columns (seq-elt report 4)
					     :reverse-sort (seq-elt report 5)
					     :period (list type time))))
	  (insert "\n"))))))


(defun elbank-list-transactions (account)
  "Display the list of transactions for ACCOUNT."
  (elbank-report :account-id (intern (map-elt account 'id))
		 :reverse-sort t))

(defun elbank-transaction-category (transaction)
  "Return the category TRANSACTION belongs to.
If TRANSACTION matches no category, return nil."
  (seq-find #'identity
	    (map-apply (lambda (key category)
			 (when (seq-find
				(lambda (regexp)
				  (string-match-p (downcase regexp)
						  (downcase (map-elt transaction
								     'raw))))
				category)
			   key))
		       elbank-categories)))

(cl-defun elbank-filter-transactions (&key account-id period category)
  "Filter transactions, all keys are optional.

Return transactions in the account with id ACCOUNT-ID for a PERIOD
that belong to CATEGORY.

ACCOUNT-ID is a symbol, PERIOD is a list of the form `(type
time)', CATEGORY is a category string."
  (elbank-filter-transactions-period
   (elbank-filter-transactions-category
    (if account-id
	(map-elt (map-elt elbank-data 'transactions) account-id)
      (elbank-all-transactions))
    category)
   period))

(defun elbank-filter-transactions-category (transactions category)
  "Return the subset of TRANSACTIONS that belong to CATEGORY.

CATEGORY is a string of the form \"cat:subcat:subsubcat\"
representing the path of a category."
  (if category
      (seq-filter (lambda (transaction)
		    (string-prefix-p (downcase category)
				     (downcase (or (elbank-transaction-category
						    transaction)
						   ""))))
		  transactions)
    transactions))

(defun elbank-filter-transactions-period (transactions period)
  "Return the subset of TRANSACTIONS that are within PERIOD.

PERIOD is a list of the form `(type time)', with `type' a
symbol (`month' or `year'), and `time' an encoded time."
  (pcase (car period)
    (`year (elbank--filter-transactions-period-format transactions
						      (cadr period)
						      "%Y"))
    (`month (elbank--filter-transactions-period-format transactions
						       (cadr period)
						       "%Y-%m"))
    (`nil transactions)
    (_ (error "Invalid period type %S" (car period)))))

(defun elbank--filter-transactions-period-format (transactions time format)
  "Return the subset of TRANSACTIONS within TIME.
Comparison is done by formatting periods using FORMAT."
  (seq-filter (lambda (transaction)
		(let ((tr-time (elbank--transaction-time transaction)))
		  (string= (format-time-string format time)
			   (format-time-string format tr-time))))
	      transactions))

(defun elbank--transaction-time (transaction)
  "Return the encoded time for TRANSACTION."
  (apply #'encode-time
	 (seq-map (lambda (el)
		    (or el 0))
		  (parse-time-string (map-elt transaction 'date)))))

(defun elbank-sum-transactions (transactions)
  "Return the sum of all TRANSACTIONS.
TRANSACTIONS are expected to all use the same currency."
  (seq-reduce (lambda (acc transaction)
		(+ acc
		   (string-to-number (map-elt transaction 'amount))))
	      transactions
	      0))

(defun elbank-transaction-years ()
  "Return all years for which there is a transaction."
  (seq-sort #'time-less-p
	    (seq-uniq
	     (seq-map (lambda (transaction)
			(encode-time 0 0 0 1 1 (seq-elt (decode-time
							 (elbank--transaction-time transaction))
							5)))
		      (elbank-all-transactions)))))

(defun elbank-transaction-months ()
  "Return all months for which there is a transaction."
  (seq-sort #'time-less-p
	    (seq-uniq
	     (seq-map (lambda (transaction)
			(let ((time (decode-time
				     (elbank--transaction-time transaction))))
			  (encode-time 0 0 0 1 (seq-elt time 4) (seq-elt time 5))))
		      (elbank-all-transactions)))))

(defun elbank-all-transactions ()
  "Return all transactions for all accounts."
  (seq-remove #'seq-empty-p
	      (apply #'seq-concatenate
		     'vector
		     (map-values (map-elt elbank-data 'transactions)))))

(defun elbank-account (id)
  "Return the account with ID, or nil."
  (unless (stringp id)
    (setq id (symbol-name id)))
  (seq-find (lambda (account)
	      (string= id (map-elt account 'id)))
	    (map-elt elbank-data 'accounts)))

(defvar elbank-transaction-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'elbank-quit)
    map)
  "Keymap for `elbank-transaction-mode'.")

(define-derived-mode elbank-transaction-mode nil "Elbank Transaction"
  "Major mode for viewing a single transaction.

\\{elbank-transaction-mode-map}"
  (read-only-mode))

(defun elbank-show-transaction (transaction)
  "Show the details of TRANSACTION in a separate buffer."
  (let ((buf (get-buffer-create "*Elbank transaction*")))
    (pop-to-buffer buf)
    (elbank-transaction-mode)
    (elbank--transaction-populate transaction)))

(defun elbank--transaction-populate (transaction)
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
	(insert (format "%s" (map-elt transaction key)))
	(insert "\n")))))

(defun elbank--longest-account-label ()
  "Return the longest account label from all accoutns."
  (seq-reduce (lambda (label1 label2)
		(if (> (seq-length label1)
		       (seq-length label2))
		    label1
		  label2))
	      (seq-map (lambda (account)
			 (map-elt account 'label))
		       (map-elt elbank-data 'accounts))
	      ""))

(defun elbank--insert-amount (amount &optional currency)
  "Insert AMOUNT as a float with a precision of 2 decimals.
When CURRENCY is non-nil, append it to the inserted text.
AMOUNT is fontified based on whether it is negative or positive."
  (let ((beg (point))
	(number (if (numberp amount)
		    amount
		  (string-to-number amount))))
    (insert (format "%.2f %s" number (or currency "")))
    (put-text-property beg (point)
		       'face
		       (if (< number 0)
			   'elbank-negative-amount-face
			 'elbank-positive-amount-face))))

(defun elbank--propertize-amount (amount &optional currency)
  "Fontify AMOUNT based on whether it is positive or not.
When CURRENCY is non-nil, append it to the inserted text."
  (with-temp-buffer
    (elbank--insert-amount amount currency)
    (buffer-string)))

(defun elbank-format-period (period)
  "Return the string representation of PERIOD."
  (pcase (car period)
    (`year (format-time-string "Year %Y" (cadr period)))
    (`month (format-time-string "%B %Y" (cadr period)))
    (`nil "")
    (`_ "Invalid period")))

(defun elbank--read-data ()
  "Return an alist of boobank data read from `elbank-data-file'.
Data is cached to `elbank-data'."
  (let ((data (when (file-exists-p (expand-file-name elbank-data-file))
		(json-read-file elbank-data-file))))
    (setq elbank-data data)))

(defun elbank--write-data (data)
  "Write DATA to `elbank-data-file'."
  (make-directory (file-name-directory elbank-data-file) t)
  (with-temp-file elbank-data-file
    (insert (json-encode data))))

(defun elbank--fetch-data ()
  "Update data from boobank."
  (let* ((current (elbank--read-data))
	 (new (elbank--scrap-bookbank-data))
	 (merged (elbank--merge-data current new)))
    (elbank--write-data merged)
    (elbank--read-data)))

(defun elbank--scrap-bookbank-data ()
  "Return all data scraped from boobank."
  (let* ((accounts (elbank--fetch-boobank-accounts))
	 (transactions (seq-map (lambda (account)
				  (list (intern (map-elt account 'id))
					(elbank--fetch-boobank-transactions account)))
				accounts)))
    (message "Elbank: fetching done!")
    `((accounts . ,accounts)
      (transactions . ,(map-apply (lambda (key val)
				    ;; Fetched transactions data is a nested
				    ;; vector, so only keep the first one.
				    (cons key (seq-elt val 0)))
				  transactions)))))

(defun elbank--fetch-boobank-accounts ()
  "Return all accounts in boobank."
  (let ((command (format "%s -f json ls" (elbank--find-boobank-executable))))
    (message "Elbank: fetching accounts...")
    (json-read-from-string (shell-command-to-string command))))

(defun elbank--fetch-boobank-transactions (account)
  "Fetch and return all transactions from ACCOUNT."
  (let* ((since "1970") ;; the current strategy is to always fetch all data.  If
	 ;; needed, this can be optimized later on.
	 (id (map-elt account 'id))
	 ;; Some backends do not support listing transactions, ignore errors
	 (command (format "%s -f json history %s %s 2> /dev/null"
			  (elbank--find-boobank-executable)
			  id
			  since)))
    (message "Elbank: fetching transactions for account %s..." id)
    (json-read-from-string (shell-command-to-string command))))

(defun elbank--merge-data (old new)
  "Merge the dataset from OLD and NEW.

The account list is taken from NEW, so accounts not present in
NEW are deleted.  New transactions for existing accounts are
*only* added, no transaction is removed."
  (if old
      `((accounts . ,(map-elt new 'accounts))
	(transactions . ,(elbank--merge-transactions
			  (map-elt old 'transactions)
			  (map-elt new 'transactions))))
    new))

(defun elbank--merge-transactions (old new)
  "Merge the transaction list from OLD and NEW."
  (map-apply (lambda (id transactions)
	       `(,id . ,(elbank--merge-account-transactions
			 transactions
			 (map-elt new id))))
	     old))

(defun elbank--merge-account-transactions (old new)
  "Merge the transactions from OLD and NEW.
OLD and NEW are lists of transactions for the same account."
  (let ((new-transactions (elbank--new-transactions old new)))
    (seq-concatenate 'vector old new-transactions)))

(defun elbank--new-transactions (old new)
  "Return all transactions not present in OLD bu present in NEW."
  (seq-filter (lambda (trans)
		(> (seq-count (lambda (elt) (equal elt trans)) new)
		   (seq-count (lambda (elt) (equal elt trans)) old)))
	      new))

(defun elbank--find-boobank-executable ()
  "Return the boobank executable.
Signal an error if the boobank executable cannot be found."
  (let ((executable (executable-find elbank-boobank-executable)))
    (unless executable
      (user-error "Cannot find boobank executable (%s) in PATH" elbank-boobank-executable))
    executable))


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

When a PERIOD is provided, append a sum row to the report."
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
    (elbank-report-refresh)))

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

(provide 'elbank)
;;; elbank.el ends here
