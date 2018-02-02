;;; elbank-boobank.el --- Elbank functions for importing from Boobank   -*- lexical-binding: t; -*-

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
(require 'json)
(require 'cl-lib)
(require 'subr-x)

(require 'elbank-common)

;;;###autoload
(defgroup elbank-boobank nil
  "Elbank boobank settings."
  :group 'elbank)

;;;###autoload
(defcustom elbank-boobank-executable "boobank"
  "Boobank executable."
  :type '(file))

(defun elbank-boobank-update ()
  "Update data from boobank."
  (let* ((current elbank-data)
	 (new (elbank-boobank--scrap-data))
	 (merged (elbank--merge-data current new)))
    (elbank-write-data merged)
    (setq elbank-data merged)))

(defun elbank-boobank--scrap-data ()
  "Return all data scraped from boobank."
  (let* ((accounts (elbank--fetch-boobank-accounts))
	 (transactions (apply #'seq-concatenate 'list
			      (seq-map #'elbank-boobank--scrap-transactions
				       accounts))))
    `((accounts . ,accounts)
      (transactions . ,transactions))))

(defun elbank-boobank--scrap-transactions (account)
  "Return a list of transactions for ACCOUNT scraped from boobank."
  (seq-map (lambda (data)
	     (elbank-boobank--make-transaction data account))
	   (elbank--fetch-boobank-transactions account)))

(defun elbank-boobank--make-transaction (data account)
  "Return a transaction alist from DATA with its account value set to ACCOUNT.

If an account already exists with the same id as ACCOUNT, use
that account instead of the new ACCOUNT."
  (let* ((account-to-use (or (elbank-account-with-id (map-elt account 'id))
			     account)))
    ;; Some banks add a category to transactions, which conflicts with elbank's
    ;; categories, so put the category in `bank-category' instead.
    (map-put data 'bank-category (map-elt data 'category))
    (map-put data 'category nil)
    (cons (cons 'account account-to-use) data)))

(defun elbank--fetch-boobank-accounts ()
  "Return all accounts in boobank."
  (let ((command (format "%s -f json ls 2>/dev/null" (elbank--find-boobank-executable))))
    (message "Elbank: fetching accounts...")
    (json-read-from-string (shell-command-to-string command))))

(defun elbank--fetch-boobank-transactions (account)
  "Fetch and return all transactions from ACCOUNT."
  (let* ((since "1970") ; the current strategy is to always fetch all data.  If
			; needed, this can be optimized later on.
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
      `((accounts . ,(elbank--merge-accounts
		      (map-elt old 'accounts)
		      (map-elt new 'accounts)))
	(transactions . ,(elbank--merge-transactions
			  (map-elt old 'transactions)
			  (map-elt new 'transactions))))
    new))

(defun elbank--merge-accounts (old new)
  "Merge the account list from OLD and NEW.
Data from existing accounts in OLD are updated with new data from
NEW."
  (seq-do (lambda (new-acc)
	    (when-let ((acc (seq-find (lambda (acc)
					(equal (map-elt new-acc 'id)
					       (map-elt acc 'id)))
				      old)))
	      (map-apply (lambda (key val)
			   (map-put acc key val))
			 new-acc)))
	  new)
  (let ((new-accounts (seq-remove (lambda (acc)
				    (seq-find (lambda (old-acc)
						(string= (map-elt old-acc 'id)
							 (map-elt acc 'id)))
					      old))
				  new)))
    (seq-concatenate 'list old new-accounts)))

(defun elbank--merge-transactions (old new)
  "Merge the transaction list from OLD and NEW."
  (let ((new-transactions (elbank--new-transactions old new)))
    (seq-concatenate 'list old new-transactions)))

(defun elbank--new-transactions (old new)
  "Return all transactions not present in OLD bu present in NEW.
When comparing transactions, ignore (manually set) categories."
  (apply #'seq-concatenate 'list
	 (seq-map (lambda (trans)
		    (let ((n (- (elbank--count-transactions-like trans new)
				(elbank--count-transactions-like trans old))))
		      (when (> n 0)
			(let ((result))
			  (dotimes (_ n)
			    (setq result (cons trans result)))
			  result))))
		  (seq-uniq new))))

(defun elbank--count-transactions-like (transaction transactions)
  "Return the number of transactions like TRANSACTION in TRANSACTIONS."
  (seq-length (elbank-filter-transactions
	       transactions
	       :raw (map-elt transaction 'raw)
	       :account (map-elt transaction 'account)
	       :amount (map-elt transaction 'amount)
	       :date (map-elt transaction 'date)
	       :vdate (map-elt transaction 'vdate)
	       :rdate (map-elt transaction 'rdate)
	       :label (map-elt transaction 'label))))

(defun elbank--find-boobank-executable ()
  "Return the boobank executable.
Signal an error if the boobank executable cannot be found."
  (let ((executable (executable-find elbank-boobank-executable)))
    (unless executable
      (user-error "Cannot find boobank executable (%s) in PATH" elbank-boobank-executable))
    executable))

(provide 'elbank-boobank)
;;; elbank-boobank.el ends here
