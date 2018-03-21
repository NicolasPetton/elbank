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

(defun elbank-boobank-update (&optional callback)
  "Update `elbank-data' from boobank and save it on file.
When CALLBACK is non-nil, evaluate it when data is updated."
  (elbank-boobank--update-accounts
   (lambda ()
     (elbank-boobank--update-transactions
      (lambda ()
	(elbank-write-data)
	(when callback (funcall callback)))))))

(defun elbank-boobank--update-accounts (callback)
  "Update the accounts data from boobank.
Evaluate CALLBACK when data is updated."
  (elbank-boobank--fetch-then-merge
   #'elbank-boobank--fetch-accounts
   #'elbank-boobank--merge-accounts
   (lambda (accounts)
     (map-put elbank-data 'accounts accounts)
     (funcall callback))))

(defun elbank-boobank--update-transactions (callback)
  "Update the transactions data from boobank.
Evaluate CALLBACK when data is updated."
  (elbank-boobank--fetch-then-merge
   #'elbank-boobank--fetch-transactions
   #'elbank-boobank--merge-transactions
   (lambda (transactions)
     (map-put elbank-data 'transactions transactions)
     (funcall callback))))

(defun elbank-boobank--fetch-accounts (callback)
  "Execute CALLBACK with all fetched accounts from boobank."
  (let ((command (format "%s -f json ls 2>/dev/null" (elbank-boobank--find-executable))))
    (message "Elbank: fetching accounts...")
    (elbank-boobank--shell-command command callback)))

(defun elbank-boobank--fetch-transactions (callback &optional accounts acc)
  "Fetch all transactions from all ACCOUNTS and evaluate CALLBACK.
If ACCOUNTS is nil, use all accounts from `elbank-data'.

CALLBACK is called with all fetched transactions.

ACC is used in recursive calls to accumulate fetched transactions."
  (let* ((since "1970")	 ; the current strategy is to always fetch all data.  If
			 ; needed, this can be optimized later on.
	 (accounts (or accounts (map-elt elbank-data 'accounts)))
	 (account (car accounts))
	 (id (elbank-account-id account))
	 ;; The backend might not support listing transactions for some
	 ;; accounts, ignore errors.
	 (command (format "%s -f json history %s %s 2> /dev/null"
			  (elbank-boobank--find-executable)
			  id
			  since)))
    (message "Elbank: fetching transactions for account %s..." id)
    (elbank-boobank--shell-command
     command
     (lambda (data)
       (let* ((transactions (seq-map (lambda (datum)
				       (elbank-boobank--make-transaction datum account))
				     data))
	      (all (seq-concatenate 'list acc transactions)))
	 (if (cdr accounts)
	     (elbank-boobank--fetch-transactions callback (cdr accounts) all)
	   (funcall callback all)))))))

(defun elbank-boobank--make-transaction (data account)
  "Return a transaction alist from DATA with its account set to ACCOUNT."
  (unless (seq-contains (map-elt elbank-data 'accounts)
			account
			#'eq)
    (error "Account %s not in the Elbank database" account))
  (let ((transaction (map-copy data)))
    ;; Some banks add a category to transactions, which conflicts with elbank's
    ;; categories, so put the category in `bank-category' instead.
    (map-put transaction 'bank-category (map-elt data 'category))
    (map-put transaction 'category nil)
    (map-put transaction 'account account)))

(defun elbank-boobank--merge-accounts (accounts-data)
  "Merge ACCOUNTS-DATA with existing accounts in `elbank-data'.
Existing accounts are updated with new data from ACCOUNTS-DATA,
never replaced."
  (elbank-boobank--update-existing-accounts accounts-data)
  (let ((existing-accounts (map-elt elbank-data 'accounts)))
    (let ((new-accounts (elbank-boobank--find-new-accounts accounts-data)))
      (seq-concatenate 'list existing-accounts new-accounts))))

(defun elbank-boobank--find-new-accounts (accounts-data)
  "Return accounts in ACCOUNTS-DATA that are not present in `elbank-data'."
  (let ((new-accounts-data (seq-remove (lambda (account-data)
					 (elbank-account-with-id
					  (map-elt account-data 'id)))
				       accounts-data)))
    (seq-map #'elbank-boobank--create-account
	     new-accounts-data)))

(defun elbank-boobank--create-account (data)
  "Return an account struct filled with DATA."
  (let ((account (elbank-account-create :id (map-elt data 'id))))
    (elbank-boobank--update-account account data)
    account))

(defun elbank-boobank--update-existing-accounts (accounts-data)
  "Update existing accounts in `elbank-data' with the data from ACCOUNTS-DATA'.
No new account is created, only existing account values are updated."
  (seq-do (lambda (account-data)
	    (when-let ((account (elbank-account-with-id (map-elt account-data 'id))))
	      (elbank-boobank--update-account
	       account
	       account-data)))
	  accounts-data))

(defun elbank-boobank--update-account (account data)
  "Update ACCOUNT with new DATA from boobank."
  (setf (elbank-account-url account) (map-elt data 'url))
  (setf (elbank-account-label account) (map-elt data 'label))
  (setf (elbank-account-currency account) (map-elt data 'currency))
  (setf (elbank-account-iban account) (map-elt data 'iban))
  (setf (elbank-account-bank-name account) (map-elt data 'bank-name))
  (setf (elbank-account-type account) (map-elt data 'type))
  (setf (elbank-account-balance account) (map-elt data 'balance)))

(defun elbank-boobank--merge-transactions (transactions)
  "Merge the transaction list from `elbank-data' and TRANSACTIONS."
  (let* ((existing-transactions (map-elt elbank-data 'transactions))
	 (new-transactions (elbank-boobank--find-new-transactions transactions)))
    (seq-concatenate 'list existing-transactions new-transactions)))

(defun elbank-boobank--find-new-transactions (transactions)
  "Return all transactions from TRANSACTIONS not present in `elbank-data'.
When comparing transactions, ignore (manually set) categories."
  (apply #'seq-concatenate 'list
	 (seq-map (lambda (trans)
		    (let ((n (- (elbank-boobank--count-transactions-like
				 trans transactions)
				(elbank-boobank--count-transactions-like
				 trans (elbank-all-transactions t)))))
		      (when (> n 0)
			(let ((result))
			  (dotimes (_ n)
			    (setq result (cons trans result)))
			  result))))
		  (seq-uniq transactions))))

(defun elbank-boobank--count-transactions-like (transaction transactions)
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

(defun elbank-boobank--shell-command (command callback)
  "Start a subprocess for COMMAND, and evaluate CALLBACK with its output."
  (let ((bufname "*boobank process*"))
    (when-let ((buf (get-buffer bufname)))
      (with-current-buffer buf
	(erase-buffer)))
    (make-process :name "boobank"
		  :buffer bufname
		  :sentinel (lambda (process event)
			      (if (eq (process-status process) 'exit)
				  (let ((json-array-type 'list))
				    (with-current-buffer (process-buffer process)
				      (goto-char (point-min))
				      (funcall callback (json-read))))
				(error "Boobank fetch failed! %s" event)))
		  :command (list shell-file-name
				 shell-command-switch
				 command))))

(defun elbank-boobank--find-executable ()
  "Return the boobank executable.
Signal an error if the boobank executable cannot be found."
  (let ((executable (executable-find elbank-boobank-executable)))
    (unless executable
      (user-error "Cannot find boobank executable (%s) in PATH" elbank-boobank-executable))
    executable))

(defun elbank-boobank--fetch-then-merge (fetch-fn merge-fn callback)
  "Evaluate MERGE-FN with the result of the evaluation of FETCH-FN.

FETCH-FN is an asynchronous function that take a callback
function as argument.

Evaluate CALLBACK with the result of the merge."
  (funcall fetch-fn
	   (lambda (data)
	     (let ((merged (funcall merge-fn data)))
	       (funcall callback merged)))))



(provide 'elbank-boobank)
;;; elbank-boobank.el ends here
