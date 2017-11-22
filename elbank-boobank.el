;;; elbank-boobank.el --- Elbank functions for importing from Boobank   -*- lexical-binding: t; -*-

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
(require 'json)

(defgroup elbank-boobank nil
  "Elbank boobank settings."
  :group 'elbank)

(defcustom elbank-boobank-executable "boobank"
  "Boobank executable."
  :type '(file))

(defun elbank-boobank-update ()
  "Update data from boobank."
  (let* ((current (elbank-read-data))
	 (new (elbank-boobank--scrap-data))
	 (merged (elbank--merge-data current new)))
    (elbank-write-data merged)
    (elbank-read-data)))

(defun elbank-boobank--scrap-data ()
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

(provide 'elbank-boobank)
;;; elbank-boobank.el ends here
