;;; elbank-migration.el --- Handle data structure migrations  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Nicolas Petton

;; Author: Nicolas Petton <nicolas@petton.fr>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'elbank-common)
(require 'elbank-boobank)
(require 'map)

(defun elbank-migration-to-2-0-run ()
  "Migrate from alist data to structs."
  (elbank-read-data)
  (let* ((accounts (map-elt elbank-data 'accounts))
	 (transactions (map-elt elbank-data 'transactions)))
    ;; Need to migrate accounts from alists to structs
    (when (listp (car accounts))
      (let ((migrated-accounts (seq-map #'elbank-boobank--create-account
					accounts)))
	(map-put elbank-data 'accounts migrated-accounts)
	;; Make all transactions point to the migrated accounts
	(seq-doseq (transaction transactions)
	  (let* ((account-id (map-nested-elt transaction '(account id)))
		 (account (seq-find (lambda (account)
				     (string= (elbank-account-id account)
					      account-id))
				    migrated-accounts)))
	    (map-put transaction 'account account)))
	(elbank-write-data)))))

(provide 'elbank-migration)
;;; elbank-migration.el ends here
