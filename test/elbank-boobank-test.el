;;; elbank-boobank-test.el --- Tests for elbank-boobank.el

;; Copyright (C) 2017-2018 Nicolas Petton

;; Author: Nicolas Petton <nicolas@petton.fr>

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

;; Tests for elbank-boobank.el

;;; Code:

(require 'buttercup)
(require 'map)
(require 'seq)
(require 'cl-lib)
(require 'elbank-boobank)

(describe "Merging data"
  (it "should append new transactions and keep old ones"
    (let* ((old `((accounts . (#1=((id . "account1") (label . "account 1"))))
  		  (transactions . (((label . "1") (account . #1#))
  				   ((label . "2") (account . #1#))
  				   ((label . "3") (account . #1#))))))
  	   (new `((accounts . (#1=((id . "account1") (label . "account 1"))))
  		  (transactions . (((label . "4") (account . #1#))
  				   ((label . "5") (account . #1#))))))
  	   (merged (elbank-boobank--merge-data old new)))
      (expect (seq-map (lambda (tr)
			 (elbank-transaction-elt tr 'label))
		       (map-elt merged 'transactions))
	      :to-equal '("1" "2" "3" "4" "5"))))

  (it "should keep existing accounts when merging accounts"
    (let* ((old '(((id . "1"))
		  ((id . "2"))))
	   (new '(((id . "1"))
		  ((id . "3"))))
	   (merged (elbank-boobank--merge-accounts old new)))
      (message "%s" merged)
      (expect (seq-length merged) :to-be 3)
      (expect (car merged) :to-be (car old))
      (expect (cadr merged) :to-be (cadr old))
      (expect (cl-caddr merged) :to-be (cadr new))))

  (it "should not duplicate accounts when there are new fields"
    (let* ((old '(((id . "1") (foo "old"))))
	   (new '(((id . "1") (foo "old") (bar "new"))))
	   (merged (elbank-boobank--merge-accounts old new)))
      (expect (seq-length merged) :to-be 1)
      (expect (car merged) :to-be (car old))))

  (it "merging accounts should update current accounts values"
    (let* ((old '(((id . "1") (balance . "3000"))))
	   (new '(((id . "1") (balance . "3500"))))
	   (merged (elbank-boobank--merge-accounts old new)))
      (expect (seq-length merged) :to-be 1)
      (expect (map-elt (car merged) 'balance) :to-equal "3500")))

  (it "should deduplicate new transactions"
    (let* ((old `((transactions . (((label . "1"))
  				   ((label . "2"))
  				   ((label . "3"))
				   ((label . "3"))))))
  	   (new `((transactions . (((label . "2"))
				   ((label . "3"))
				   ((label . "3"))
				   ((label . "3"))
				   ((label . "3"))))))
  	   (merged (elbank-boobank--merge-data old new)))
      (expect (seq-map (lambda (tr)
			 (elbank-transaction-elt tr 'label))
		       (map-elt merged 'transactions))
	      :to-equal
	      '("1" "2" "3" "3" "3" "3"))))

  (it "should ignore categories when deduplicating"
    (let* ((old `((accounts . (((id . "account1") (label . "account 1"))))
  		  (transactions . (((label . "1"))
  				   ((label . "2"))
  				   ((label . "3") (category . "foo"))
				   ((label . "3") (category . "bar"))))))
  	   (new `((accounts . (((id . "account1") (label . "account 1"))))
  		  (transactions . (((label . "2"))
				   ((label . "3"))
				   ((label . "3"))
				   ((label . "3"))
				   ((label . "3"))))))
  	   (merged (elbank-boobank--merge-data old new)))
      (expect (seq-map (lambda (tr)
			 (elbank-transaction-elt tr 'label))
		       (map-elt merged 'transactions))
	      :to-equal
	      '("1" "2" "3" "3" "3" "3"))))

  (it "should ignore alist elements order deduplicating"
    (let* ((old `((transactions . (((label . "1") (amount . "10"))
  				   ((label . "2") (amount . "20"))
  				   ((amount . "15") (label . "3"))
				   ((label . "3"))))))
  	   (new `((transactions . (((amount . "20") (label . "2"))
				   ((amount . "15") (label . "3"))
				   ((label . "3"))
				   ((label . "3"))))))
  	   (merged (elbank-boobank--merge-data old new)))
      (expect (seq-map (lambda (tr)
			 (elbank-transaction-elt tr 'label))
		       (map-elt merged 'transactions))
	      :to-equal
	      '("1" "2" "3" "3" "3"))))

  (it "should use existing accounts when making new transactions"
    (let* ((account '((id . "account1") (label . "account 1")))
	   (new-account '((id . "account1") (label . "account 1")))
	   (elbank-data `((accounts . (,account))))
	   (transaction (elbank-boobank--make-transaction `((amount . "3000"))
							  new-account)))
      (expect (elbank-transaction-elt transaction 'account) :to-be account))))

(provide 'elbank-boobank-test)
;;; elbank-boobank-test.el ends here
