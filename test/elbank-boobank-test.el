;;; elbank-boobank-test.el --- Tests for elbank-boobank.el

;; Copyright (C) 2017 Nicolas Petton

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
(require 'elbank-boobank)

(describe "Merging data"
  (it "should keep new accounts"
    (let* ((old '((accounts . [((id . "account1"))
			       ((id . "account2"))])))
	   (new '((accounts . [((id . "account1"))
			       ((id . "account3"))])))
	   (merged (elbank--merge-data old new)))
      (expect (map-elt merged 'accounts) :to-equal
	      [((id . "account1"))
	       ((id . "account3"))])))

  (it "should append new transactions and keep old ones"
    (let* ((old `((accounts . [((id . "account1") (label . "account 1"))])
  		  (transactions (account1 . [((label "1"))
  					     ((label "2"))
  					     ((label "3"))]))))
  	   (new `((accounts . [((id . "account1") (label . "account 1"))])
  		  (transactions (account1 . [((label "4"))
  					     ((label "5"))]))))
  	   (merged (elbank--merge-data old new)))
      (expect (map-elt merged 'transactions) :to-equal
	      '((account1 . [((label "1"))
			     ((label "2"))
			     ((label "3"))
			     ((label "4"))
			     ((label "5"))])))))

  (it "should deduplicate new transactions"
    (let* ((old `((accounts . [((id . "account1") (label . "account 1"))])
  		  (transactions (account1 . [((label "1"))
  					     ((label "2"))
  					     ((label "3"))
					     ((label "3"))]))))
  	   (new `((accounts . [((id . "account1") (label . "account 1"))])
  		  (transactions (account1 . [((label "2"))
					     ((label "3"))
					     ((label "3"))
					     ((label "3"))
					     ((label "3"))]))))
  	   (merged (elbank--merge-data old new)))
      (expect (map-elt merged 'transactions) :to-equal
	      '((account1 . [((label "1"))
			     ((label "2"))
			     ((label "3"))
			     ((label "3"))
			     ((label "3"))
			     ((label "3"))])))))

   (it "should ignore categories when deduplicating"
    (let* ((old `((accounts . [((id . "account1") (label . "account 1"))])
  		  (transactions (account1 . [((label "1"))
  					     ((label "2"))
  					     ((label "3") (category . "foo"))
					     ((label "3") (category . "bar"))]))))
  	   (new `((accounts . [((id . "account1") (label . "account 1"))])
  		  (transactions (account1 . [((label "2"))
					     ((label "3"))
					     ((label "3"))
					     ((label "3"))
					     ((label "3"))]))))
  	   (merged (elbank--merge-data old new)))
      (expect (map-elt merged 'transactions) :to-equal
	      '((account1 . [((label "1"))
			     ((label "2"))
			     ((label "3") (category . "foo"))
			     ((label "3") (category . "bar"))
			     ((label "3"))
			     ((label "3"))]))))))

(provide 'elbank-boobank-test)
;;; elbank-boobank-test.el ends here
