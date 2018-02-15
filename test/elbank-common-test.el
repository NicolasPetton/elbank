;;; elbank-common-test.el --- Unit tests for elbank-common.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2018  Nicolas Petton

;; Author: Nicolas Petton <nicolas@petton.fr>
;; Keywords: test

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

;; Tests for elbank-common.el

;;; Code:

(require 'buttercup)
(require 'map)
(require 'elbank-common)

(describe "Transaction elements"
  (it "should access transactions element"
    (let ((tr '((amount . "2000")
		(category . "Expenses"))))
      (expect (elbank-transaction-elt tr 'amount) :to-equal "2000")
      (expect (elbank-transaction-elt tr 'category) :to-equal "Expenses")))

  (it "should set transactions element"
    (let ((tr '((amount . "2000")
		(category . "Expenses"))))
      (setf (elbank-transaction-elt tr 'amount) "-1500")
      (setf (elbank-transaction-elt tr 'category) "Income")
      (expect (elbank-transaction-elt tr 'amount) :to-equal "-1500")
      (expect (elbank-transaction-elt tr 'category) :to-equal "Income")))

  (it "should mutate transactions in place when adding new keys"
    (let* ((original '((amount . "2000")))
	   (tr original))
      (setf (elbank-transaction-elt tr 'category) "Income")
      (expect (elbank-transaction-elt tr 'category) :to-equal "Income")
      (expect original :to-be tr)))

  (it "split transactions should have no category"
    (let ((tr '((category . (("foo" . 20) ("bar" . 10))))))
      (expect (elbank-transaction-elt tr 'category) :to-be nil)
      (expect (elbank-transaction-in-category-p tr "foo") :to-be nil)
      (expect (elbank-transaction-in-category-p tr "bar") :to-be nil)))

  (it "transactions with multiple categories should be split."
    (let ((tr1 '((category . (("foo" . 20) ("bar" . 10)))))
	  (tr2 '((category . "foo"))))
      (expect (elbank-transaction-split-p tr1) :to-be-truthy)
      (expect (elbank-transaction-split-p tr2) :to-be nil)))

  (describe "when asked for label"
    (it "returns label if present"
      (let ((tr '((label . "foo"))))
        (expect (elbank-transaction-elt tr 'label) :to-equal "foo")))

    (it "prefers label over raw"
      (let ((tr '((label . "foo") (raw . "bar"))))
        (expect (elbank-transaction-elt tr 'label) :to-equal "foo")))

    (it "returns raw if present and label is not"
      (let ((tr '((raw . "foo"))))
        (expect (elbank-transaction-elt tr 'label) :to-equal "foo")))

    (it "returns default if neither label nor raw are present"
      (let ((tr '((something-else . "bar"))))
        (expect (elbank-transaction-elt tr 'label "foo") :to-equal "foo")))))

(provide 'elbank-common-test)
;;; elbank-common-test.el ends here
