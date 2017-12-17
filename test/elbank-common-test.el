;;; elbank-common-test.el --- Unit tests for elbank-common.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Nicolas Petton

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
      (expect (elbank-transaction-elt tr 'category) :to-equal "Income"))))

(provide 'elbank-common-test)
;;; elbank-common-test.el ends here
