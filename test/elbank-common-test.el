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

(describe "Setting categories"
  (it "Should set the category"
    (let ((tr '((category . nil) (amount . "3000"))))
      (setf (elbank-transaction-category tr) "foo")
      (expect (elbank-transaction-category tr) :to-equal "foo"))))

(provide 'elbank-common-test)
;;; elbank-common-test.el ends here
