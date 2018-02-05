;;; elbank-report-test.el --- Unit tests for elbank-report.el  -*- lexical-binding: t; -*-

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

;; Tests for elbank-report.el

;;; Code:

(require 'buttercup)
(require 'elbank-report)

(describe "Splitting transaction"
  (spy-on 'elbank-report-set-category)
  (spy-on 'elbank-report--split-amount :and-return-value '(("foo" . "1500") ("bar" . "500")))

  (describe "should split a non-splitted transaction"
    (it "with a category"
      (let* ((transaction '((amount . "2000") (category . "Expenses"))))
        (elbank-report-split-transaction transaction)
        (expect 'elbank-report-set-category
                :to-have-been-called-with '(("foo" . "1500") ("bar" . "500")) transaction)))

    (it "with no category"
      (let* ((transaction '((amount . "2000"))))
        (elbank-report-split-transaction transaction)
        (expect 'elbank-report-set-category
                :to-have-been-called-with '(("foo" . "1500") ("bar" . "500")) transaction))))

  (describe "should split a sub-transaction"
    (it "with different categories"
      (let* ((main-transaction '((amount . "3000")
                                 (category . (("cat1" . "1000") ("cat2" . "2000")))))
             (sub-transaction `((amount . "2000")
                                (category . "cat2")
                                (split-from . ,main-transaction))))
        (elbank-report-split-transaction sub-transaction)
        (expect 'elbank-report-set-category
                :to-have-been-called-with '(("cat1" . "1000")
                                            ("foo" . "1500")
                                            ("bar" . "500")) main-transaction))))

  (it "with 2 identical sub-transactions"
    (let* ((main-transaction '((amount . "4000")
                               (category . (("cat1" . "2000") ("cat1" . "2000")))))
           (sub-transaction `((amount . "2000")
                              (category . "cat1")
                              (split-from . ,main-transaction))))
      (elbank-report-split-transaction sub-transaction)
      (expect 'elbank-report-set-category
              :to-have-been-called-with '(("cat1" . "2000")
                                          ("foo" . "1500")
                                          ("bar" . "500")) main-transaction))))

(provide 'elbank-report-test)
;;; elbank-report-test.el ends here
