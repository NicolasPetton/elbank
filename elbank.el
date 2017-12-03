;;; elbank.el --- Personal finances reporting application  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Nicolas Petton

;; Author: Nicolas Petton <nicolas@petton.fr>
;; Version: 0.1
;; Keywords: tools, personal-finances
;; Package-Requires: ((emacs "25") (seq "2.16"))

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

;; Elbank is a personal finances reporting (and soon budgeting) application.
;; It uses Weboob (https://weboob.org) for scraping data.
;;
;; Data is stored as JSON in `elbank-data-file' which defaults to
;; `$HOME/.emacs.d/elbank-data.json'.
;;
;; Transactions are automatically categorized with `elbank-categories', an
;; association list of the form:
;;
;;   '(("category1" . ("regexp1" "regexp2"))
;;     (("category2" . ("regexp")))
;;
;; TODO:
;; - Add unit tests
;; - Implement budgeting

;;; Code:


;; HACK: In Emacs 25.1, an older version of seq.el is provided, which can be
;; loaded before indium or even package.el.  If this happens, the feature `seq'
;; being already provided, the correct version of seq.el won't get loaded.
(require 'seq)
(if (fboundp 'seq-map-indexed)
    (defalias 'elbank-seq-map-indexed #'seq-map-indexed)
  (defun elbank-seq-map-indexed (function sequence)
    "Return the result of applying FUNCTION to each element of SEQUENCE.
Unlike `seq-map', FUNCTION takes two arguments: the element of
the sequence, and its index within the sequence."
    (let ((index 0))
      (seq-map (lambda (elt)
                 (prog1
                     (funcall function elt index)
                   (setq index (1+ index))))
               sequence))))

(require 'button)
(require 'tabulated-list)
(eval-and-compile (require 'cl-lib))

(require 'elbank-overview)
(require 'elbank-transaction)
(require 'elbank-report)
(require 'elbank-budget)
(require 'elbank-boobank)

(provide 'elbank)
;;; elbank.el ends here
