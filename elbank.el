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

;;; Code:

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
