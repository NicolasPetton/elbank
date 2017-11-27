;;; elbank-overview.el --- Elbank overview buffer    -*- lexical-binding: t; -*-

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

(require 'button)
(require 'seq)
(require 'map)
(require 'subr-x)

(require 'elbank-common)
(require 'elbank-boobank)
(require 'elbank-report)
(require 'elbank-budget)

(defvar elbank-overview-buffer-name "*elbank overview*"
  "Name of the elbank overview buffer.")

(defvar elbank-overview-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'elbank-overview-update-buffer)
    (define-key map (kbd "u") #'elbank-overview-update-data)
    (define-key map (kbd "r") #'elbank-report)
    (define-key map (kbd "b") #'elbank-budget-report)
    (define-key map (kbd "n") #'forward-button)
    (define-key map (kbd "p") #'backward-button)
    (define-key map [tab] #'forward-button)
    (define-key map [backtab] #'backward-button)
    (define-key map (kbd "q") #'elbank-quit)
    map)
  "Keymap for `elbank-overview-mode'.")

(define-derived-mode elbank-overview-mode nil "Elbank Overview"
  "Major mode for Elbank overview.

\\{elbank-overview-mode-map}"
  (read-only-mode)
  (setq imenu-prev-index-position-function #'elbank-overview--imenu-prev-index-position-function)
  (setq imenu-extract-index-name-function #'elbank-overview--imenu-extract-index-name-function))

(defun elbank-overview-account-at-point (&optional point)
  "Return account at POINT, nil if none.
If POINT is nil, use current point."
  (get-text-property (or point (point)) 'elbank-account))

(defun elbank-overview--imenu-prev-index-position-function ()
  "Move point to previous account line in current buffer.
This function is used as a value for
`imenu-prev-index-position-function'."
  (ignore-errors
    (backward-button (if (= (point) (point-max))
                         2
                       1))))

(defun elbank-overview--imenu-extract-index-name-function ()
  "Return imenu name for line at point.
This function is used as a value for
`imenu-extract-index-name-function'.  Point should be at the
beginning of an account line."
  (when-let ((account (elbank-overview-account-at-point)))
    (format "%s@%s"
            (elbank-overview-account-group account)
            (map-elt account 'label))))

;;;###autoload
(defun elbank-overview ()
  "Show an overview of all accounts."
  (interactive)
  (elbank-read-data)
  (unless elbank-data
    (when (yes-or-no-p "No data found, import from weboob?")
      (elbank-overview-update-data)))
  (if-let ((buf (get-buffer elbank-overview-buffer-name)))
      (progn
	(switch-to-buffer buf)
	(elbank-overview-update-buffer))
    (let ((buf (get-buffer-create elbank-overview-buffer-name)))
      (pop-to-buffer buf)
      (elbank-overview-mode)
      (elbank-overview-update-buffer))))

;;;###autoload
(defun elbank-overview-update-buffer ()
  "Update the overview buffer with the latest data."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (remove-overlays (point-min) (point-max))
    (insert "Bank accounts overview")
    (put-text-property (point-at-bol) (point)
		       'face 'elbank-header-face)
    (insert "\n\n")
    (elbank-overview--insert-accounts)
    (elbank-overview--insert-hr)
    (insert "\n\n")
    (insert "Custom reports")
    (put-text-property (point-at-bol) (point)
		       'face 'elbank-header-face)
    (insert " ")
    (let ((beg (point)))
      (insert "[Customize]")
      (make-text-button beg (point)
			'follow-link t
			'action (lambda (&rest _)
				  (customize-group 'elbank-report))))
    (insert "\n\n")
    (elbank-overview--insert-saved-reports)
    (insert "\n")
    (insert "Budget report")
    (put-text-property (point-at-bol) (point)
		       'face 'elbank-header-face)
    (insert "\n- ")
    (let ((beg (point)))
      (insert (format "Budget report of %s"
		      (elbank-format-period
		       `(month ,(car (last (elbank-transaction-months)))))))
      (make-text-button beg (point)
			'follow-link t
			'action (lambda (&rest _)
				  (elbank-budget-report))))
    (goto-char (point-min))))

(defun elbank-overview-update-data ()
  "Read new data from boobank and update the buffer."
  (interactive)
  (elbank-boobank-update)
  (elbank-overview-update-buffer))

(defun elbank-overview--insert-hr ()
  "Insert a horizontal rule."
  ;; End line
  (let ((p (1+ (point))))
    (insert "\n\n")
    (put-text-property p (1+ p) 'face '(:underline t :inherit border))
    (overlay-put (make-overlay p (1+ p))
		 'before-string
		 (propertize "\n" 'face '(:underline t)
			     'display '(space :align-to 999)))))

(defun elbank-overview-account-group (account)
  "Return the group into which ACCOUNT is classified."
  (cadr (split-string (map-elt account 'id) "@")))

(defun elbank-overview--insert-accounts ()
  "Insert all accounts informations in the current buffer."
  (seq-do (lambda (group)
	    (elbank-overview--insert-bank (car group))
	    (seq-map #'elbank-overview--insert-account
	    	     (cdr group))
	    (insert "\n"))
	  (seq-group-by #'elbank-overview-account-group (map-elt elbank-data 'accounts))))

(defun elbank-overview--insert-bank (bankname)
  "Insert BANKNAME into the current buffer as a header."
  (insert (format "%s" bankname))
  (put-text-property (point-at-bol) (point)
		     'face
		     'elbank-subheader-face)
  (insert "\n"))

(defun elbank-overview--insert-account (account)
  "Insert ACCOUNT informations in the current buffer."
  (insert (format "%s" (map-elt account 'label)))
  (make-text-button (point-at-bol) (point)
		    'follow-link t
		    'action (lambda (&rest _)
			      (elbank-list-transactions account)))
  (let* ((balance (format "%s"
			  (map-elt account 'balance)))
	 (fill-width (+ (- (seq-length (elbank--longest-account-label))
			   (current-column))
			(- 25 (seq-length balance)))))
    (dotimes (_ fill-width)
      (insert " "))
    (elbank--insert-amount balance (map-elt account 'currency))
    (put-text-property (point-at-bol) (point-at-eol) 'elbank-account account)
    (insert "\n")))

(defun elbank-overview--insert-saved-reports ()
  "Insert links to saved monthly and yearly reports."
  (seq-doseq (type '(month year))
    (elbank-overview--insert-saved-reports-type type)
    (insert "\n")))

(defun elbank-overview--insert-saved-reports-type (type)
  "Insert links to saved reports of period TYPE."
  (let ((label) (reports) (time))
    (pcase type
      (`month (setq label "Monthly reports")
	      (setq reports elbank-saved-monthly-reports)
	      (setq time (car (last (elbank-transaction-months)))))
      (`year (setq label "Yearly reports")
	     (setq reports elbank-saved-yearly-reports)
	     (setq time (car (last (elbank-transaction-years))))))
    (when reports
      (insert label)
      (put-text-property (point-at-bol) (point)
			 'face 'elbank-subheader-face)
      (insert " ")
      (insert "\n")
      (seq-doseq (report (seq-sort (lambda (a b)
				     (string< (car a) (car b)))
				   reports))
	(insert "- ")
	(let ((beg (point)))
	  (insert (car report))
	  (make-button beg (point)
		       'action
		       (lambda (&rest _)
			 (elbank-report :category (seq-elt report 1)
					:group-by (seq-elt report 2)
					:sort-by (seq-elt report 3)
					:columns (seq-elt report 4)
					:reverse-sort (seq-elt report 5)
					:period (list type time))))
	  (insert "\n"))))))

(provide 'elbank-overview)
;;; elbank-overview.el ends here
