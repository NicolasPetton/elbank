;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(When "^I make a full report$"
  (lambda ()
    (switch-to-buffer (elbank-report))))

(When "^I make a monthly report$"
  (lambda ()
    (switch-to-buffer (elbank-report
		       :period `(month ,@(last (elbank-transaction-months)))))))

(When "^I make a monthly report with \"\\(.+\\)\"$"
  (lambda (options)
    (switch-to-buffer (apply #'elbank-report
			     :period `(month ,@(last (elbank-transaction-months)))
			     (read (format "(%s)" options))))))

(Given "^I have \"\\(.+\\)\"$"
  (lambda (options)
))

(When "^I have \"\\(.+\\)\"$"
  (lambda (something)
    ;; ...
    ))

(Then "^I should have \"\\(.+\\)\"$"
  (lambda (something)
    ;; ...
    ))

(And "^I have \"\\(.+\\)\"$"
  (lambda (something)
    ;; ...
    ))

(But "^I should not have \"\\(.+\\)\"$"
  (lambda (something)
    ;; ...
    ))
