(require 'f)
(require 'seq)
(require 'subr-x)

(defvar elbank-support-path
  (f-dirname load-file-name))

(defvar elbank-features-path
  (f-parent elbank-support-path))

(defvar elbank-root-path
  (f-parent elbank-features-path))

(add-to-list 'load-path elbank-root-path)

;; Ensure that we don't load old byte-compiled versions
(let ((load-prefer-newer t))
  (require 'elbank)
  (require 'espuds)
  (require 'ert))

(Setup
 ;; Before anything has run
 (let ((accounts `[,(elbank-account-create :id "1234@fakebank"
					   :label "Fake account 1"
					   :currency "EUR"
					   :iban "1234"
					   :type 1
					   :balance "400")
		   ,(elbank-account-create :id "1235@fakebank"
					   :label "Fake account 2"
					   :currency "EUR"
					   :iban "1235"
					   :type 1
					   :balance "50")]))
   (setq real-elbank-data elbank-data)
   (setq elbank-data
	 `((accounts . [#1=,(seq-elt accounts 0)
			   #2=,(seq-elt accounts 1)])
	   (transactions . (((id . "@fakebank")
			     (date . "2017-11-24")
			     (rdate . "2017-11-24")
			     (type . 1)
			     (raw . "CB Supermarket 1")
			     (label . "CB Supermarket 1")
			     (amount . "-124.00")
			     (account . #1#))
			    ((id . "@fakebank")
			     (date . "2017-11-20")
			     (rdate . "2017-11-20")
			     (type . 1)
			     (label . "Paycheck")
			     (raw . "Transfer company XX paycheck")
			     (amount . "2300.00")
			     (account . #1#))
			    ((id . "@fakebank")
			     (date . "2017-11-20")
			     (rdate . "2017-11-20")
			     (type . 1)
			     (raw . "Bakery xxx")
			     (label . "Bakery xxx")
			     (amount . "-4.25")
			     (account . #1#))
			    ((id . "@fakebank")
			     (date . "2017-11-18")
			     (rdate . "2017-11-18")
			     (type . 1)
			     (raw . "CB Restaurant")
			     (label . "CB Restaurant")
			     (amount . "-31.00")
			     (account . #1#))
			    ((id . "@fakebank")
			     (date . "2017-11-01")
			     (rdate . "2017-11-01")
			     (type . 1)
			     (label . "Rent")
			     (raw . "Rent November 2017")
			     (amount . "-450.00")
			     (account . #1#)))))))

 (setq real-elbank-categories elbank-categories)
 (setq elbank-categories '(("Expenses:Food" . ("supermarket"
					       "restaurant"
					       "Local store"
					       "Bakery"))
			   ("Expenses:Rent" . ("rent"))
			   ("Income:Salary" . ("company xx"))))

 (setq real-elbank-budget elbank-budget)
 (setq elbank-budget '(("Expenses:Food" . 300)
		       ("Expenses:Rent" . 450))))

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 (setq elbank-data real-elbank-data)
 (setq elbank-categories real-elbank-categories)
 (setq elbank-budget real-elbank-budget)
 (when-let ((buf (get-buffer "*elbank-report*")))
   (kill-buffer buf)))
