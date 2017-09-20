(cl:in-package :cl-user)

(defpackage :cambl-test
  (:use :cl :cambl :xlunit)
  (:export #:cambl-test-suite
	   #:commodity-test-case
	   #:amount-test-case
           #:run-tests
           #:latest-test-results))

(in-package :cambl-test)

(defclass commodity-test-case (test-case)
  ()
  (:documentation "test-case for CAMBL commodities"))

(defmacro assert-valid (object)
  `(assert ,object))
(defmacro assert-value-equal (left right)
  `(assert (value-equal ,left ,right)))
(defmacro assert-value-equalp (left right)
  `(assert (value= ,left ,right)))
(defmacro assert-value-not-equal (left right)
  `(assert (value-not-equal ,left ,right)))
(defmacro assert-value-not-equalp (left right)
  `(assert (value/= ,left ,right)))

(def-test-method test-price-history ((test commodity-test-case) :run nil)
  ;; ptime jan17_07    = parse_datetime("2007/01/17 00:00:00");
  ;; ptime feb27_07    = parse_datetime("2007/02/27 18:00:00");
  ;; ptime feb28_07    = parse_datetime("2007/02/28 06:00:00");
  ;; ptime feb28_07sbm = parse_datetime("2007/02/28 11:59:59");
  ;; ptime mar01_07    = parse_datetime("2007/03/01 00:00:00");
  ;; ptime apr15_07    = parse_datetime("2007/04/15 13:00:00");

  ;; // jww (2007-04-17): tbd
  ;; amount_t x0;
  ;; amount_t x1("100.10 AAPL");

  ;; (assert-condition x0.value() amount_error)
  ;; (assert-false x1.value())

  ;; // Commodities cannot be constructed by themselves, since a great
  ;; // deal of their state depends on how they were seen to be used.
  ;; commodity_t& aapl(x1.commodity());

  ;; aapl.add_price(jan17_07 amount_t("$10.20"));
  ;; aapl.add_price(feb27_07 amount_t("$13.40"));
  ;; aapl.add_price(feb28_07 amount_t("$18.33"));
  ;; aapl.add_price(feb28_07sbm amount_t("$18.30"));
  ;; aapl.add_price(mar01_07 amount_t("$19.50"));
  ;; aapl.add_price(apr15_07 amount_t("$21.22"));

  ;; optional<amount_t> amt1 = x1.value(feb28_07sbm);
  ;; (assert-true amt1)
  ;; (assert-value-equal amount_t("$1831.83") *amt1)

  ;; optional<amount_t> amt2 = x1.value(now);
  ;; (assert-true amt2)
  ;; (assert-value-equal amount_t("$2124.12") *amt2)

  ;; (assert-valid x1)
  )

(def-test-method test-lots ((test commodity-test-case) :run nil)
  ;; // jww (2007-04-17): tbd
  )

(def-test-method test-scaling-base ((test commodity-test-case) :run nil)
  ;; // jww (2007-04-17): tbd
  )

(def-test-method test-reduction ((test commodity-test-case) :run nil)
  ;; // jww (2007-04-17): tbd
  )

(defclass amount-test-case (test-case)
  ()
  (:documentation "test-case for CAMBL amounts"))

(defmethod set-up ((test amount-test-case))
  ;; Cause the display precision for dollars to be initialized to 2.
  (reset-commodity-pool)
  (amount "$1,000.00"))

(defmacro define-test (name &rest body-forms)
  `(def-test-method ,name ((test amount-test-case) :run nil)
     ,@body-forms))

(define-test amount/uncommoditized
  (assert-equal "0" (format-value (amount "0")))
  (assert-equal "0.10" (format-value (amount "0.10")))
  (assert-equal "0.10" (format-value (amount ".10")))
  (assert-equal "12.1000000000000" (format-value (amount "12.1000000000000")))
  (assert-equal "12.10000" (format-value (amount* "12.10000"))))

(define-test amount/commoditized
  (assert-equal "$0.00" (format-value (amount "$0")))
  (assert-equal "$0.10" (format-value (amount "$0.10")))
  (assert-equal "$12.10" (format-value (amount* "$12.1000000000000")))
  (assert-equal "$12.10" (format-value (amount* "$12.10000")))
  (assert-equal "$ 12.10" (format-value (amount "$ 12.10")))
  (assert-equal "$12.10" (format-value (amount "$12.10")))

  (assert-equal "DX 12.10001" (format-value (amount "DX 12.10001")))
  (assert-equal "DX 12.10000" (format-value (amount* "DX 12.1")))

  (assert-equal "12x" (format-value (amount "12x")))
  ;; jww (2007-10-25): This should be an error instead
  (assert-equal "12x" (format-value (amount "12x."))) ; ignore bogus chars

  (assert-condition 'end-of-file (amount "EUR"))
  ;; jww (2007-10-25): This should give a much better error
  ;;(assert-condition 'no-integer-present (amount "."))

  (let ((x12 (amount* "$100")))
    (assert-equal 2 (display-precision x12))
    (assert-equal 2 (display-precision (amount-commodity x12)))

    (with-input-from-string (in "$100...")
      (let ((x13 (read-amount* in)))
	(assert-value-equal x12 x13)
	(assert-equal 2 (display-precision x13))))

    (assert-equal "$2.000,00"
                  (format-value (amount* "$2000")))
    (assert-equal "0" (format-value (amount "0")))
    (assert-equal "$0,00" (format-value (amount "$0")))
    (assert-equal "$2.000,00"
                  (format-value (amount* "$2,000.00")))
    (assert-equal "$2.000,00"
                  (format-value (amount* "$2.000,00")))

    (let ((x15 (amount* "$2000"))
	  (x16 (amount* "$2,000")))
      (assert-equal "$2,000.00" (format-value x15))
      (assert-equal "$2,000.00" (format-value x16))
      (assert-value-equal x15 x16))

    (assert-equal "EUR 100" (format-value (amount "EUR 100")))

    (let ((x1 (amount* "$100.0000")))
      (assert-eql 2 (display-precision x12))
      (assert-eql (amount-commodity x1) (amount-commodity x12))
      (assert-value-equal x1 x12))

    (let ((x0 (amount "$100.0000")))
      (assert-eql 4 (display-precision x12)) ; should have changed now
      (assert-eql (amount-commodity x0) (amount-commodity x12))
      (assert-value-equal x0 x12))

    (let ((x2 (amount "$100.00")))
      (assert-value-equal x2 x12))
    (let ((x3 (amount* "$100.00")))
      (assert-value-equal x3 x12))

    (let ((x4 (amount "$100.00")))
      (assert-value-equal x4 x12))
    (let ((x5 (amount* "$100.00")))
      (assert-value-equal x5 x12))
    (let ((x6 (amount "$100.00")))
      (assert-value-equal x6 x12))
    (let ((x7 (amount* "$100.00")))
      (assert-value-equal x7 x12))

    (assert-valid x12)))

(defun read-string (function string)
  (with-input-from-string (in string)
    (funcall function in)))

(define-test read-amount/commoditized
  (assert-equal "$0.00" (format-value (read-string #'read-amount "$0")))
  (assert-equal "$0.10" (format-value (read-string #'read-amount "$0.10")))
  (assert-equal "$12.10" (format-value (read-string #'read-amount*
						    "$12.1000000000000")))
  (assert-equal "$12.10" (format-value (read-string #'read-amount*
						    "$12.10000")))

  (assert-equal "EUR 12.10001"
		(format-value (read-string #'read-amount "EUR 12.10001")))
  (assert-equal "EUR 12.10000"
		(format-value (read-string #'read-amount* "EUR 12.1"))))

(define-test exact-amount
  (assert-equal "$0.0000" (format-value (exact-amount "$0.0000")))
  (assert-equal "$0.00" (format-value (amount* "$0.0000"))))

(define-test read-exact-amount
  (assert-equal "$0.0000"
		(format-value (read-string #'read-exact-amount
					   "$0.0000"))))

(define-test value-zerop
  (assert-true (value-zerop (amount* "$0")))
  (assert-true (value-zerop (amount* "$0.00000000000")))
  (assert-true (value-zerop (amount* "EDU 0")))
  (assert-true (value-zerop (amount* "0")))
  (assert-true (value-zerop (amount* "0000000")))
  (assert-true (value-zerop (amount* "0.0000000000000")))
  ;; It's would display as zero...
  (assert-true (value-zerop (amount* "$0.0000000000000000000000000000001")))
  ;; But it's not *really* zero
  (assert-false (value-zerop* (amount* "$0.0000000000000000000000000000001"))))

(define-test value-plusp
  (assert-false (value-plusp (amount* "$0")))
  (assert-false (value-plusp (amount* "$0.00000000000")))
  (assert-false (value-plusp (amount* "EDU 0")))
  (assert-false (value-plusp (amount* "0")))
  (assert-false (value-plusp (amount* "0000000")))
  (assert-false (value-plusp (amount* "0.0000000000000")))
  ;; It's would display as zero...
  (assert-false (value-plusp (amount* "$0.0000000000000000000000000000001")))
  ;; But it's *really* plusp
  (assert-true (value-plusp* (amount* "$0.0000000000000000000000000000001")))

  (assert-true (value-plusp (amount* "$1")))
  (assert-false (value-plusp (amount* "$0.00000000001")))
  (assert-true (value-plusp (amount* "EDU 1")))
  (assert-true (value-plusp (amount* "1")))
  (assert-true (value-plusp (amount* "10000000")))
  ;; It's would display as zero...
  (assert-false (value-plusp (amount* "$0.0000000000000000000000000000001")))
  ;; But it's not *really* zero
  (assert-true (value-plusp* (amount* "$0.0000000000000000000000000000001"))))

(define-test value-minusp
  (assert-false (value-minusp (amount* "-$0")))
  (assert-false (value-minusp (amount* "-$0.00000000000")))
  (assert-false (value-minusp (amount* "EDU -0")))
  (assert-false (value-minusp (amount* "-0")))
  (assert-false (value-minusp (amount* "-0000000")))
  (assert-false (value-minusp (amount* "-0.0000000000000")))
  ;; It's would display as zero...
  (assert-false (value-minusp (amount* "$-0.0000000000000000000000000000001")))
  ;; But it's *really* minusp
  (assert-true (value-minusp* (amount* "$-0.0000000000000000000000000000001")))

  (assert-true (value-minusp (amount* "-$1")))
  (assert-false (value-minusp (amount* "-$0.00000000001")))
  (assert-true (value-minusp (amount* "-EDU 1")))
  (assert-true (value-minusp (amount* "-1")))
  (assert-true (value-minusp (amount* "-10000000")))
  ;; It's would display as zero...
  (assert-false (value-minusp (amount* "$-0.0000000000000000000000000000001")))
  ;; But it's not *really* zero
  (assert-true (value-minusp* (amount* "$-0.0000000000000000000000000000001"))))

(define-test equality
  (let ((x1 123456)
	(x2 456789)
	(x3 333333)
	(x4 (amount "123456.0"))
	(x5 (amount* "123456.0"))
	(x6 (amount "123456.0")))

    (assert-true (value= x1 123456))
    (assert-true (value/= x1 x2))
    (assert-true (value= x1 (subtract x2 x3)))
    (assert-true (value= x1 x4))
    (assert-true (value= x4 x5))
    (assert-true (value= x4 x6))

    (assert-true (value= x1 123456))
    (assert-true (value= 123456 x1))
    (assert-true (value= x1 123456))
    (assert-true (value= 123456 x1))
    (assert-true (value= x1 (amount "123456.0")))
    (assert-true (value= (amount "123456.0") x1))

    (assert-valid x1)
    (assert-valid x2)
    (assert-valid x3)
    (assert-valid x4)
    (assert-valid x5)
    (assert-valid x6)))

(define-test commodity-equality
  (let ((x1  (amount "$123.45"))
	(x2  (amount "-$123.45"))
	(x3  (amount "$-123.45"))
	(x4  (amount "DM 123.45"))
	(x5  (amount "-DM 123.45"))
	(x6  (amount "DM -123.45"))
	(x7  (amount "123.45 euro"))
	(x8  (amount "-123.45 euro"))
	#-ecl (x9  (amount "123.45€"))
	#-ecl (x10 (amount "-123.45€")))

    (assert-true (value/= x1 x2))
    (assert-condition 'amount-error (value/= x1 x4))
    (assert-condition 'amount-error (value/= x1 x7))
    #-ecl (assert-condition 'amount-error (value/= x1 x9))
    (assert-true (value= x2 x3))
    (assert-true (value/= x4 x5))
    (assert-true (value= x5 x6))
    (assert-true (value= x7 (negate x8)))
    #-ecl (assert-true (value= x9 (negate x10)))

    (assert-valid x1)
    (assert-valid x2)
    (assert-valid x3)
    (assert-valid x4)
    (assert-valid x5)
    (assert-valid x6)
    (assert-valid x7)
    (assert-valid x8)
    #-ecl (assert-valid x9)
    #-ecl (assert-valid x10)))

(define-test comparisons
  (let ((x1 -123)
	(x2 123)
	(x3 (amount "-123.45"))
	(x4 (amount "123.45"))
	(x5 (amount "-123.45"))
	(x6 (amount "123.45")))

    (assert-true (value> x1 x3))
    (assert-true (value<= x3 x5))
    (assert-true (value>= x3 x5))
    (assert-true (value< x3 x1))
    (assert-true (value< x3 x4))

    (assert-true (value< x1 100))
    (assert-true (value> 100 x1))
    (assert-true (value< x1 100))
    (assert-true (value> 100 x1))
    (assert-true (value< x1 (amount "100.0")))
    (assert-true (value> (amount "100.0") x1))

    (assert-valid x1)
    (assert-valid x2)
    (assert-valid x3)
    (assert-valid x4)
    (assert-valid x5)
    (assert-valid x6)))

(define-test commodity-comparisons
  (let ((x1 (amount "$-123"))
	(x2 (amount "$123.00"))
	(x3 (exact-amount "$-123.4544"))
	(x4 (exact-amount "$123.4544"))
	(x5 (amount "$-123.45"))
	(x6 (amount "$123.45"))
	(x7 (amount "DM 123.45")))

    (assert-true (value> x1 x3))
    (assert-true (value<= x3 x5))
    (assert-true (value< x3 x5))
    (assert-true (value<= x3 x5))
    (assert-false (value= x3 x5))
    (assert-true (value< x3 x1))
    (assert-true (value< x3 x4))
    (assert-condition 'amount-error (value= x6 x7))
    (assert-condition 'amount-error (value< x6 x7))

    (assert-valid x1)
    (assert-valid x2)
    (assert-valid x3)
    (assert-valid x4)
    (assert-valid x5)
    (assert-valid x6)))

(define-test integer-addition
  (let ((x1 (amount "123"))
	(y1 (amount "456")))

    (assert-value-equal 579 (add x1 y1))
    (assert-value-equal 579 (add x1 456))
    (assert-value-equal 579 (add 456 x1))

    (setf x1 (add x1 456))
    (assert-value-equal 579 x1)
    (setf x1 (add x1 456))
    (assert-value-equal 1035 x1)

    (let ((x4 (amount "123456789123456789123456789")))

      (assert-value-equal (amount "246913578246913578246913578")
			  (add x4 x4))

      (assert-valid x1)
      (assert-valid y1)
      (assert-valid x4))))

(define-test fractional-addition
  (let ((x1 (amount "123.123"))
	(y1 (amount "456.456")))

    (assert-value-equal (amount "579.579") (add x1 y1))
    (assert-value-equal (amount "579.579") (add x1 (amount "456.456")))
    (assert-value-equal (amount "579.579") (add (amount "456.456") x1))

    (setf x1 (add x1 (amount "456.456")))
    (assert-value-equal (amount "579.579") x1)
    (setf x1 (add x1 (amount "456.456")))
    (assert-value-equal (amount "1036.035") x1)
    (setf x1 (add x1 456))
    (assert-value-equal (amount "1492.035") x1)

    (let ((x2 (amount "123456789123456789.123456789123456789")))
      (assert-value-equal (amount "246913578246913578.246913578246913578")
			  (add x2 x2))
      (assert-valid x1)
      (assert-valid y1)
      (assert-valid x2))))

(define-test commodity-addition
  (let ((x1 (amount "$123.45"))
	(x2 (exact-amount "$123.456789"))
	(x3 (amount "DM 123.45"))
	(x4 (amount "123.45 euro"))
	#-ecl (x5 (amount "123.45€"))
	(x6 (amount "123.45")))

    (assert-value-equal (amount "$246.90") (add x1 x1))
    (assert-value-not-equal (amount "$246.91") (add x1 x2))
    (assert-value-equal (exact-amount "$246.906789") (add x1 x2))

    ;; Converting to string drops internal precision
    (assert-equal "$246.90" (format-value (add x1 x1)))
    (assert-equal "$246.91" (format-value (add x1 x2)))

    (assert-value-equal (amount "DM 246.90") (add x3 x3))
    (assert-value-equal (amount "246.90 euro") (add x4 x4))
    #-ecl (assert-value-equal (amount "246.90€") (add x5 x5))

    (assert-equal "DM 246.90" (format-value (add x3 x3)))
    (assert-equal "246.90 euro" (format-value (add x4 x4)))
    #-ecl (assert-equal "246.90€" (format-value (add x5 x5)))

    (setf x1 (add x1 (amount "$456.45")))
    (assert-value-equal (amount "$579.90") x1)
    (setf x1 (add x1 (amount "$456.45")))
    (assert-value-equal (amount "$1036.35") x1)
    (setf x1 (add x1 (amount "$456")))
    (assert-value-equal (amount "$1492.35") x1)

    (let ((x7 (exact-amount "$123456789123456789.123456789123456789")))
      (assert-value-equal (exact-amount "$246913578246913578.246913578246913578")
			  (add x7 x7))
      (assert-valid x1)
      (assert-valid x2)
      (assert-valid x3)
      (assert-valid x4)
      #-ecl (assert-valid x5)
      (assert-valid x6)
      (assert-valid x7))))

(define-test integer-subtraction
  (let ((x1 (amount "123"))
	(y1 (amount "456")))

    (assert-value-equal (amount "333") (subtract y1 x1))
    (assert-value-equal (amount "-333") (subtract x1 y1))
    (assert-value-equal (amount "23") (subtract x1 100))
    (assert-value-equal (amount "-23") (subtract 100 x1))

    (setf x1 (subtract x1 (amount "456")))
    (assert-value-equal (amount "-333") x1)
    (setf x1 (subtract x1 456))
    (assert-value-equal (amount "-789") x1)

    (let ((x4 (amount "123456789123456789123456789"))
	  (y4 (amount "8238725986235986")))
      (assert-value-equal (amount "123456789115218063137220803")
			  (subtract x4 y4))
      (assert-value-equal (amount "-123456789115218063137220803")
			  (subtract y4 x4))
      (assert-valid x1)
      (assert-valid y1)
      (assert-valid x4)
      (assert-valid y4))))

(define-test fractional-subtraction
  (let ((x1 (amount "123.123"))
	(y1 (amount "456.456")))

    (assert-value-equal (amount "-333.333") (subtract x1 y1))
    (assert-value-equal (amount "333.333") (subtract y1 x1))

    (setf x1 (subtract x1 (amount "456.456")))
    (assert-value-equal (amount "-333.333") x1)
    (setf x1 (subtract x1 (amount "456.456")))
    (assert-value-equal (amount "-789.789") x1)
    (setf x1 (subtract x1 456))
    (assert-value-equal (amount "-1245.789") x1)

    (let ((x2 (amount "123456789123456789.123456789123456789"))
	  (y2 (amount "9872345982459.248974239578")))
      (assert-value-equal (amount "123446916777474329.874482549545456789")
			  (subtract x2 y2))
      (assert-value-equal (amount "-123446916777474329.874482549545456789")
			  (subtract y2 x2))
      (assert-valid x1)
      (assert-valid y1)
      (assert-valid x2)
      (assert-valid y2))))

(define-test commodity-subtraction
  (let ((x1 (amount "$123.45"))
	(x2 (exact-amount "$123.456789"))
	(x3 (amount "DM 123.45"))
	(x4 (amount "123.45 euro"))
	#-ecl (x5 (amount "123.45€"))
	(x6 (amount "123.45")))

    (assert-true (value-zerop (subtract x1 x1)))
    (assert-true (value-zerop* (subtract x1 x1)))
    (assert-value-equal (amount "$0") (subtract x1 x1))
    (assert-value-equal (amount "$23.45") (subtract x1 (amount "$100.00")))
    (assert-value-equal (amount "$-23.45") (subtract (amount "$100.00") x1))
    (assert-value-not-equal (amount "$-0.01") (subtract x1 x2))
    (assert-value-equal (exact-amount "$-0.006789") (subtract x1 x2))

    ;; Converting to string drops internal precision.  If an amount is
    ;; zero it drops the commodity as well.
    (assert-equal "$0.00" (format-value (subtract x1 x1)))
    (assert-equal "$-0.01" (format-value (subtract x1 x2)))

    (assert-value-equal (amount "DM 0.00") (subtract x3 x3))
    (assert-value-equal (amount "DM 23.45") (subtract x3 (amount "DM 100.00")))
    (assert-value-equal (amount "DM -23.45") (subtract (amount "DM 100.00") x3))
    (assert-value-equal (amount "0.00 euro") (subtract x4 x4))
    (assert-value-equal (amount "23.45 euro") (subtract x4 (amount "100.00 euro")))
    (assert-value-equal (amount "-23.45 euro") (subtract (amount "100.00 euro") x4))
    #-ecl (assert-value-equal (amount "0.00€") (subtract x5 x5))
    #-ecl (assert-value-equal (amount "23.45€") (subtract x5 (amount "100.00€")))
    #-ecl (assert-value-equal (amount "-23.45€") (subtract (amount "100.00€") x5))

    (assert-equal "DM 0.00" (format-value (subtract x3 x3)))
    (assert-equal "DM 23.45" (format-value (subtract x3 (amount "DM 100.00"))))
    (assert-equal "DM -23.45" (format-value (subtract (amount "DM 100.00") x3)))
    (assert-equal "0.00 euro" (format-value (subtract x4 x4)))
    (assert-equal "23.45 euro" (format-value (subtract x4 (amount "100.00 euro"))))
    (assert-equal "-23.45 euro" (format-value (subtract (amount "100.00 euro") x4)))
    #-ecl (assert-equal "0.00€" (format-value (subtract x5 x5)))
    #-ecl (assert-equal "23.45€" (format-value (subtract x5 (amount "100.00€"))))
    #-ecl (assert-equal "-23.45€" (format-value (subtract (amount "100.00€") x5)))

    (setf x1 (subtract x1 (amount "$456.45")))
    (assert-value-equal (amount "$-333.00") x1)
    (setf x1 (subtract x1 (amount "$456.45")))
    (assert-value-equal (amount "$-789.45") x1)
    (setf x1 (subtract x1 (amount "$456")))
    (assert-value-equal (amount "$-1245.45") x1)

    (let ((x7 (exact-amount "$123456789123456789.123456789123456789"))
	  (x8 (exact-amount "$2354974984698.98459845984598")))

      (assert-value-equal (exact-amount "$123454434148472090.138858329277476789")
			  (subtract x7 x8))
      (assert-equal "$123,454,434,148,472,090.138858329277476789"
		    (format-value (subtract x7 x8)))
      (assert-equal "$123,454,434,148,472,090.14"
		    (format-value (multiply (amount "$1.00") (subtract x7 x8))))
      (assert-value-equal (exact-amount "$-123454434148472090.138858329277476789")
			  (subtract x8 x7))
      (assert-equal "$-123,454,434,148,472,090.138858329277476789"
		    (format-value (subtract x8 x7)))
      (assert-equal "$-123,454,434,148,472,090.14"
		    (format-value (multiply (amount "$1.00") (subtract x8 x7)))))

    (assert-valid x1)
    (assert-valid x2)
    (assert-valid x3)
    (assert-valid x4)
    #-ecl (assert-valid x5)
    (assert-valid x6)))

(define-test integer-multiplication
  (let ((x1 (amount "123"))
	(y1 (amount "456")))

    (assert-value-equal (amount "0") (multiply x1 0))
    (assert-value-equal (amount "0") (multiply 0 x1))
    (assert-value-equal x1 (multiply x1 1))
    (assert-value-equal x1 (multiply (amount "1") x1))
    (assert-value-equal x1 (multiply 1 x1))
    (assert-value-equal (negate x1) (multiply x1 -1))
    (assert-value-equal (negate x1) (multiply (amount "-1") x1))
    (assert-value-equal (negate x1) (negate (multiply 1 x1)))
    (assert-value-equal (amount "56088") (multiply x1 y1))
    (assert-value-equal (amount "56088") (multiply y1 x1))
    (assert-value-equal (amount "56088") (multiply x1 456))
    (assert-value-equal (amount "56088") (multiply (amount "456") x1))
    (assert-value-equal (amount "56088") (multiply 456 x1))

    (setf x1 (multiply x1 (amount "123")))
    (assert-value-equal (amount "15129") x1)
    (setf x1 (multiply x1 123))
    (assert-value-equal (amount "1860867") x1)

    (let ((x4 (amount "123456789123456789123456789")))
      (assert-value-equal (amount "15241578780673678546105778281054720515622620750190521")
			  (multiply x4 x4)))

    (assert-valid x1)
    (assert-valid y1)))

(define-test fractional-multiplication
  (let ((x1 (amount "123.123"))
	(y1 (amount "456.456")))

    (assert-value-equal (amount "0") (multiply x1 0))
    (assert-value-equal (amount "0") (multiply (amount "0") x1))
    (assert-value-equal (amount "0") (multiply 0 x1))
    (assert-value-equal x1 (multiply x1 1))
    (assert-value-equal x1 (multiply (amount "1") x1))
    (assert-value-equal x1 (multiply 1 x1))
    (assert-value-equal (negate x1) (multiply x1 -1))
    (assert-value-equal (negate x1) (multiply (amount "-1") x1))
    (assert-value-equal (negate x1) (negate (multiply 1 x1)))
    (assert-value-equal (amount "56200.232088") (multiply x1 y1))
    (assert-value-equal (amount "56200.232088") (multiply y1 x1))
    (assert-value-equal (amount "56200.232088") (multiply x1 (amount "456.456")))
    (assert-value-equal (amount "56200.232088") (multiply (amount "456.456") x1))

    (setf x1 (multiply x1 (amount "123.123")))
    (assert-value-equal (amount "15159.273129") x1)
    (setf x1 (multiply x1 (amount "123.123")))
    (assert-value-equal (amount "1866455.185461867") x1)
    (setf x1 (multiply x1 123))
    (assert-value-equal (amount "229573987.811809641") x1)

    (let ((x2 (amount "123456789123456789.123456789123456789")))
      (assert-value-equal
       (amount "15241578780673678546105778311537878.046486820281054720515622620750190521")
       (multiply x2 x2))) 

    (assert-valid x1)
    (assert-valid y1)))

(define-test commodity-multiplication
  (let ((x1 (amount "$123.12"))
	(y1 (amount "$456.45"))
	(x2 (exact-amount "$123.456789"))
	(x3 (amount "DM 123.45"))
	(x4 (amount "123.45 euro"))
	#-ecl (x5 (amount "123.45€")))

    (assert-value-equal (amount "$0.00") (multiply x1 0))
    (assert-value-equal (amount "$0.00") (multiply 0 x1))
    (assert-value-equal x1 (multiply x1 1))
    (assert-value-equal x1 (multiply 1 x1))
    (assert-value-equal (negate x1) (multiply x1 -1))
    (assert-value-equal (negate x1) (negate (multiply 1 x1)))
    (assert-value-equal (exact-amount "$56198.124") (multiply x1 y1))
    (assert-equal "$56,198.12" (format-value (multiply x1 y1)))
    (assert-value-equal (exact-amount "$56198.124") (multiply y1 x1))
    (assert-equal "$56,198.12" (format-value (multiply y1 x1)))

    ;; Internal amounts retain their precision even when being
    ;; converted to strings
    (assert-value-equal (exact-amount "$15199.99986168") (multiply x1 x2))
    (assert-value-equal (exact-amount "$15199.99986168") (multiply x2 x1))
    (assert-equal "$15,200.00" (format-value (multiply x1 x2)))
    (assert-equal "$15,199.99986168" (format-value (multiply x2 x1)))

    (setf x1 (multiply x1 (amount "123.12")))
    (assert-value-equal (exact-amount "$15158.5344") x1)
    (assert-equal "$15,158.53" (format-value x1))
    (setf x1 (multiply x1 (amount "123.12")))
    (assert-value-equal (exact-amount "$1866318.755328") x1)
    (assert-equal "$1,866,318.76" (format-value x1))
    (setf x1 (multiply x1 123))
    (assert-value-equal (exact-amount "$229557206.905344") x1)
    (assert-equal "$229,557,206.91" (format-value x1))

    (let ((x7 (exact-amount "$123456789123456789.123456789123456789")))
      (assert-value-equal
       (exact-amount "$15241578780673678546105778311537878.046486820281054720515622620750190521")
       (multiply x7 x7))) 

    (assert-valid x1)
    (assert-valid x2)
    (assert-valid x3)
    (assert-valid x4)
    #-ecl (assert-valid x5)))

(define-test integer-division
  (let ((x1 (amount "123"))
	(y1 (amount "456")))

    (assert-condition 'amount-error (divide x1 0))
    (assert-value-equal (amount "0") (divide (amount "0") x1))
    (assert-value-equal (amount "0") (divide 0 x1))
    (assert-value-equal x1 (divide x1 1))
    (assert-value-equal (amount "0.008130") (divide (amount "1") x1))
    (assert-value-equal (amount "0.008130") (divide 1 x1))
    (assert-value-equal (negate x1) (divide x1 -1))
    (assert-value-equal (negate (amount "0.008130")) (divide (amount "-1") x1))
    (assert-value-equal (negate (amount "0.008130")) (divide -1 x1))
    (assert-value-equal (amount "0.269737") (divide x1 y1))
    (assert-value-equal (amount "3.707317") (divide y1 x1))
    (assert-value-equal (amount "0.269737") (divide x1 456))
    (assert-value-equal (amount "3.707317") (divide (amount "456") x1))
    (assert-value-equal (amount "3.707317") (divide 456 x1))

    (setf x1 (divide x1 (amount "456")))
    (assert-value-equal (amount "0.269737") x1)
    (setf x1 (divide x1 456))
    (assert-value-equal (amount "0.00059152850877193") x1)

    (let ((x4 (amount "123456789123456789123456789"))
	  (y4 (amount "56")))

      (assert-value-equal (amount "1") (divide x4 x4))
      (assert-value-equal (amount "2204585520061728377204585.517857") (divide x4 y4)))

    (assert-valid x1)
    (assert-valid y1)))

(define-test fractional-division
  (let ((x1 (amount "123.123"))
	(y1 (amount "456.456")))

    (assert-condition 'amount-error (divide x1 0))
    (assert-value-equal (amount "0.00812195934") (divide (amount "1.0") x1))
    (assert-value-equal x1 (divide x1 (amount "1.0")))
    (assert-value-equal (negate x1) (divide x1 (amount "-1.0")))
    (assert-value-equal (negate (amount "0.00812195934")) (divide (amount "-1.0") x1))
    (assert-value-equal (amount "0.269736842105263") (divide x1 y1))
    (assert-value-equal (amount "3.707317073170732") (divide y1 x1))
    (assert-value-equal (amount "0.269736842105263") (divide x1 (amount "456.456")))
    (assert-value-equal (amount "3.707317073170732") (divide (amount "456.456") x1))

    (setf x1 (divide x1 (amount "456.456")))
    (assert-value-equal (amount "0.269736842105263") x1)
    (setf x1 (divide x1 (amount "456.456")))
    (assert-value-equal (amount "0.000590937225286255411255411255411255411") x1)
    (setf x1 (divide x1 456))
    (assert-value-equal (amount "0.000001295914967733016252753094858358016252192982456140350877192982456140350877192982") x1)

    (let ((x4 (amount "1234567891234567.89123456789"))
	  (y4 (amount "56.789")))
      (assert-value-equal (amount "1.0") (divide x4 x4))
      (assert-value-equal (amount "21739560323910.7554497273748437197344556164046")
			  (divide x4 y4)))
  
    (assert-valid x1)
    (assert-valid y1)))

(define-test commodity-division
  (let ((x1 (amount "$123.12"))
	(y1 (amount "$456.45"))
	(x2 (exact-amount "$123.456789"))
	(x3 (amount "DM 123.45"))
	(x4 (amount "123.45 euro"))
	#-ecl (x5 (amount "123.45€")))

    (assert-condition 'amount-error (divide x1 0))
    (assert-true (value-zerop (divide 0 x1)))
    (assert-value-equal (amount "$0.00") (divide 0 x1))
    (assert-value-equal x1 (divide x1 1))
    (assert-value-equal (exact-amount "$0.00812216") (divide 1 x1))
    (assert-value-equal (negate x1) (divide x1 -1))
    (assert-value-equal (exact-amount "$-0.00812216") (divide -1 x1))
    (assert-value-equal (exact-amount "$0.26973382") (divide x1 y1))
    (assert-equal "$0.27" (format-value (divide x1 y1)))
    (assert-value-equal (exact-amount "$3.70735867") (divide y1 x1))
    (assert-equal "$3.71" (format-value (divide y1 x1)))

    ;; Internal amounts retain their precision even when being
    ;; converted to strings
    (assert-value-equal (exact-amount "$0.99727201") (divide x1 x2))
    (assert-value-equal (exact-amount "$1.00273545321637426901") (divide x2 x1))
    (assert-equal "$1.00" (format-value (divide x1 x2)))
    (assert-equal "$1.00273545321637426901" (format-value (divide x2 x1)))

    (setf x1 (divide x1 (amount "123.12")))
    (assert-value-equal (exact-amount "$1.00") x1)
    (assert-equal "$1.00" (format-value x1))
    (setf x1 (divide x1 (amount "123.12")))
    (assert-value-equal (exact-amount "$0.00812216") x1)
    (assert-equal "$0.01" (format-value x1))
    (setf x1 (divide x1 123))
    (assert-value-equal (exact-amount "$0.00006603") x1)
    (assert-equal "$0.00" (format-value x1))

    (let ((x6 (amount* "$237235987235987.98723987235978"))
	  (x7 (amount* "$123456789123456789.123456789123456789")))
      (assert-value-equal (amount* "$1") (divide x7 x7))

      (assert-value-equal
       (amount* "0.0019216115121765559608381226612019501046413574469262")
       (divide (amount-quantity x6) (amount-quantity x7)))
      ;; Commoditized values, when not dealing with exact-amount arithmetic,
      ;; only preserve the commodity's display precision plus
      ;; *extra-precision*.
      (assert-value-equal
       (amount* "$0.00192161")
       (divide x6 x7))

      (assert-value-equal
       (amount* "520.39654928343335571379527154924040947271699678158689736256")
       (divide (amount-quantity x7) (amount-quantity x6)))
      (assert-value-equal
       (amount* "$520.39654928")
       (divide x7 x6)))

    (assert-valid x1)
    (assert-valid x2)
    (assert-valid x3)
    (assert-valid x4)
    #-ecl (assert-valid x5)))

(define-test negation
  (let* ((x1 (amount "-123456"))
	 (x3 (amount "-123.456"))
	 (x5 (amount "-123456"))
	 (x6 (amount "-123.456"))
	 (x7 (amount "-123456"))
	 (x8 (amount "-123.456"))
	 (x9 (negate x3)))

    (assert-value-equal x5 x1)
    (assert-value-equal x7 x1)
    (assert-value-equal x6 x3)
    (assert-value-equal x8 x3)
    (assert-value-equal (negate x6) x9)
    (assert-value-equal (negate x3) x9)

    (let ((x10 (negate x9)))
      (assert-value-equal x3 x10))

    (assert-valid x1)
    (assert-valid x3)
    (assert-valid x5)
    (assert-valid x6)
    (assert-valid x7)
    (assert-valid x8)
    (assert-valid x9)))

(define-test commodity-negation
  (let ((x1 (amount "$123.45"))
	(x2 (amount "-$123.45"))
	(x3 (amount "$-123.45"))
	(x4 (amount "DM 123.45"))
	(x5 (amount "-DM 123.45"))
	(x6 (amount "DM -123.45"))
	(x7 (amount "123.45 euro"))
	(x8 (amount "-123.45 euro"))
	#-ecl (x9 (amount "123.45€"))
	#-ecl (x10 (amount "-123.45€")))

    (assert-value-equal (amount "$-123.45") (negate x1))
    (assert-value-equal (amount "$123.45") (negate x2))
    (assert-value-equal (amount "$123.45") (negate x3))
    (assert-value-equal (amount "DM -123.45") (negate x4))
    (assert-value-equal (amount "DM 123.45") (negate x5))
    (assert-value-equal (amount "DM 123.45") (negate x6))
    (assert-value-equal (amount "-123.45 euro") (negate x7))
    (assert-value-equal (amount "123.45 euro") (negate x8))
    #-ecl (assert-value-equal (amount "-123.45€") (negate x9))
    #-ecl (assert-value-equal (amount "123.45€") (negate x10))

    (assert-value-equal (amount "$-123.45") (negate x1))
    (assert-value-equal (amount "$123.45") (negate x2))
    (assert-value-equal (amount "$123.45") (negate x3))

    (assert-equal "$-123.45" (format-value (negate x1)))
    (assert-equal "$123.45" (format-value (negate x2)))
    (assert-equal "$123.45" (format-value (negate x3)))
    (assert-equal "DM -123.45" (format-value (negate x4)))
    (assert-equal "DM 123.45" (format-value (negate x5)))
    (assert-equal "DM 123.45" (format-value (negate x6)))
    (assert-equal "-123.45 euro" (format-value (negate x7)))
    (assert-equal "123.45 euro" (format-value (negate x8)))
    #-ecl (assert-equal "-123.45€" (format-value (negate x9)))
    #-ecl (assert-equal "123.45€" (format-value (negate x10)))

    (assert-value-equal (amount "$-123.45") (negate x1))
    (assert-value-equal (amount "$123.45") (negate x2))
    (assert-value-equal (amount "$123.45") (negate x3))

    (assert-valid x1)
    (assert-valid x2)
    (assert-valid x3)
    (assert-valid x4)
    (assert-valid x5)
    (assert-valid x6)
    (assert-valid x7)
    (assert-valid x8)
    #-ecl (assert-valid x9)
    #-ecl (assert-valid x10)))

(define-test abs
  (let ((x1 (amount "-1234"))
	(x2 (amount "1234")))

    (assert-value-equal (amount "1234") (value-abs x1))
    (assert-value-equal (amount "1234") (value-abs x2))

    (assert-valid x1)
    (assert-valid x2)))

(define-test commodity-abs
  (let ((x1 (amount "$-1234.56"))
	(x2 (amount "$1234.56")))

    (assert-value-equal (amount "$1234.56") (value-abs x1))
    (assert-value-equal (amount "$1234.56") (value-abs x2))

    (assert-valid x1)
    (assert-valid x2)))

(define-test fractional-round
  (let ((x1 (amount "1234.567890")))
    (assert-value-equal 6 (amount-precision x1))
  
    (let ((y7 (value-round x1 7))
	  (y6 (value-round x1 6))
	  (y5 (value-round x1 5))
	  (y4 (value-round x1 4))
	  (y3 (value-round x1 3))
	  (y2 (value-round x1 2))
	  (y1 (value-round x1 1))
	  (y0 (value-round x1 0)))

      (assert-equal 6 (amount-precision y7))
      (assert-equal 6 (amount-precision y6))
      (assert-equal 5 (amount-precision y5))
      (assert-equal 4 (amount-precision y4))
      (assert-equal 3 (amount-precision y3))
      (assert-equal 2 (amount-precision y2))
      (assert-equal 1 (amount-precision y1))
      (assert-equal 0 (amount-precision y0))

      (assert-value-equal (amount "1234.56789") y7)
      (assert-value-equal (amount "1234.56789") y6)
      (assert-value-equal (amount "1234.56789") y5)
      (assert-value-equal (amount "1234.5679") y4)
      (assert-value-equal (amount "1234.568") y3)
      (assert-value-equal (amount "1234.57") y2)
      (assert-value-equal (amount "1234.6") y1)
      (assert-value-equal (amount "1235") y0))
  
    (let ((x2 (amount "9876.543210")))
      (assert-value-equal (amount "9876.543210") (value-round x2 6))
      (assert-value-equal (amount "9876.54321") (value-round x2 5))
      (assert-value-equal (amount "9876.5432") (value-round x2 4))
      (assert-value-equal (amount "9876.543") (value-round x2 3))
      (assert-value-equal (amount "9876.54") (value-round x2 2))
      (assert-value-equal (amount "9876.5") (value-round x2 1))
      (assert-value-equal (amount "9877") (value-round x2 0)))

    (let ((x3 (amount "-1234.567890")))
      (assert-value-equal (amount "-1234.56789") (value-round x3 6))
      (assert-value-equal (amount "-1234.56789") (value-round x3 5))
      (assert-value-equal (amount "-1234.5679") (value-round x3 4))
      (assert-value-equal (amount "-1234.568") (value-round x3 3))
      (assert-value-equal (amount "-1234.57") (value-round x3 2))
      (assert-value-equal (amount "-1234.6") (value-round x3 1))
      (assert-value-equal (amount "-1235") (value-round x3 0)))

    (let ((x4 (amount "-9876.543210")))
      (assert-value-equal (amount "-9876.543210") (value-round x4 6))
      (assert-value-equal (amount "-9876.54321") (value-round x4 5))
      (assert-value-equal (amount "-9876.5432") (value-round x4 4))
      (assert-value-equal (amount "-9876.543") (value-round x4 3))
      (assert-value-equal (amount "-9876.54") (value-round x4 2))
      (assert-value-equal (amount "-9876.5") (value-round x4 1))
      (assert-value-equal (amount "-9877") (value-round x4 0)))

    (let ((x5 (amount "0.0000000000000000000000000000000000001")))
      (assert-value-equal (amount "0.0000000000000000000000000000000000001")
			  (value-round x5 37))
      (assert-value-equal (amount 0) (value-round x5 36)))))

(define-test commodity-round
  (let ((x1 (exact-amount "$1234.567890")))
    (assert-value-equal (exact-amount "$1234.56789") (value-round x1 6))
    (assert-value-equal (exact-amount "$1234.56789") (value-round x1 5))
    (assert-value-equal (exact-amount "$1234.5679") (value-round x1 4))
    (assert-value-equal (exact-amount "$1234.568") (value-round x1 3))
    (assert-value-equal (amount "$1234.57") (value-round x1 2))
    (assert-value-equal (amount "$1234.6") (value-round x1 1))
    (assert-value-equal (amount "$1235") (value-round x1 0)))

  (let ((x2 (exact-amount "$9876.543210")))
    (assert-value-equal (exact-amount "$9876.543210") (value-round x2 6))
    (assert-value-equal (exact-amount "$9876.54321") (value-round x2 5))
    (assert-value-equal (exact-amount "$9876.5432") (value-round x2 4))
    (assert-value-equal (exact-amount "$9876.543") (value-round x2 3))
    (assert-value-equal (amount "$9876.54") (value-round x2 2))
    (assert-value-equal (amount "$9876.5") (value-round x2 1))
    (assert-value-equal (amount "$9877") (value-round x2 0)))

  (let ((x3 (exact-amount "$-1234.567890")))
    (assert-value-equal (exact-amount "$-1234.56789") (value-round x3 6))
    (assert-value-equal (exact-amount "$-1234.56789") (value-round x3 5))
    (assert-value-equal (exact-amount "$-1234.5679") (value-round x3 4))
    (assert-value-equal (exact-amount "$-1234.568") (value-round x3 3))
    (assert-value-equal (amount "$-1234.57") (value-round x3 2))
    (assert-value-equal (amount "$-1234.6") (value-round x3 1))
    (assert-value-equal (amount "$-1235") (value-round x3 0)))

  (let ((x4 (exact-amount "$-9876.543210")))
    (assert-value-equal (exact-amount "$-9876.543210") (value-round x4 6))
    (assert-value-equal (exact-amount "$-9876.54321") (value-round x4 5))
    (assert-value-equal (exact-amount "$-9876.5432") (value-round x4 4))
    (assert-value-equal (exact-amount "$-9876.543") (value-round x4 3))
    (assert-value-equal (amount "$-9876.54") (value-round x4 2))
    (assert-value-equal (amount "$-9876.5") (value-round x4 1))
    (assert-value-equal (amount "$-9877") (value-round x4 0)))

  (let ((x5 (amount "$123.45")))
    (setf x5 (multiply x5 (amount "100.12")))

    (assert-value-equal (exact-amount "$12359.814") x5)
    (assert-equal "$12,359.81" (format-value x5))
    (assert-equal "$12,359.8140" (format-value x5 :full-precision-p t))))

(define-test commodity-display-round
  (let ((x1 (amount "$0.85"))
	(x2 (amount "$0.1")))

    (setf x1 (multiply x1 (amount "0.19")))

    (assert-value-not-equal (amount "$0.16") x1)
    (assert-value-equalp (amount "$0.16") x1)
    (assert-value-equal (exact-amount "$0.1615") x1)
    (assert-equal "$0.16" (format-value x1))

    (assert-value-equal (amount "$0.10") x2)
    (assert-value-not-equal (exact-amount "$0.101") x2)
    (assert-equal "$0.10" (format-value x2))

    (setf x1 (multiply x1 7))

    (assert-value-not-equal (amount "$1.13") x1)
    (assert-value-equal (exact-amount "$1.1305") x1)
    (assert-equal "$1.13" (format-value x1))))

;; (define-test reduction
;;   (let ((x1 (amount "60s"))
;; 	(x2 (amount "600s"))
;; 	(x3 (amount "6000s"))
;; 	(x4 (amount "360000s"))
;; 	(x5 (amount "10m"))   ;; 600s
;; 	(x6 (amount "100m"))  ;; 6000s
;; 	(x7 (amount "1000m")) ;; 60000s
;; 	(x8 (amount "10000m"))	;; 600000s
;; 	(x9 (amount "10h"))	;; 36000s
;; 	(x10 (amount "100h"))	;; 360000s
;; 	(x11 (amount "1000h"))	;; 3600000s
;; 	(x12 (amount "10000h"))) ;; 36000000s

;;     (assert-value-equal x2 x5)
;;     (assert-value-equal x3 x6)
;;     (assert-value-equal x4 x10)
;;     (assert-equal "100.0h" (format-value (largest-units x4)))))

(define-test sign
  (let ((x1 (amount "0.0000000000000000000000000000000000001"))
	(x2 (amount "-0.0000000000000000000000000000000000001"))
	(x3 (amount "1"))
	(x4 (amount "-1")))

    (assert-true (> (sign x1) 0))
    (assert-true (< (sign x2) 0))
    (assert-true (> (sign x3) 0))
    (assert-true (< (sign x4) 0))

    (assert-valid x1)
    (assert-valid x2)
    (assert-valid x3)
    (assert-valid x4)))

(define-test commodity-sign
  (let ((x1 (exact-amount "$0.0000000000000000000000000000000000001"))
	(x2 (exact-amount "$-0.0000000000000000000000000000000000001"))
	(x3 (amount "$1"))
	(x4 (amount "$-1")))

    (assert-true (/= (sign x1) 0))
    (assert-true (/= (sign x2) 0))
    (assert-true (> (sign x3) 0))
    (assert-true (< (sign x4) 0))

    (assert-valid x1)
    (assert-valid x2)
    (assert-valid x3)
    (assert-valid x4)))

(define-test truth
  (let ((x1 (amount "1234"))
	(x2 (amount "1234.56")))

    (assert-true x1)
    (assert-true x2)

    (assert-valid x1)
    (assert-valid x2)))

(define-test for-zero
  (let ((x1 (amount "0.000000000000000000001")))
    (assert-false (value-zerop x1))
    (assert-false (value-zerop* x1))
    (assert-valid x1)))

(define-test commodity-for-zero
  (let ((x1 (amount* "$0.000000000000000000001")))
    (assert-true (value-zerop x1))
    (assert-false (value-zerop* x1))
    (assert-valid x1)))

(defvar latest-test-results nil)

(defun run-tests ()
  (setq latest-test-results 
        (list (textui-test-run (get-suite commodity-test-case))
              (textui-test-run (get-suite amount-test-case)))))
