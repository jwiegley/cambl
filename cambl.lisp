;; -*- mode: lisp -*-

;; cambl.lisp - Commoditized AMounts and Balances Library.

;;;_* Copyright (c) 2003-2008, John Wiegley.  All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;; - Redistributions of source code must retain the above copyright
;;   notice, this list of conditions and the following disclaimer.
;;
;; - Redistributions in binary form must reproduce the above copyright
;;   notice, this list of conditions and the following disclaimer in the
;;   documentation and/or other materials provided with the distribution.
;;
;; - Neither the name of New Artisans LLC nor the names of its
;;   contributors may be used to endorse or promote products derived from
;;   this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;;_* Commentary

;; This library provides a convenient facility for working with commoditized
;; values.  It does not allow compound units -- and so is not suited for
;; scientific operations -- but does work rather nicely for the purpose of
;; financial calculations.
;;
;; The primary entry point to using this library is the creation of VALUES,
;; which may be of many different types.  These VALUES are then used as
;; arguments to a set of basic mathematical OPERATIONS, which also yield
;; VALUES.  However, the type of the result may not be the same type as either
;; of the operands.
;;
;; For example, let's say you want to create a figure representing 10 US
;; dollars.  You then add an INTEGER to this amount, the number 200.  The
;; result of such an operation will be an amalgam of the two units involved:
;; Dollars, and No-units.  This dual-commodity value is represented as a
;; BALANCE object, whose printed form shows each commodity amount on its own
;; line.  For example:
;;
;; (let ((value (cambl:amount "$100.00")))
;;   (princ (cambl:format-value (cambl:add value 200))))
;;  => 200
;;     $100.00

;; This library aims to provide convenient access to commoditized math; that
;; is: math involving commodity units.  Unlike scientific units, there is no
;; concept of "compound" units.  If you divide 1kg by 1m, you do not get "1
;; kg/m", but "1 kg" (the unit of the second operand is ignored for
;; multiplication and division).  The intended use of this library is in
;; situations such as computing financial transactions.

;;;_ * Usage: Conventions

;; 

;;;_ * Usage: Amounts

;; There are just a few main entry points to the CAMBL library for dealing
;; with amounts (which is mainly how you'll use it).  Here is a quick list,
;; following by a description in the context of a REPL session.  Note that
;; where the name contains value, it will work for integers, amounts and
;; balances.  If it contains amount or balance, it only operates on entities
;; of that type.
;;
;;   amount[*]          ; create an amount from a string
;;   exact-amount       ; create an amount from a string which overrides
;;                      ; its commodity's display precision; this feature
;;                      ; "sticks" through any math operations
;;   parse-amount[*]    ; parse an amount from a string (alias for `amount')
;;   read-amount[*]     ; read an amount from a stream
;;   read-exact-amount  ; read an exact amount from a stream
;;   format-value       ; format a value to a string
;;   print-value        ; print a value to a stream
;;
;;   add[*]             ; perform math using values; the values are
;;   subtract[*]        ; changed as necessary to preserve information
;;   multiply[*]        ; (adding two amounts may result in a balance)
;;   divide[*]          ; the * versions change the first argument.
;;
;;   value-zerop        ; would the value display as zero?
;;   value-zerop*       ; is the value truly zero?
;;
;;   value-minusp       ; would the amount display as a negative amount?
;;   value-minusp*      ; is the amount truly negative?
;;   value-plusp        ; would it display as an amount greater than zero?
;;   value-plusp*       ; is it truly greater than zero?
;;
;;   value=, value/=    ; compare two values
;;   value<, value<=    ; compare values (not meaningful for all values,
;;   value>, value>=    ; such as balances)
;;
;;   value-equal        ; compare two values for exact match
;;   value-equalp       ; compare two values only after rounding to what
;;                      ; would be shown to the user (approximate match)
;;                      ; -- this is the same as value=
;;   value-not-equal    ; compare if two values for not an exact match
;;   value-not-equalp   ; compare two values after commodity rounding
;;
;;   value-lessp        ; same as value<
;;   value-lessp*       ; compare if a < b exactly, with full precision
;;   value-lesseqp      ; same as value<=
;;   value-lesseqp*     ; exact version of value<=
;;   value-greaterp     ; same as value>
;;   value-greaterp*    ; exact version of value>
;;   value-greatereqp   ; same as value>=
;;   value-greatereqp*  ; exact version of value>=
;;
;;   amount-precision   ; return the internal precision of an amount
;;   display-precision  ; return the "display" precision for an amount
;;                      ; or a commodity

;;;_ * Example Session

;; Interacting with CAMBL begin with creating an amount.  This is done most
;; easily from a string, but it can also be read from a stream:
;;
;;   (cambl:amount "$100.00") =>
;;     #<CAMBL:AMOUNT "$100.00" :KEEP-PRECISION-P NIL>
;;
;;   (with-input-from-string (in "$100.00"))
;;      (cambl:read-amount in)) =>
;;     #<CAMBL:AMOUNT "$100.00" :KEEP-PRECISION-P NIL>
;;
;; When you parse an amount using one of these two functions, CAMBL creates a
;; COMMODITY class for you, whose symbol name is "$".  This class remembers
;; details about how you used the commodity, such as the input precision and
;; the format of the commodity symbol.  Some of these details can be inspected
;; by looking at the amount's commodity directly:
;;
;;   (cambl:amount-commodity (cambl:amount "$100.00")) =>
;;     #<CAMBL::COMMODITY #S(CAMBL::COMMODITY-SYMBOL
;;                           :NAME $
;;                           :NEEDS-QUOTING-P NIL
;;                           :PREFIXED-P T
;;                           :CONNECTED-P T)
;;         :THOUSAND-MARKS-P NIL :DISPLAY-PRECISION 2>
;;
;; Here you can see that the commodity for $100.00 is $, and it knows that the
;; commodity should be prefixed to the amount, and that it gets connected to
;; the amount.  This commodity was used without any "thousand marks" (i.e.,
;; $1000.00 vs $1,000.00), and it has a maximum display precision of TWO
;; observed so far.  If we print sach an amount, we'll see the same style as
;; was input:
;;
;;   (cambl:format-value (cambl:amount "$100.00")) => "$100.00"
;;   (cambl:print-value (cambl:amount "$100.00"))  => NIL
;;     $100.00
;;
;; CAMBL observed how you used the "$" commodity, and now reports back all
;; dollar figures after the same fashion.  Even though there are no cents in
;; the amounts above, CAMBL will still record a full two digits of precision
;; (this becomes important during division, to guard against fractional losses
;; during repeated rounding).
;;
;;   (cambl:amount-precision (cambl:amount "$100.00")) => 2
;;
;; CAMBL remembers the greatest precision it has seen thus far, but never
;; records a lesser precision.  So if you parse $100.00 and then $100, both
;; values will be printed as $100.00.
;;
;; There are three functions for creating amounts, but they have some subtle
;; differences where precision is concerned.  They are: `amount', `amount*',
;; and `exact-amount'.  Here are the differences:
;;
;;   (cambl:amount "$100.00") =>
;;     #<CAMBL:AMOUNT "$100.00" :KEEP-PRECISION-P NIL>
;;
;;       a. amount has an internal precision of 2
;;       b. commodity $ has a display precision of 2 (if no other
;;          amount using a higher precision was observed so far)
;;       c. when printing, amount uses the commodity's precision
;;  
;;         (cambl:format-value *)                      => "$100.00"
;;         (cambl:format-value ** :full-precision-p t) => "$100.00"
;;
;;   (cambl:amount* "$100.0000") =>
;;     #<CAMBL:AMOUNT "$100.0000" :KEEP-PRECISION-P NIL>
;;
;;       a. amount has an internal precision of 4
;;       b. commodity $ still has a display precision of 2 (from above)
;;       c. when printing, amount uses the commodity's precision
;;  
;;         (cambl:format-value *)                      => "$100.00"
;;         (cambl:format-value ** :full-precision-p t) => "$100.0000"
;;
;;   (cambl:exact-amount "$100.0000") =>
;;     #<CAMBL:AMOUNT "$100.0000" :KEEP-PRECISION-P T>
;;
;;       a. amount has an internal precision of 4
;;       b. commodity $ still has a display precision of 2 (from above)
;;       c. when printing, amount uses its internal precision
;;  
;;         (cambl:format-value *)                      => "$100.0000"
;;         (cambl:format-value ** :full-precision-p t) => "$100.0000"
;;
;; There are similar variants for the stream reading functions:
;;
;;   read-amount
;;   read-amount*
;;   read-exact-amount
;;
;; NOTE: The KEEP-PRECISION-P property of an amount carries through any math
;; operations involving that amount, so that the final result is ealways
;; displayed using its own internal percision.
;;
;; The point of all this is that amounts are displayed as the user expects
;; them to be, but internally never lose information.  In fact, if you divide
;; two high internal precision amounts together, you'll get a new amount with
;; a very high internal precision, but which still displays as expected:
;;
;;   (setf *tmp* (cambl:divide  (cambl:amount "$100.00")
;;                              (cambl:amount "50000000")))
;;
;;   (cambl:format-value *tmp* :full-precision-p t) =>
;;     "$0.000002000000000"
;;   (cambl:format-value *tmp*) => "$0.00"
;;
;; You'll notice here that the amount displayed is not $0.00000200000000002.
;; This is because CAMBL does not try to capture every digit resulting from a
;; division; rather, it keeps six more digits of precision than is strictly
;; necessary so that even after millions of calculations, not a penny is lost.
;; If you find this is not enough slack for your calculations, you can set
;; CAMBL:*EXTRA-PRECISION* to a higher or lower value.

;;;_ * Usage: Commodities

;; CAMBL offers several methods for accessing the commodity information
;; relating to amounts:
;;
;;   amount-commodity          ; the COMMODITY referenced by an amount
;;   display-precision         ; display precision of an AMOUNT or COMMODITY
;;   commodity-qualified-name  ; the name used print a COMMODITY

;;;_* Package

;; (declaim (optimize (safety 1) (speed 3) (space 0)))
(declaim (optimize (debug 3) (safety 3) (speed 1) (space 0)))

(defpackage :cambl
  (:use :cl :red-black :local-time :periods)
  (:export pushend

	   value
	   valuep
	   amount
	   amount-p
	   amount*
	   parse-amount
	   parse-amount*
	   exact-amount
	   read-amount
	   read-amount*
	   read-exact-amount
	   amount-quantity

	   balance-p
	   amount-in-balance
	   get-amounts-map

	   value-zerop
	   value-zerop*
	   value-minusp
	   value-minusp*
	   value-plusp
	   value-plusp*

	   compare
	   compare*
	   sign
	   sign*

	   value-equal
	   value-equalp
	   value-not-equal
	   value-not-equalp
	   value=
	   value/=

	   value-lessp
	   value-lessp*
	   value-lesseqp
	   value-lesseqp*
	   value<
	   value<=

	   value-greaterp
	   value-greaterp*
	   value-greatereqp
	   value-greatereqp*
	   value>
	   value>=

	   value-abs
	   value-maybe-round
	   value-round
	   negate
	   add
	   subtract
	   multiply
	   divide

	   print-value
	   format-value

	   exchange-commodity

	   amount-commodity
	   balance-amounts
	   balance-first-amount
	   balance-commodities
	   balance-commodity-count
	   commodity-name
	   amount-precision
	   amount-keep-precision-p
	   display-precision

	   amount-error

	   market-value

	   commodity-qualified-name
	   commodity-thousand-marks-p
	   commodity-no-market-price-p
	   commodity-builtin-p
	   commodity-equal
	   commodity-equalp
	   commodity-lessp
	   commodity-price-history
	   compare-amounts-visually

	   annotated-commodity-p
	   make-commodity-annotation
	   commodity-annotation
	   commodity-annotation-equal
	   annotation-price
	   annotation-date
	   annotation-tag
	   annotate-commodity
	   annotated-commodity
	   strip-annotations

	   *default-commodity-pool*
	   commodity-pool
	   make-commodity-pool
	   reset-commodity-pool
	   find-commodity
	   find-annotated-commodity

	   commodity-error))

(in-package :cambl)

;;;_* Types

;;;_ - COMMODITY-SYMBOL

(defstruct commodity-symbol
  (name "" :type string)
  (needs-quoting-p nil :type boolean)
  (prefixed-p nil :type boolean)
  (connected-p nil :type boolean))

;;;_ - COMMODITY-POOL

(defstruct commodity-pool
  (by-name-map (make-hash-table :test 'equal) :type hash-table)
  (default-commodity nil))

(defvar *default-commodity-pool* (make-commodity-pool))
(defvar *default-display-precision* 3)

;;;_ + COMMODITY

(defclass commodity ()
  ((symbol :accessor get-symbol :initarg :symbol)
   (description :accessor get-description
		:initarg :description :initform nil
		:type (or string null))
   (comment :accessor get-comment :initarg :comment
	    :initform nil :type (or string null))
   (thousand-marks-p :accessor get-thousand-marks-p
		     :initarg :thousand-marks-p
		     :initform nil :type boolean)
   (no-market-price-p :accessor get-no-market-price-p
		      :initarg :no-market-price-p
		      :initform nil :type boolean)
   (builtin-p :accessor get-builtin-p :initarg :builtin-p
	      :initform nil :type boolean)
   (display-precision :accessor get-display-precision
		      :initarg :display-precision
		      :initform 0 :type fixnum)
   (price-history :accessor get-price-history
		  :initarg :price-history :initform nil)
   (commodity-pool :accessor get-commodity-pool :initarg :commodity-pool
		   :type commodity-pool)
   (qualified-name :accessor commodity-qualified-name
		   :initarg :qualified-name :type (or string null))))

(defmethod print-object ((commodity commodity) stream)
  (print-unreadable-object (commodity stream :type t)
    (princ (get-symbol commodity) stream)
    (format stream "~%    :THOUSAND-MARKS-P ~S :DISPLAY-PRECISION ~D"
	    (get-thousand-marks-p commodity)
	    (get-display-precision commodity))))

;;;_ + COMMODITY-ANNOTATION

(defstruct (commodity-annotation (:conc-name annotation-))
  (price nil) ;; (:type (or amount null))
  (date nil :type (or fixed-time null))
  (tag nil :type (or string null)))

;;;_ + ANNOTATED-COMMODITY

(defclass annotated-commodity (commodity)
  ((referent :accessor get-referent
		       :initarg :referent :type commodity)
   (annotation :accessor get-commodity-annotation :initarg :annotation
	       :type commodity-annotation)))

(declaim (inline annotated-commodity-p))
(defun annotated-commodity-p (object)
  (typep object 'annotated-commodity))

(defmethod print-object ((annotated-commodity annotated-commodity) stream)
  (print-unreadable-object (annotated-commodity stream :type t)
    (print-object (get-referent annotated-commodity) stream)))

;;;_ + AMOUNT

;; Amounts are bignums with a specific attached commodity.  [TODO: Also, when
;; math is performed with them, they retain knowledge of the origins of their
;; value].

;; The following three members determine whether lot details are maintained
;; when working with commoditized values.  The default is false for all three.
;;
;; Let's say a user adds two values of the following form:
;;   10 AAPL + 10 AAPL {$20}
;;
;; This expression adds ten shares of Apple stock with another ten shares that
;; were purchased for $20 a share.  If `keep_price' is false, the result of
;; this expression will be an amount equal to 20 AAPL.  If `keep_price' is
;; true, the expression yields an exception for adding amounts with different
;; commodities.  In that case, a balance_t object must be used to store the
;; combined sum.

(defstruct (amount (:print-function print-amount))
  (commodity nil :type (or commodity null))
  (quantity 0 :type rational)
  (keep-precision-p nil :type boolean))

;;;_ + BALANCE

(defstruct (balance (:conc-name get-)
		    (:print-function print-balance))
  (amounts-map nil))

;;;_ + VALUE

(deftype value ()
  '(or rational amount balance))

(declaim (inline valuep))
(defun valuep (object)
  (typep object 'value))

;;;_* Generics

;;;_ + Public generics

(defgeneric value-zerop (value))
(defgeneric value-zerop* (value))	; is it *really* zerop?
(defgeneric value-minusp (value))
(defgeneric value-minusp* (value))	; is it *really* minusp?
(defgeneric value-plusp (value))
(defgeneric value-plusp* (value))	; is it *really* plusp?

(defgeneric compare (left right))
(defgeneric compare* (left right))
(defgeneric value-equal (left right))
(defgeneric value-equalp (left right))
(defgeneric value-not-equal (left right))
(defgeneric value-not-equalp (left right))
(defgeneric value= (left right))
(defgeneric value/= (left right))

(defgeneric value-abs (value))
(defgeneric negate (value))
(defgeneric add (value-a value-b))
(defgeneric subtract (value-a value-b))
(defgeneric multiply (value-a value-b))
(defgeneric divide (value-a value-b))

(defgeneric print-value (value &key output-stream omit-commodity-p
			       full-precision-p width latter-width
			       line-feed-string))
(defgeneric format-value (value &key omit-commodity-p full-precision-p
				width latter-width line-feed-string))

(defgeneric commodity-name (item &optional no-annotation))
(defgeneric display-precision (item))

(defgeneric market-value (any-item &optional fixed-time))

(defgeneric commodity-annotation (item))
(defgeneric commodity-annotation-equal (left-item right-item))
(defgeneric annotate-commodity (item annotation))
(defgeneric strip-annotations (any-item &key keep-price keep-date keep-tag))

;;;_ - Private generics

(defgeneric commodity-annotation-empty-p (item))

;;;_* Object printing functions

(defun print-amount (amount stream depth)
  (print-unreadable-object (amount stream :type t)
    (print-object (amount-commodity amount) stream)
    (format stream "~&~vA~S :KEEP-PRECISION-P ~S"
	    depth " " (amount-quantity amount)
	    (amount-keep-precision-p amount))))

(defun print-balance (balance stream depth)
  (declare (ignore depth))
  (print-unreadable-object (balance stream :type t)
    (mapc #'(lambda (entry)
	      (terpri stream)
	      (princ "     " stream)
	      (princ (cdr entry) stream))
	  (get-amounts-map balance))))

;;;_* Constants

(defparameter *invalid-symbol-chars*
  #(#|        0   1   2   3   4   5   6   7   8   9   a   b   c   d   e   f |#
    #| 00 |# nil nil nil nil nil nil nil nil nil  t   t  nil nil  t  nil nil
    #| 10 |# nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    #| 20 |#  t   t   t  nil nil nil  t  nil  t   t   t   t   t   t   t   t 
    #| 30 |#  t   t   t   t   t   t   t   t   t   t   t   t   t   t   t   t 
    #| 40 |#  t  nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    #| 50 |# nil nil nil nil nil nil nil nil nil nil nil  t  nil  t   t  nil
    #| 60 |# nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    #| 70 |# nil nil nil nil nil nil nil nil nil nil nil  t  nil  t   t  nil
    #| 80 |# nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    #| 90 |# nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    #| a0 |# nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    #| b0 |# nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    #| c0 |# nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    #| d0 |# nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    #| e0 |# nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil
    #| f0 |# nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil)
  "The invalid commodity symbol characters are:

  Space Tab Newline Return
  0-9 . , ; - + * / ^ ? : & | ! = \"
  < > { } [ ] ( ) @")

;;;_* Functions

;;;_ * COMMODITY

(defmacro define-commodity-accessor (function accessor)
  `(progn
     (defun ,function (comm)
       (if (annotated-commodity-p comm)
	   (,function (get-referent comm))
	   (,accessor comm)))
     (defsetf ,function (comm) (value)
       `(setf (,',accessor ,comm) ,value))))

(define-commodity-accessor commodity-symbol get-symbol)
(define-commodity-accessor commodity-description get-description) 
(define-commodity-accessor commodity-comment get-comment) 
(define-commodity-accessor commodity-thousand-marks-p get-thousand-marks-p) 
(define-commodity-accessor commodity-no-market-price-p get-no-market-price-p) 
(define-commodity-accessor commodity-builtin-p get-builtin-p) 
(define-commodity-accessor commodity-price-history get-price-history) 
(define-commodity-accessor commodity-pool get-commodity-pool) 

;;;_ * AMOUNT and BALANCE

;;;_  + Error class

(define-condition amount-error (error) 
  ((description :reader error-description :initarg :msg :type string)
   (operands :reader error-operands :initarg :operands :type list))
  (:report (lambda (condition stream)
	     (princ (error-description condition) stream))))

;;;_  + Create AMOUNT objects from strings

(defun amount (string &key (pool *default-commodity-pool*))
  (declare (type string string))
  (read-amount (make-string-input-stream string)
	       :observe-properties-p t :pool pool))

(defun amount* (string &key (pool *default-commodity-pool*))
  (declare (type string string))
  (read-amount (make-string-input-stream string)
	       :observe-properties-p nil :pool pool))

(declaim (inline parse-amount))
(defun parse-amount (&rest args)
  (apply #'amount args))

(declaim (inline parse-amount*))
(defun parse-amount* (&rest args)
  (apply #'amount args))

(declaim (inline exact-amount))
(defun exact-amount (string &key (pool *default-commodity-pool*))
  (declare (type string string))
  (let ((amount (amount* string :pool pool)))
    (setf (amount-keep-precision-p amount) t)
    amount))

;;;_  + Read AMOUNT objects from streams

(defun read-amount-quantity (in)
  (declare (type stream in))
  (declare (optimize (speed 3) (safety 0)))
  (with-output-to-string (buf nil :element-type 'base-char)
    (let (last-special)
      (loop
	 for c = (read-char in nil) while c do
	 (if (digit-char-p c)
	     (progn
	       (when last-special
		 (write-char last-special buf)
		 (setf last-special nil))
	       (write-char c buf))
	     (if (and (null last-special)
		      (or (char= c #\-)
			  (char= c #\.)
			  (char= c #\,)))
		 (setf last-special c)
		 (progn
		   (unread-char c in)
		   (return)))))
      (if last-special
	  (unread-char last-special in)))))

(defun peek-char-in-line (in &optional skip-whitespace)
  (declare (type stream in))
  (declare (optimize (speed 3) (safety 0)))
  (loop
     for c = (peek-char nil in nil)
     while (and c (char/= c #\Newline)) do
     (if (and skip-whitespace
	      (or (char= #\Space c)
		  (char= #\Tab c)))
	 (read-char in)
	 (return c))))

(defun read-amount (in &key (observe-properties-p t)
		    (pool *default-commodity-pool*))
  "Parse an AMOUNT from the input IN, which may be a stream or string.

  If :OBSERVE-PROPERTIES-P is T (the default), any display details noticed in
this amount will be set as defaults for displaying this kind of commodity in
the future.

  If :POOL is set, any commodities created by this routine (a maximum possible
of two, if an annotated price is given with a second commodity) will be
associated with the given commodity pool.

  The possible syntax for an amount is:
  
  [-]NUM[ ]SYM [ANNOTATION]
  SYM[ ][-]NUM [ANNOTATION]"
  (declare (type stream in))
  (declare (optimize (speed 3) (safety 0)))

  (let ((connected-p t) (prefixed-p t)
	symbol quantity details
	negative-p thousand-marks-p)

    (when (char= #\- (peek-char-in-line in t))
      (setf negative-p t)
      (read-char in))

    (if (digit-char-p (peek-char-in-line in t))
	(progn
	  (setf quantity (the simple-string
			   (read-amount-quantity in)))
	  (let ((c (peek-char-in-line in)))
	    (if (and (characterp c)
		     (char= #\Space c))
		(setf connected-p nil))
	    (let ((n (peek-char-in-line in t)))
	      (when (and (characterp n)
			 (not (char= #\Newline n)))
		(setf symbol (read-commodity-symbol in))
		(if symbol
		    (setf prefixed-p nil))))))
	(progn
	  (setf symbol (read-commodity-symbol in))
	  (if (char= #\Space (peek-char nil in))
	      (setf connected-p nil))
	  (let ((n (peek-char-in-line in t)))
	    (if (and (characterp n)
		     (not (char= #\Newline n)))
		(setf quantity (the simple-string
				 (read-amount-quantity in)))
		(error 'amount-error
		       :msg "No quantity specified for amount")))))

    (let ((c (peek-char-in-line in t)))
      (if (and (characterp c)
	       (or (char= c #\{)
		   (char= c #\[)
		   (char= c #\()))
	  (setf details (read-commodity-annotation in))))

    ;; Now that we have the full commodity symbol, create the commodity object
    ;; it refers to
    (multiple-value-bind (commodity newly-created-p)
	(if (and symbol
		 (not (zerop (length (commodity-symbol-name symbol)))))
	    (if details
		(find-annotated-commodity symbol details :pool pool
					  :create-if-not-exists-p t)
		(find-commodity symbol :pool pool :create-if-not-exists-p t))
	    (values nil nil))

      (let* ((last-comma
	      (locally #+sbcl (declare (sb-ext:muffle-conditions
					sb-ext:compiler-note))
		       (position #\, quantity :from-end t)))
	     (last-period
	      (locally #+sbcl (declare (sb-ext:muffle-conditions
					sb-ext:compiler-note))
		       (position #\. quantity :from-end t)))

	     (precision (cond ((and last-comma last-period)
			       (- (length quantity)
				  (if (> last-comma last-period)
				      last-comma last-period) 1))
			      (last-period
			       (- (length quantity) last-period 1))
			      (t 0)))
	     (denominator (the integer (expt 10 (the fixnum precision))))
	     (quantity
	      (/ (parse-integer (delete-if #'(lambda (c)
					       (or (char= #\. c)
						   (char= #\, c)))
					   quantity))
		 denominator)))

	(if last-comma (setf thousand-marks-p t))
	(if negative-p (setf quantity (- quantity)))

	(if commodity
	    (progn
	      (when (or newly-created-p observe-properties-p)
		(let ((base-commodity
		       (if (annotated-commodity-p commodity)
			   (get-referent commodity)
			   commodity)))
		  ;; Observe the commodity usage details we noticed while
		  ;; parsing
		  (setf (commodity-symbol-prefixed-p
			 (get-symbol base-commodity)) prefixed-p)
		  (setf (commodity-symbol-connected-p
			 (get-symbol base-commodity)) connected-p)
		  (if thousand-marks-p
		      (setf (get-thousand-marks-p base-commodity)
			    thousand-marks-p))

		  (if (> precision (the fixnum
				     (get-display-precision base-commodity)))
		      (setf (get-display-precision base-commodity)
			    precision))))

	      (make-amount :commodity commodity :quantity quantity))

	    ;; If the amount had no commodity at all, always preserve full
	    ;; precision, as if the user had used `exact-amount'.
	    quantity)))))

(declaim (inline read-amount*))
(defun read-amount* (in &key (pool *default-commodity-pool*))
  (declare (type stream in))
  (read-amount in :observe-properties-p nil :pool pool))

(declaim (inline read-exact-amount))
(defun read-exact-amount (in &key (pool *default-commodity-pool*))
  (declare (type stream in))
  (let ((amount (read-amount* in :pool pool)))
    (setf (amount-keep-precision-p amount) t)
    amount))

;;;_  + BALANCE commodity details

(declaim (inline balance))
(defun balance (&rest amounts)
  ;; Although it may seem inefficient, this is the only way to generate the
  ;; correct value, considering that AMOUNTS may contain duplicated commodity
  ;; values, and the commodities may not be in correctly sorted order.
  (assert (> (length amounts) 1))
  (reduce #'add amounts))

(declaim (inline balance-commodities))
(defun balance-commodities (balance)
  (mapcar #'car (get-amounts-map balance)))

(declaim (inline balance-amounts))
(defun balance-amounts (balance)
  (mapcar #'cdr (get-amounts-map balance)))

(declaim (inline balance-first-amount))
(defun balance-first-amount (balance)
  (cdar (get-amounts-map balance)))

(declaim (inline balance-commodity-count))
(defun balance-commodity-count (balance)
  (length (get-amounts-map balance)))

;;;_  + Unary truth tests

(declaim (inline value-maybe-round))
(defun value-maybe-round (amount)
  (declare (type amount amount))
  (if (amount-keep-precision-p amount)
      amount
      (value-round amount)))

(defmethod value-zerop ((rational rational))
  (zerop rational))
(defmethod value-zerop ((amount amount))
  (zerop (amount-quantity (value-maybe-round amount))))
(defmethod value-zerop ((balance balance))
  (mapc #'(lambda (entry)
	    (unless (value-zerop (cdr entry))
	      (return-from value-zerop)))
	(get-amounts-map balance))
  t)

(defmethod value-zerop* ((rational rational))
  (zerop rational))
(defmethod value-zerop* ((amount amount))
  (zerop (amount-quantity amount)))
(defmethod value-zerop* ((balance balance))
  (mapc #'(lambda (entry)
	    (unless (value-zerop* (cdr entry))
	      (return-from value-zerop*)))
	(get-amounts-map balance))
  t)

(defmethod value-minusp ((rational rational))
  (minusp rational))
(defmethod value-minusp ((amount amount))
  (minusp (amount-quantity (value-maybe-round amount))))
(defmethod value-minusp ((balance balance))
  (mapc #'(lambda (entry)
	    (unless (value-minusp (cdr entry))
	      (return-from value-minusp)))
	(get-amounts-map balance))
  t)

(defmethod value-minusp* ((rational rational))
  (minusp rational))
(defmethod value-minusp* ((amount amount))
  (minusp (amount-quantity amount)))
(defmethod value-minusp* ((balance balance))
  (mapc #'(lambda (entry)
	    (unless (value-minusp* (cdr entry))
	      (return-from value-minusp*)))
	(get-amounts-map balance))
  t)

(defmethod value-plusp ((rational rational))
  (plusp rational))
(defmethod value-plusp ((amount amount))
  (plusp (amount-quantity (value-maybe-round amount))))
(defmethod value-plusp ((balance balance))
  (mapc #'(lambda (entry)
	    (unless (value-plusp (cdr entry))
	      (return-from value-plusp)))
	(get-amounts-map balance))
  t)

(defmethod value-plusp* ((rational rational))
  (plusp rational))
(defmethod value-plusp* ((amount amount))
  (plusp (amount-quantity amount)))
(defmethod value-plusp* ((balance balance))
  (mapc #'(lambda (entry)
	    (unless (value-plusp* (cdr entry))
	      (return-from value-plusp*)))
	(get-amounts-map balance))
  t)

;;;_  + Commodity equality

(declaim (inline commodity-equal))
(defun commodity-equal (a b)
  "Two commodities are EQUAL if they are the same object."
  (eq a b))

(declaim (inline commodity-equalp))
(defun commodity-equalp (a b)
  (eq (if (annotated-commodity-p a)
	  (get-referent a) a)
      (if (annotated-commodity-p b)
	  (get-referent b) b)))

;;;_  + AMOUNT comparison (return sort order of value)

(defun verify-amounts (left right capitalized-gerund)
  (declare (type amount left))
  (declare (type amount right))
  (unless (commodity-equal (amount-commodity left)
			   (amount-commodity right))
    (error 'amount-error :msg
	   (format nil "~A amounts with different commodities: ~A != ~A"
		   capitalized-gerund
		   (commodity-name (amount-commodity left))
		   (commodity-name (amount-commodity right))))))

(defmethod compare ((left rational) (right rational))
  (- left right))
(defmethod compare ((left amount) (right rational))
  (- (amount-quantity (value-maybe-round left)) right))
(defmethod compare ((left rational) (right amount))
  (- left (amount-quantity (value-maybe-round right))))
(defmethod compare ((left amount) (right amount))
  (verify-amounts left right "Comparing")
  (- (amount-quantity (value-maybe-round left))
     (amount-quantity (value-maybe-round right))))

(defmethod compare* ((left rational) (right rational))
  (- left right))
(defmethod compare* ((left amount) (right rational))
  (- (amount-quantity left) right))
(defmethod compare* ((left rational) (right amount))
  (- left (amount-quantity right)))
(defmethod compare* ((left amount) (right amount))
  (verify-amounts left right "Exactly comparing")
  (- (amount-quantity left) (amount-quantity right)))

(declaim (inline sign))
(defun sign (amount)
  "Return -1, 0 or 1 depending on the sign of AMOUNT."
  (etypecase amount
    (rational (sign* amount))
    (amount (sign* (value-maybe-round amount)))))

(defun sign* (amount)
  "Return -1, 0 or 1 depending on the sign of AMOUNT."
  (etypecase amount
    (rational
     (if (minusp amount)
	 -1
	 (if (plusp amount)
	     1
	     0)))
    (amount
     (let ((quantity (amount-quantity amount)))
       (if (minusp quantity)
	   -1
	   (if (plusp quantity)
	       1
	       0))))))

;;;_  + Equality tests

(defun balance-equal (left right test-func)
  (declare (type function test-func))
  (let ((left-amounts-map (get-amounts-map left))
	(right-amounts-map (get-amounts-map right)))
    (loop while (and left-amounts-map right-amounts-map
		     (commodity-equal (caar left-amounts-map)
				      (caar right-amounts-map))
		     (funcall test-func (cdar left-amounts-map)
			      (cdar right-amounts-map))) do
	 (setf left-amounts-map (cdr left-amounts-map)
	       right-amounts-map (cdr right-amounts-map)))
    (and (null left-amounts-map)
	 (null right-amounts-map))))

(defmethod value-equal ((left rational) (right rational))
  (= left right))
(defmethod value-equal ((left rational) (right amount))
  nil)
(defmethod value-equal ((left rational) (right balance))
  nil)
(defmethod value-equal ((left amount) (right rational))
  nil)
(defmethod value-equal ((left amount) (right amount))
  (and (commodity-equal (amount-commodity left)
			(amount-commodity right))
       (= (amount-quantity left) (amount-quantity right))))
(defmethod value-equal ((left amount) (right balance))
  nil)
(defmethod value-equal ((left balance) (right rational))
  nil)
(defmethod value-equal ((left balance) (right amount))
  nil)
(defmethod value-equal ((left balance) (right balance))
  (balance-equal left right #'value-equal))

(defmethod value-equalp ((left rational) (right rational))
  (= left right))
(defmethod value-equalp ((left rational) (right amount))
  nil)
(defmethod value-equalp ((left rational) (right balance))
  nil)
(defmethod value-equalp ((left amount) (right rational))
  nil)
(defmethod value-equalp ((left amount) (right amount))
  (and (commodity-equal (amount-commodity left)
			(amount-commodity right))
       (= (amount-quantity (value-maybe-round left))
	  (amount-quantity (value-maybe-round right)))))
(defmethod value-equalp ((left amount) (right balance))
  nil)
(defmethod value-equalp ((left balance) (right rational))
  nil)
(defmethod value-equalp ((left balance) (right amount))
  nil)
(defmethod value-equalp ((left balance) (right balance))
  (balance-equal left right #'value-equalp))

(defmethod value-not-equal ((left rational) (right rational))
  (not (value-equal left right)))
(defmethod value-not-equal ((left rational) (right amount))
  (not (value-equal left right)))
(defmethod value-not-equal ((left rational) (right balance))
  (not (value-equal left right)))
(defmethod value-not-equal ((left amount) (right rational))
  (not (value-equal left right)))
(defmethod value-not-equal ((left amount) (right amount))
  (not (value-equal left right)))
(defmethod value-not-equal ((left amount) (right balance))
  (not (value-equal left right)))
(defmethod value-not-equal ((left balance) (right rational))
  (not (value-equal left right)))
(defmethod value-not-equal ((left balance) (right amount))
  (not (value-equal left right)))
(defmethod value-not-equal ((left balance) (right balance))
  (not (value-equal left right)))

(defmethod value-not-equalp ((left rational) (right rational))
  (not (value-equalp left right)))
(defmethod value-not-equalp ((left rational) (right amount))
  (not (value-equalp left right)))
(defmethod value-not-equalp ((left rational) (right balance))
  (not (value-equalp left right)))
(defmethod value-not-equalp ((left amount) (right rational))
  (not (value-equalp left right)))
(defmethod value-not-equalp ((left amount) (right amount))
  (not (value-equalp left right)))
(defmethod value-not-equalp ((left amount) (right balance))
  (not (value-equalp left right)))
(defmethod value-not-equalp ((left balance) (right rational))
  (not (value-equalp left right)))
(defmethod value-not-equalp ((left balance) (right amount))
  (not (value-equalp left right)))
(defmethod value-not-equalp ((left balance) (right balance))
  (not (value-equalp left right)))

(defmethod value= ((left rational) (right rational))
  (value-equalp left right))
(defmethod value= ((left rational) (right amount))
  (value-equalp left right))
(defmethod value= ((left rational) (right balance))
  (value-equalp left right))
(defmethod value= ((left amount) (right rational))
  (value-equalp left right))
(defmethod value= ((left amount) (right amount))
  (value-equalp left right))
(defmethod value= ((left amount) (right balance))
  (value-equalp left right))
(defmethod value= ((left balance) (right rational))
  (value-equalp left right))
(defmethod value= ((left balance) (right amount))
  (value-equalp left right))
(defmethod value= ((left balance) (right balance))
  (value-equalp left right))

(defmethod value/= ((left rational) (right rational))
  (not (value-equalp left right)))
(defmethod value/= ((left rational) (right amount))
  (not (value-equalp left right)))
(defmethod value/= ((left rational) (right balance))
  (not (value-equalp left right)))
(defmethod value/= ((left amount) (right rational))
  (not (value-equalp left right)))
(defmethod value/= ((left amount) (right amount))
  (not (value-equalp left right)))
(defmethod value/= ((left amount) (right balance))
  (not (value-equalp left right)))
(defmethod value/= ((left balance) (right rational))
  (not (value-equalp left right)))
(defmethod value/= ((left balance) (right amount))
  (not (value-equalp left right)))
(defmethod value/= ((left balance) (right balance))
  (not (value-equalp left right)))

;;;_  + Comparison tests

(declaim (inline value-lessp value-lessp*))
(defun value-lessp (left right)
  (minusp (compare left right)))
(defun value-lessp* (left right)
  (minusp (compare* left right)))

(declaim (inline value-lesseqp value-lesseqp*))
(defun value-lesseqp (left right)
  (<= (compare left right) 0))
(defun value-lesseqp* (left right)
  (<= (compare* left right) 0))

(declaim (inline value< value<=))
(defun value< (left right)
  (minusp (compare left right)))
(defun value<= (left right)
  (<= (compare left right) 0))

(declaim (inline value-greaterp value-greaterp*))
(defun value-greaterp (left right)
  (plusp (compare left right)))
(defun value-greaterp* (left right)
  (plusp (compare* left right)))

(declaim (inline value-greatereqp value-greatereqp*))
(defun value-greatereqp (left right)
  (>= (compare left right) 0))
(defun value-greatereqp* (left right)
  (>= (compare* left right) 0))

(declaim (inline value> value>=))
(defun value> (left right)
  (plusp (compare left right)))
(defun value>= (left right)
  (>= (compare left right) 0))

;;;_  + Unary math operators

(defmethod value-abs ((rational rational))
  (abs rational))

(defmethod value-abs ((amount amount))
  (assert amount)
  (if (minusp (amount-quantity amount))
      (make-amount :commodity (amount-commodity amount)
		   :quantity (- (amount-quantity amount))
		   :keep-precision-p (amount-keep-precision-p amount))
      amount))

(defmacro transform-balance (balance predicate function &key (first-only nil)
			     (skip-to-next t) (lookahead nil))
  `(make-balance :amounts-map
		 (fprog:apply-to-list (get-amounts-map ,balance)
				      ,predicate ,function
				      :first-only ,first-only
				      :skip-to-next ,skip-to-next
				      :lookahead ,lookahead)))

(defmethod value-abs ((balance balance))
  (transform-balance balance
		     #'(lambda (cell) (value-minusp* (cdr cell)))
		     #'(lambda (list)
			 (cons (cons (caar list)
				     (value-abs (cdar list)))
			       (cdr list)))))

(defun value-round (amount &optional precision)
  "Round the given AMOUNT to the stated PRECISION.

  If PRECISION is less than the current internal precision, data will be lost.
If it is greater, this operation has no effect."
  (declare (type (or fixnum null) precision))
  (declare (optimize (speed 3) (safety 0)))
  (let ((divisor (and precision
		      (/ 1 (the integer
			     (expt 10 (the fixnum precision)))))))
    (etypecase amount
      (rational (if divisor
		    (* (round amount divisor) divisor)
		    amount))
      (amount
       (unless divisor
	 (setf divisor (/ 1 (the integer
			      (expt 10 (the fixnum
					 (display-precision
					  (amount-commodity amount))))))))
       (make-amount
	:commodity (amount-commodity amount)
	:quantity (* (round (amount-quantity amount)
			    divisor) divisor)
	:keep-precision-p (amount-keep-precision-p amount))))))

(defmethod negate ((rational rational))
  (- rational))

(defmethod negate ((amount amount))
  (make-amount
   :commodity (amount-commodity amount)
   :quantity (- (amount-quantity amount))
   :keep-precision-p (amount-keep-precision-p amount)))

(defmethod negate ((balance balance))
  (make-balance :amounts-map
		(mapcar #'(lambda (cell)
			    (cons (car cell) (negate (cdr cell))))
			(get-amounts-map balance))))

;;;_  + Find AMOUNT of COMMODITY in a BALANCE

(declaim (inline amount-in-balance))
(defun amount-in-balance (balance commodity)
  (cdr (assoc commodity (get-amounts-map balance)
	      :test #'commodity-equal)))

;;;_  + Binary math operators

;;;_   : Addition

(defun balance-helper (left right)
  (declare (type rational left))
  (declare (type amount right))
  (make-balance :amounts-map
		(list (cons nil left)
		      (cons (amount-commodity right) right))))

(defmethod add ((left rational) (right rational))
  (+ left right))

(defmethod add ((left rational) (right amount))
  (if (zerop left)
      right
      (balance-helper left right)))

(defmethod add ((left rational) (right balance))
  (add right left))

(defmethod add ((left amount) (right rational))
  (if (zerop right)
      left
      (balance-helper right left)))

(defun apply-between-amounts (left right function &optional adjustor)
  (if (commodity-equal (amount-commodity left)
		       (amount-commodity right))
      (let ((new-value (funcall function (amount-quantity left)
				(amount-quantity right))))
	(if (zerop new-value)
	    0
	    (make-amount
	     :commodity (amount-commodity left)
	     :quantity  new-value
	     :keep-precision-p (amount-keep-precision-p left))))
      (let ((left-cell (cons (amount-commodity left) left))
	    (right-cell (cons (amount-commodity right)
			      (if adjustor
				  (funcall adjustor right)
				  right))))
	(make-balance :amounts-map
		      (if (commodity-lessp (amount-commodity left)
					   (amount-commodity right))
			  (list left-cell right-cell)
			  (list right-cell left-cell))))))

(defmethod add ((left amount) (right amount))
  (apply-between-amounts left right #'+))

(defmethod add ((left amount) (right balance))
  (add right left))

(defun apply-to-balance (balance commodity value function &optional adjustor)
  (let ((amounts-map
	 (fprog:apply-to-list
	  (get-amounts-map balance)
	  #'(lambda (cell)
	      (or (commodity-equal (car cell) commodity)
		  (not (commodity-lessp (car cell) commodity))))
	  #'(lambda (list)
	      (let ((new-value (funcall function (cdar list) value)))
		(if (commodity-equal (caar list) commodity)
		    (if (value-zerop* new-value)
			(cdr list)
			(cons (cons commodity new-value) (cdr list)))
		    (cons (cons commodity
				(if adjustor
				    (funcall adjustor value)
				    value))
			  list))))
	  :first-only t :lookahead t)))
    (if (eq amounts-map (get-amounts-map balance))
	(make-balance :amounts-map
		      (append (get-amounts-map balance)
			      (list (cons commodity
					  (if adjustor
					      (funcall adjustor value)
					      value)))))
	;; If the result of the subtraction is a balance of one commodity,
	;; return it as a single amount.
	(if (= 1 (length amounts-map))
	    (cdar amounts-map)
	    (make-balance :amounts-map amounts-map)))))

(defmethod add ((left balance) (right rational))
  (if (zerop right)
      left
      (apply-to-balance left nil right #'add)))

(defmethod add ((left balance) (right amount))
  (apply-to-balance left (amount-commodity right) right #'add))

(defun apply-between-balances (left right function &optional adjustor)
  (let ((left-amounts (get-amounts-map left))
	(right-amounts (get-amounts-map right))
	new-amounts-map last-cell)
    (loop while (or left-amounts right-amounts) do
	 (cond ((and left-amounts right-amounts
		     (commodity-equal (caar left-amounts)
				      (caar right-amounts)))
		(let ((new-value (funcall function (cdar left-amounts)
					  (cdar right-amounts))))
		  (unless (value-zerop* new-value)
		    (let ((new-cell (cons (caar left-amounts) new-value)))
		      (setf last-cell
			    (if last-cell
				(setf (cdr last-cell) (list new-cell))
				(setf new-amounts-map (list new-cell))))))
		  (setf left-amounts (cdr left-amounts)
			right-amounts (cdr right-amounts))))
	       ((or (null right-amounts)
		    (and left-amounts
			 (commodity-lessp (caar left-amounts)
					  (caar right-amounts))))
		(setf last-cell
		      (if last-cell
			  (setf (cdr last-cell) (list (car left-amounts)))
			  (setf new-amounts-map (list (car left-amounts))))
		      left-amounts (cdr left-amounts)))
	       (t
		(setf last-cell
		      (if last-cell
			  (setf (cdr last-cell)
				(list (cons (caar right-amounts)
					    (if adjustor
						(funcall adjustor
							 (cdar right-amounts))
						(cdar right-amounts)))))
			  (setf new-amounts-map
				(list (cons (caar right-amounts)
					    (if adjustor
						(funcall adjustor
							 (cdar right-amounts))
						(cdar right-amounts))))))
		      right-amounts (cdr right-amounts)))))
    ;; If the result of the subtraction is a balance of one commodity, return
    ;; it as a single amount.
    (let ((new-length (length new-amounts-map)))
      (cond
	((zerop new-length)
	 0)
	((= 1 new-length)
	 (cdar new-amounts-map))
	(t
	 (make-balance :amounts-map new-amounts-map))))))

(defmethod add ((left balance) (right balance))
  (apply-between-balances left right #'add))

;;;_   : Subtraction

(defmethod subtract ((left rational) (right rational))
  (- left right))

(defmethod subtract ((left rational) (right amount))
  (if (zerop left)
      (negate right)
      (balance-helper left (negate right))))

(defmethod subtract ((left rational) (right balance))
  (add (negate right) left))

(defmethod subtract ((left amount) (right rational))
  (if (zerop right)
      left
      (balance-helper (- right) left)))

(defmethod subtract ((left amount) (right amount))
  (apply-between-amounts left right #'- #'negate))

(defmethod subtract ((left amount) (right balance))
  (add (negate right) left))

(defmethod subtract ((left balance) (right rational))
  (if (zerop right)
      left
      (apply-to-balance left nil right #'subtract #'negate)))

(defmethod subtract ((left balance) (right amount))
  (apply-to-balance left (amount-commodity right) right
		    #'subtract #'negate))

(defmethod subtract ((left balance) (right balance))
  (apply-between-balances left right #'subtract #'negate))

;;;_   : Multiplication

(defmethod multiply ((left rational) (right rational))
  (* left right))

(defmethod multiply ((left rational) (right amount))
  (multiply right left))

(defmethod multiply ((left rational) (right balance))
  (multiply right left))

(defmethod multiply ((left amount) (right rational))
  (if (zerop right)
      0
      (make-amount :commodity (amount-commodity left)
		   :quantity (* (amount-quantity left) right)
		   :keep-precision-p (amount-keep-precision-p left))))

(defmethod multiply ((left amount) (right amount))
  (make-amount
   :commodity (amount-commodity left)
   :quantity (* (amount-quantity left) (amount-quantity right))
   :keep-precision-p (amount-keep-precision-p left)))

(defun multiply-in-balance (balance commodity value)
  (transform-balance balance
		     #'(lambda (cell) (commodity-equal (car cell) commodity))
		     #'(lambda (list)
			 (cons (cons (caar list)
				     (multiply (cdar list) value))
			       (cdr list)))
		     :first-only t))

(defmethod multiply ((left balance) (right rational))
  (if (zerop right)
      left
      (multiply-in-balance left nil right)))

(defmethod multiply ((left balance) (right amount))
  (multiply-in-balance left (amount-commodity right) right))

;;;_   : Division

(defmethod divide ((left rational) (right rational))
  (/ left right))

(defmethod divide ((left rational) (right amount))
  (divide right left))

(defmethod divide ((left rational) (right balance))
  (divide right left))

(defmethod divide ((left amount) (right rational))
  (make-amount :commodity (amount-commodity left)
	       :quantity (/ (amount-quantity left) right)
	       :keep-precision-p (amount-keep-precision-p left)))

(defmethod divide ((left amount) (right amount))
  (make-amount
   :commodity (amount-commodity left)
   :quantity (/ (amount-quantity left) (amount-quantity right))
   :keep-precision-p (amount-keep-precision-p left)))

(defun divide-in-balance (balance commodity value)
  (transform-balance balance
		     #'(lambda (cell) (commodity-equal (car cell) commodity))
		     #'(lambda (list)
			 (cons (cons (caar list)
				     (divide (cdar list) value))
			       (cdr list)))
		     :first-only t))

(defmethod divide ((left balance) (right rational))
  (divide-in-balance left nil right))

(defmethod divide ((left balance) (right amount))
  (divide-in-balance left (amount-commodity right) right))

;;;_ * BALANCE specific

;;;_  + Print and format AMOUNT and BALANCE

(defun print-value-to-string (commodity commodity-symbol quantity
			      precision output-stream)
  (flet ((maybe-gap ()
	   (unless (commodity-symbol-connected-p commodity-symbol)
	     (princ #\Space output-stream))))

    (when (and commodity-symbol
	       (commodity-symbol-prefixed-p commodity-symbol))
      (princ (commodity-name commodity t) output-stream)
      (maybe-gap))

    (multiple-value-bind (quotient remainder)
	(truncate quantity)

      (format output-stream "~:[~,,vD~;~,,v:D~]"
	      (and commodity (commodity-thousand-marks-p commodity))
	      #\, quotient)

      (unless (or (and precision (zerop precision))
		  (zerop quantity))
	(format output-stream "~v,0,,'0$"
		(or precision *default-display-precision*)
		(abs remainder))))
      
    (when (and commodity-symbol
	       (not (commodity-symbol-prefixed-p commodity-symbol)))
      (maybe-gap)
      (princ (commodity-name commodity t) output-stream)))

  (if (annotated-commodity-p commodity)
      (format-commodity-annotation (commodity-annotation commodity)
				   :output-stream output-stream)))

(defmethod print-value ((integer integer) &key
			(output-stream *standard-output*)
			omit-commodity-p
			full-precision-p
			(width nil)
			latter-width
			line-feed-string)
  (declare (ignore omit-commodity-p full-precision-p latter-width
		   line-feed-string))
  (format output-stream "~vD" width integer))

(defmethod print-value ((rational rational) &key
			(output-stream *standard-output*)
			omit-commodity-p
			full-precision-p
			(width nil)
			latter-width
			line-feed-string)
  (declare (ignore omit-commodity-p full-precision-p latter-width
		   line-feed-string))
  (if (zerop rational)
      (format output-stream "~vD" width 0)
      ;; jww (2007-12-07): Make the amount of displayed precision configurable
      (format output-stream "~v,vF" width *default-display-precision*
	      rational)))

(defmethod print-value ((amount amount) &key
			(output-stream *standard-output*)
			(omit-commodity-p nil)
			(full-precision-p nil)
			(width nil)
			latter-width
			line-feed-string)
  (declare (type stream output-stream))
  (declare (type boolean omit-commodity-p))
  (declare (type boolean full-precision-p))
  (declare (type (or fixnum null) width))
  (declare (ignore latter-width line-feed-string))
  (declare (optimize (speed 3) (safety 0)))

  (let* ((commodity (amount-commodity amount))
	 (commodity-symbol (and (not omit-commodity-p)
				(commodity-symbol commodity)))
	 (display-precision
	  (unless (or full-precision-p
		      (amount-keep-precision-p amount))
	    (display-precision commodity))))
    (if width
	(format output-stream "~v@A" width
		(with-output-to-string (buffer)
		  (print-value-to-string commodity commodity-symbol
					 (amount-quantity amount)
					 display-precision buffer)))
	(print-value-to-string commodity commodity-symbol
			       (amount-quantity amount)
			       display-precision output-stream))))

(defun compare-amounts-visually (left right)
  (declare (type (cons (or commodity null) amount) left))
  (declare (type (cons (or commodity null) amount) right))
  (if (commodity-equal (car left) (car right))
      (value-lessp* (cdr left) (cdr right))
      (commodity-lessp (car left) (car left))))

(defmethod print-value ((balance balance) &key
			(output-stream *standard-output*)
			(omit-commodity-p nil)
			(full-precision-p nil)
			(width 12)
			(latter-width nil)
			(line-feed-string (format nil "~C" #\Newline)))
  (declare (type boolean omit-commodity-p))
  (declare (type boolean full-precision-p))
  (declare (type (or fixnum null) latter-width))
  (let ((first t))
    (mapc #'(lambda (entry)
	      (unless (value-zerop (cdr entry))
		(unless first
		  (princ line-feed-string output-stream))
		(print-value (cdr entry)
			     :width (if (or first (null latter-width))
					width latter-width)
			     :output-stream output-stream
			     :omit-commodity-p omit-commodity-p
			     :full-precision-p full-precision-p)
		(setf first nil)))
	  (get-amounts-map balance))
    (if first
	(format output-stream "~vD" width 0))))

(defmethod format-value ((rational rational) &key
			 omit-commodity-p
			 full-precision-p
			 (width nil)
			 latter-width
			 line-feed-string)
  (declare (ignore omit-commodity-p full-precision-p latter-width
		   line-feed-string))
  (with-output-to-string (out)
    (print-value rational :output-stream out :width width)))

(defmethod format-value ((amount amount) &key
			 (omit-commodity-p nil)
			 (full-precision-p nil)
			 (width nil)
			 latter-width
			 line-feed-string)
  (declare (ignore latter-width line-feed-string))
  (with-output-to-string (out)
    (print-value amount :output-stream out
		 :omit-commodity-p omit-commodity-p
		 :full-precision-p full-precision-p :width width)))

(defmethod format-value ((balance balance) &key
			 (omit-commodity-p nil)
			 (full-precision-p nil)
			 (width nil)
			 (latter-width nil)
			 (line-feed-string (format nil "~C" #\Newline)))
  (with-output-to-string (out)
    (print-value balance :output-stream out
		 :omit-commodity-p omit-commodity-p
		 :full-precision-p full-precision-p
		 :width width
		 :latter-width latter-width
		 :line-feed-string line-feed-string)))

;;;_ * COMMODITY

;;;_  - Parse commodity symbols

(declaim (inline symbol-char-invalid-p))
(defun symbol-char-invalid-p (c)
  (declare (type character c))
  (declare (optimize (speed 3) (safety 0)))
  (let ((code (char-code c)))
    (and (< code 256)
	 (aref (the (simple-array boolean (256))
		 *invalid-symbol-chars*) code))))

(declaim (inline symbol-name-needs-quoting-p))
(defun symbol-name-needs-quoting-p (name)
  "Return T if the given symbol NAME requires quoting."
  (declare (type string name))
  (declare (optimize (speed 3) (safety 0)))
  (loop for c across (the simple-string name) do
       (if (symbol-char-invalid-p c)
	   (return t))))

(define-condition commodity-error (error) 
  ((description :reader error-description :initarg :msg)
   (commodity :reader error-commodity :initarg :commodity :initform nil))
  (:report
   (lambda (condition stream)
     (format stream "~S" (error-description condition)))))

(defun read-commodity-symbol (in)
  "Parse a commodity symbol from the input stream IN.

  This is the correct entry point for creating a new commodity symbol.

  A commodity contain any character not found in *INVALID-SYMBOL-CHARS*.  To
include such characters in a symbol name---except for #\\\", which may never
appear in a symbol name---surround the commodity name with double quotes.  It
is an error if EOF is reached without reading the ending double quote.  If the
symbol name is not quoted, and an invalid character is reading, reading from
the stream stops and the invalid character is put back."
  (declare (type stream in))
  (declare (optimize (speed 3) (safety 0)))
  (let ((buf (make-string-output-stream))
	needs-quoting-p)
    (if (char= #\" (peek-char nil in))
	(progn
	  (read-char in)
	  (loop
	     for c = (read-char in nil) do
	     (if c
		 (if (char= #\" c)
		     (return)
		     (progn
		       (if (symbol-char-invalid-p c)
			   (setf needs-quoting-p t))
		       (write-char c buf)))
		 (error 'commodity-error
			:msg "Quoted commodity symbol lacks closing quote"))))
	(loop
	   for c = (read-char in nil) while c do
	   (if (symbol-char-invalid-p c)
	       (progn
		 (unread-char c in)
		 (return))
	       (write-char c buf))))

    (make-commodity-symbol :name (get-output-stream-string buf)
			   :needs-quoting-p needs-quoting-p)))

;;;_  * Access commodity details (across all commodity classes)

;; The commodity and annotated-commodity classes are the main interface class
;; for dealing with commodities themselves (which most people will never do).

(defmethod commodity-name ((null null) &optional no-annotation)
  (declare (ignore null no-annotation))
  "")

(defmethod commodity-name ((commodity commodity) &optional no-annotation)
  (if (slot-boundp commodity 'qualified-name)
      (if (and no-annotation (annotated-commodity-p commodity))
	  (commodity-name (get-referent commodity))
	  (commodity-qualified-name commodity))
      (let ((symbol (commodity-symbol commodity)))
	(if symbol
	    (commodity-symbol-name symbol)
	    ""))))

(defmethod commodity-name ((amount amount) &optional no-annotation)
  (commodity-name (amount-commodity amount) no-annotation))

(defmethod display-precision ((commodity commodity))
  (get-display-precision commodity))

(defmethod display-precision ((annotated-commodity annotated-commodity))
  (get-display-precision (get-referent annotated-commodity)))

(defmethod display-precision ((amount amount))
  (display-precision (amount-commodity amount)))

;;;_  + Function to sort commodities

(defun commodity-lessp (left right)
  "Return T if commodity LEFT should be sorted before RIGHT."
  (declare (type (or commodity annotated-commodity null) left))
  (declare (type (or commodity annotated-commodity null) right))
  (the boolean
    (block nil
      (if (and (null left) right)
	  (return t))
      (if (and left (null right))
	  (return nil))
      (if (and (null left) (null right))
	  (return t))

      (unless (commodity-equalp left right)
	(return (not (null (string-lessp (commodity-name left)
					 (commodity-name right))))))

      (let ((left-annotated-p (annotated-commodity-p left))
	    (right-annotated-p (annotated-commodity-p right)))
	(if (and (not left-annotated-p)
		 right-annotated-p)
	    (return t))
	(if (and left-annotated-p
		 (not right-annotated-p))
	    (return nil))
	(if (and (not left-annotated-p)
		 (not right-annotated-p))
	    (return t)))

      (let ((left-annotation (commodity-annotation left))
	    (right-annotation (commodity-annotation right)))

	(let ((left-price (annotation-price left-annotation))
	      (right-price (annotation-price right-annotation)))
	  (if (and (not left-price) right-price)
	      (return t))
	  (if (and left-price (not right-price))
	      (return nil))
	  (if (and left-price right-price)
	      (if (commodity-equal (amount-commodity left-price)
				   (amount-commodity right-price))
		  (return (value< left-price right-price))
		  ;; Since we have two different amounts, there's really no way
		  ;; to establish a true sorting order; we'll just do it based
		  ;; on the numerical values.
		  (return (< (amount-quantity left-price)
			     (amount-quantity right-price))))))

	(let ((left-date (annotation-date left-annotation))
	      (right-date (annotation-date right-annotation)))
	  (if (and (not left-date) right-date)
	      (return t))
	  (if (and left-date (not right-date))
	      (return nil))
	  (when (and left-date right-date)
	    (return (coerce (local-time:timestamp< left-date right-date)
			    'boolean))))

	(let ((left-tag (annotation-tag left-annotation))
	      (right-tag (annotation-tag right-annotation)))
	  (if (and (not left-tag) right-tag)
	      (return t))
	  (if (and left-tag (not right-tag))
	      (return nil))
	  (when (and left-tag right-tag)
	    (return (string-lessp left-tag right-tag)))))

      (return t))))

;;;_  + Current and historical market values (prices) for a commodity

(defstruct pricing-entry
  moment
  price)				; (:type amount)

(defun add-price (commodity price &optional fixed-time)
  (declare (type (or commodity annotated-commodity null) commodity))
  (declare (type amount price))
  (declare (type (or fixed-time null) fixed-time))
  (assert commodity)
  (if (annotated-commodity-p commodity)
      (setf commodity (get-referent commodity)))
  (let ((pricing-entry
	 (make-pricing-entry :moment (or fixed-time (local-time:now))
			     :price price))
	(history (or (get-price-history commodity)
		     (setf (get-price-history commodity) (rbt:nil-tree)))))
    (multiple-value-bind (new-root node-inserted-or-found item-already-in-p)
	(rbt:insert-item pricing-entry history :key #'pricing-entry-moment
			 :test-equal #'timestamp= :test #'timestamp<)
      (if item-already-in-p
	  (setf (pricing-entry-price (rbt:node-item node-inserted-or-found))
		price))
      (setf (get-price-history commodity) new-root))
    price))

(defun remove-price (commodity fixed-time)
  (declare (type (or commodity annotated-commodity null) commodity))
  (declare (type fixed-time fixed-time))
  (assert commodity)
  (if (annotated-commodity-p commodity)
      (setf commodity (get-referent commodity)))
  (when (get-price-history commodity)
    (multiple-value-bind (new-root node-deleted-p)
	(rbt:delete-item fixed-time (get-price-history commodity)
			 :key #'pricing-entry-moment
			 :test-equal #'timestamp= #'timestamp<)
      (if node-deleted-p
	  (values (setf (get-price-history commodity) new-root) t)
	  (values (get-price-history commodity) nil)))))

(defun find-nearest (it root &key (test #'<=) (key #'identity))
  "Find an item in the tree which is closest to IT according to TEST.
For the default, <=, this means no other item will be more less than IT in the
tree than the one found."
  (declare (type fixed-time it))
  (declare (type rbt:rbt-node root))
  (declare (type function test))
  (declare (type function key))
  (loop
     with p = root
     with last-found = nil
     finally (return (and last-found (rbt:node-item last-found)))
     while (not (rbt:rbt-null p)) do
     (if (funcall test (funcall (the function key)
				(rbt:node-item p)) it)
	 ;; If the current item meets the test, it may be the one we're
	 ;; looking for.  However, there might be something closer to the
	 ;; right -- though definitely not to the left.
	 (setf last-found p p (rbt:right p))
	 ;; If the current item does not meet the test, there might be a
	 ;; candidate to the left -- but definitely not the right.
	 (setf p (rbt:left p)))))

(defmethod market-value ((commodity commodity) &optional fixed-time)
  (declare (type (or commodity annotated-commodity null) commodity))
  (declare (type (or fixed-time null) fixed-time))
  ;; jww (2007-12-03): This algorithm needs extensive testing
  (let ((history (commodity-price-history commodity)))
    (when history
      (let ((pricing-entry
	     (if (null fixed-time)
		 (progn
		   (loop while (not (rbt:rbt-null (rbt:right history))) do
			(setf history (rbt:right history)))
		   (assert history)
		   (rbt:node-item history))
		 (find-nearest fixed-time history
			       :key #'pricing-entry-moment
			       :test #'timestamp<=))))
	(if pricing-entry
	    (values (pricing-entry-price pricing-entry)
		    (pricing-entry-moment pricing-entry))
	    (values nil nil))))))

(defmethod market-value ((annotated-commodity annotated-commodity)
			 &optional fixed-time)
  (market-value (get-referent annotated-commodity) fixed-time))

(defmethod market-value ((rational rational) &optional fixed-time)
  (values rational fixed-time))

(defmethod market-value ((amount amount) &optional fixed-time)
  (multiple-value-bind (value moment)
      (market-value (amount-commodity amount) fixed-time)
    (if value
	(values (multiply value amount) moment)
	(values amount fixed-time))))

(defmethod market-value ((balance balance) &optional fixed-time)
  (let ((market-balance 0)
	earliest latest)
    (mapc #'(lambda (cell)
	      (multiple-value-bind (value moment)
		  (market-value (cdr cell) fixed-time)
		(setf market-balance (add market-balance value))
		(if (or (null earliest)
			(timestamp< moment earliest))
		    (setf earliest moment))
		(if (or (null latest)
			(timestamp> moment latest))
		    (setf latest moment))))
	  (get-amounts-map balance))
    (values market-balance earliest latest)))

;;;_  + Exchange a commodity

(defun exchange-commodity (amount &key (total-cost nil) (per-unit-cost nil)
			   (moment nil) (tag nil))
  (declare (type amount amount))
  (declare (type (or value null) total-cost))
  (declare (type (or value null) per-unit-cost))
  (declare (type (or fixed-time null) moment))
  (declare (type (or string null) tag))

  (assert (or (and total-cost (not per-unit-cost))
	      (and per-unit-cost (not total-cost))))

  (let* ((commodity (amount-commodity amount))
	 (current-annotation
	  (and (annotated-commodity-p commodity)
	       (commodity-annotation commodity)))
	 (base-commodity (if (annotated-commodity-p commodity)
			     (get-referent commodity)
			     commodity))
	 (per-unit-cost (or per-unit-cost
			    (divide total-cost amount)))
	 (total-cost (or total-cost
			 (multiply per-unit-cost amount))))

    ;; Add a price history entry for this conversion if we know when it took
    ;; place
    (if (and moment (not (commodity-no-market-price-p base-commodity)))
	(add-price base-commodity per-unit-cost moment))

    ;; returns: ANNOTATED-AMOUNT TOTAL-COST BASIS-COST
    (values (annotate-commodity
	     amount
	     (make-commodity-annotation :price per-unit-cost
					:date  moment
					:tag   tag))
	    total-cost
	    (if current-annotation
		(multiply (annotation-price current-annotation) amount)
		total-cost))))

;;;_ * COMMODITY-POOL

;;;_  - Commodity creation and pool management

;; All commodities are allocated within a pool, which can be used to look them
;; up.

(defun reset-commodity-pool (&optional pool)
  (setf *default-commodity-pool* (or pool (make-commodity-pool))))

;;;_   : COMMODITY

(defmacro pushend (item the-list &optional final-cons)
  (let ((items-sym (gensym))
	(new-cons-sym (gensym)))
    `(let* ((,items-sym ,the-list)
	    (,new-cons-sym (list ,item)))
       (if ,items-sym
	   ,(if final-cons
		`(setf (cdr ,final-cons) ,new-cons-sym
		       ,final-cons ,new-cons-sym)
		`(nconc ,items-sym ,new-cons-sym))
	   (progn
	     ,`(setf ,the-list ,new-cons-sym)
	     ,(if final-cons
		  `(setf ,final-cons ,new-cons-sym))))
       (car ,new-cons-sym))))

(defun create-commodity (name &key (pool *default-commodity-pool*))
  "Create a COMMODITY after the symbol name found by parsing NAME.

  The NAME can be either a string or an input stream, or nil, in which case
the name is read from *STANDARD-INPUT*.

  The argument :pool specifies the commodity pool which will maintain this
commodity, and by which other code may access it again.  The resulting
COMMODITY object is returned."
  (declare (type (or string commodity-symbol) name))
  (declare (type commodity-pool pool))
  (let* ((symbol
	  (if (stringp name)
	      (read-commodity-symbol (make-string-input-stream name))
	      name))
	 (commodity (make-instance 'commodity :symbol symbol
					      :commodity-pool pool))
	 (symbol-name (commodity-symbol-name symbol)))

    (if (commodity-symbol-needs-quoting-p symbol)
	(setf (commodity-qualified-name commodity)
	      (concatenate 'string "\"" symbol-name "\"")))
      
    (setf (gethash symbol-name (commodity-pool-by-name-map pool))
	  commodity)))

(defun find-commodity (name &key (create-if-not-exists-p nil)
		       (pool *default-commodity-pool*))
  "Find a COMMODITY identifier by the symbol name found by parsing NAME.

  The NAME can be either a string or an input stream, or nil, in which case
the name is read from *STANDARD-INPUT*.

  The argument :POOL specifies the commodity pool which will maintain this
commodity, and by which other code may access it again.

  The argument :CREATE-IF-NOT-EXISTS-P indicates whether a new commodity
should be created if one cannot already be found.

  The return values are: COMMODITY or NIL, NEWLY-CREATED-P"
  (declare (type (or string commodity-symbol) name))
  (declare (type boolean create-if-not-exists-p))
  (declare (type commodity-pool pool))
  (multiple-value-bind (entry present-p)
      (gethash (if (stringp name)
		   name
		   (commodity-symbol-name name))
	       (commodity-pool-by-name-map pool))
    (if present-p
	(values entry nil)
	(if create-if-not-exists-p
	    (values (create-commodity name :pool pool) t)
	    (values nil nil)))))

;;;_   : ANNOTATED-COMMODITY

(defun make-qualified-name (commodity commodity-annotation)
  (declare (type commodity commodity))
  (declare (type commodity-annotation commodity-annotation))
  (if (and (annotation-price commodity-annotation)
	   (< (sign (annotation-price commodity-annotation)) 0))
      (error 'amount-error :msg "A commodity's price may not be negative"))

  (with-output-to-string (out)
    (princ (commodity-symbol-name (commodity-symbol commodity)) out)
    (format-commodity-annotation commodity-annotation :output-stream out)))

(defun create-annotated-commodity (commodity details qualified-name)
  "Create an ANNOTATED-COMMODITY which annotates COMMODITY.

The NAME can be either a string or a COMMODITY-SYMBOL."
  (declare (type commodity commodity))
  (declare (type commodity-annotation details))
  (declare (type string qualified-name))
  (let ((annotated-commodity
	 (make-instance 'annotated-commodity
			:referent commodity
			:annotation details
			:qualified-name qualified-name))
	(pool (commodity-pool commodity)))
    (setf (gethash qualified-name (commodity-pool-by-name-map pool))
	  annotated-commodity)))

(defun find-annotated-commodity (name-or-commodity details
				 &key (create-if-not-exists-p nil)
				 (pool *default-commodity-pool*))
  "Find an annotated commodity matching the commodity symbol NAME,
which may be a STRING or a COMMODITY-SYMBOL, and given set of commodity
DETAILS, of type COMMODITY-ANNOTATION.

  Returns two values: COMMODITY or NIL, NEWLY-CREATED-P"
  (declare (type (or string commodity-symbol commodity)
		 name-or-commodity))
  (declare (type commodity-annotation details))
  (declare (type boolean create-if-not-exists-p))
  (declare (type commodity-pool pool))
  (assert (not (commodity-annotation-empty-p details)))
  (let ((commodity
	 (if (typep name-or-commodity 'commodity)
	     name-or-commodity
	     (find-commodity name-or-commodity
			     :create-if-not-exists-p create-if-not-exists-p
			     :pool pool))))
    (if commodity
	(let* ((annotated-name (make-qualified-name commodity details))
	       (annotated-commodity
		(find-commodity annotated-name :pool pool)))
	  (if annotated-commodity
	      (values annotated-commodity nil)
	      (if create-if-not-exists-p
		  (values
		   (create-annotated-commodity commodity details
					       annotated-name)
		   t))))
	(values nil nil))))

;;;_ * ANNOTATED-COMMODITY

;;;_  - Read commodity annotation from a stream

(declaim (inline read-until))
(defun read-until (in char &optional error-message)
  (declare (type stream in))
  (declare (type character char))
  (declare (type (or string null) error-message))
  (declare (optimize (speed 3) (safety 0)))
  (with-output-to-string (text)
    (loop for c = (read-char in nil)
       while (if c (char/= char c)
		 (if error-message
		     (error 'amount-error :msg error-message))) do
       (write-char c text))))

(defun read-commodity-annotation (in)
  (declare (type stream in))
  (let ((annotation (make-commodity-annotation)))
    (loop
       for c = (peek-char t in nil) while c do
       (cond
	 ((char= c #\{)
	  (if (annotation-price annotation)
	      (error 'amount-error :msg
		     "Commodity annotation specifies more than one price"))
	  (read-char in)
	  (setf (annotation-price annotation)
		(parse-amount*
		 (read-until in #\} "Commodity price lacks closing brace"))))

	 ((char= c #\[)
	  (if (annotation-date annotation)
	      (error 'amount-error :msg
		     "Commodity annotation specifies more than one date"))
	  (read-char in)
	  (setf (annotation-date annotation)
		(strptime (read-until in #\]
				      "Commodity date lacks closing bracket"))))

	 ((char= c #\()
	  (if (annotation-tag annotation)
	      (error 'amount-error :msg
		     "Commodity annotation specifies more than one tag"))
	  (read-char in)
	  (setf (annotation-tag annotation)
		(read-until in #\) "Commodity tag lacks closing parenthesis")))

	 (t
	  (return))))

    annotation))

;;;_  - Format commodity annotation to a string

(defun format-commodity-annotation (annotation &key
				    (output-stream *standard-output*))
  "Return the canonical annotation string for ANNOTATION.

  A fully annotated commodity always follows the form:

    [-]SYMBOL <VALUE> {PRICE} [DATE] (TAG)
or
    <VALUE> SYMBOL {PRICE} [DATE] (TAG)

  If a particular annotation is not present, those sections is simply dropped
from the string, for example:

    <VALUE> SYMBOL {PRICE}"
  (declare (type commodity-annotation annotation))
  (declare (type stream output-stream))
  (format output-stream "~:[~; {~:*~A}~]~:[~; [~:*~A]~]~:[~; (~:*~A)~]"
	  (and (annotation-price annotation)
	       (format-value (annotation-price annotation)))
	  (and (annotation-date annotation)
	       (strftime (annotation-date annotation)))
	  (annotation-tag annotation)))

;;;_  + Annotate the commodity of a COMMODITY or AMOUNT

(defmethod annotate-commodity ((commodity commodity)
			       (details commodity-annotation))
  (find-annotated-commodity commodity details :create-if-not-exists-p t))

(defmethod annotate-commodity ((amount amount)
			       (details commodity-annotation))
  (let ((commodity (amount-commodity amount)))
    (unless commodity
      (error 'amount-error
	     :msg "Cannot annotate an amount which has no commodity"
	     :operands (list amount)))
    (let ((referent commodity))
      (if (annotated-commodity-p referent)
	  (setf referent (get-referent referent)))

      (let ((annotated-commodity
	     (find-annotated-commodity referent details
				       :create-if-not-exists-p t)))
	(let ((tmp (copy-amount amount)))
	  (setf (amount-commodity tmp) annotated-commodity)
	  tmp)))))

;;;_  + Access a commodity's annotation

(defmethod commodity-annotation ((commodity commodity))
  nil)
(defmethod commodity-annotation ((annotated-commodity annotated-commodity))
  (get-commodity-annotation annotated-commodity))
(defmethod commodity-annotation ((amount amount))
  ;; This calls the appropriate generic function
  (commodity-annotation (amount-commodity amount)))

;;;_  + Commodity annotation tests

(defmethod commodity-annotation-empty-p ((annotation commodity-annotation))
  (not (or (annotation-price annotation)
	   (annotation-date annotation)
	   (annotation-tag annotation))))

(defmethod commodity-annotation-equal ((a commodity-annotation)
				       (b commodity-annotation))
  (let ((price-a (annotation-price a))
	(price-b (annotation-price b))
	(date-a (annotation-date a))
	(date-b (annotation-date b))
	(tag-a (annotation-tag a))
	(tag-b (annotation-tag b)))
    (and (or (and (null price-a) (null price-b))
	     (and price-a price-b
		  (value= price-a price-b)))
	 (or (and (null date-a) (null date-b))
	     (and date-a date-b
		  (local-time:timestamp= date-a date-b)))
	 (or (and (null tag-a) (null tag-b))
	     (and tag-a tag-b
		  (string= tag-a tag-b))))))

;;;_  + Strip commodity annotations

;; annotated-commodity's are references to other commodities (which in turn
;; references a commodity) which carry along additional contextual information
;; relating to a point in time.

(defmethod strip-annotations ((commodity commodity)
			      &key keep-price keep-date keep-tag)
  (declare (ignore keep-price))
  (declare (ignore keep-date))
  (declare (ignore keep-tag))
  commodity)

(defmethod strip-annotations ((annotated-commodity annotated-commodity)
			      &key keep-price keep-date keep-tag)
  (declare (type boolean keep-price))
  (declare (type boolean keep-date))
  (declare (type boolean keep-tag))
  (let* ((annotation (commodity-annotation annotated-commodity))
	 (price (annotation-price annotation))
	 (date (annotation-date annotation))
	 (tag (annotation-tag annotation)))
    (cond
      ((and (not keep-price)
	    (not keep-date)
	    (not keep-tag))
       (get-referent annotated-commodity))
      ((and (or keep-price (null price))
	    (or keep-date (null date))
	    (or keep-tag (null tag)))
       annotated-commodity)
      (t
       (let ((annotation (make-commodity-annotation)))
	 (if keep-price
	     (setf (annotation-price annotation) price))
	 (if keep-date
	     (setf (annotation-date annotation) date))
	 (if keep-tag
	     (setf (annotation-tag annotation) tag))
	 (if (commodity-annotation-empty-p annotation)
	     (get-referent annotated-commodity)
	     (make-instance 'annotated-commodity
			    :referent (get-referent annotated-commodity)
			    :annotation annotation)))))))

(defmethod strip-annotations ((rational rational)
			      &key keep-price keep-date keep-tag)
  (declare (ignore keep-price))
  (declare (ignore keep-date))
  (declare (ignore keep-tag))
  rational)

(defmethod strip-annotations ((amount amount)
			      &key keep-price keep-date keep-tag)
  (declare (type boolean keep-price))
  (declare (type boolean keep-date))
  (declare (type boolean keep-tag))
  (if (and keep-price keep-date keep-tag)
      amount
      (let ((commodity (amount-commodity amount)))
	(if (annotated-commodity-p commodity)
	    (make-amount :commodity
			 (strip-annotations commodity
					    :keep-price keep-price
					    :keep-date keep-date
					    :keep-tag keep-tag)
			 :quantity (amount-quantity amount)
			 :keep-precision-p (amount-keep-precision-p amount))
	    amount))))

(defmethod strip-annotations ((balance balance)
			      &key keep-price keep-date keep-tag)
  (declare (type boolean keep-price))
  (declare (type boolean keep-date))
  (declare (type boolean keep-tag))
  (if (and keep-price keep-date keep-tag)
      balance
      (let ((stripped-balance (make-balance)))
	(mapc #'(lambda (entry)
		  (setf stripped-balance
			(add stripped-balance
			     (strip-annotations (cdr entry)
						:keep-price keep-price
						:keep-date  keep-date
						:keep-tag   keep-tag))))
	      (get-amounts-map balance))
	(if (= 1 (length (get-amounts-map stripped-balance)))
	    (cdar (get-amounts-map stripped-balance))
	    stripped-balance))))

(provide 'cambl)

;; cambl.lisp ends here
