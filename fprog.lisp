;;; fprog.lisp -- Code to support functional programming in Common Lisp

(declaim (optimize (safety 0) (speed 3) (space 1)))

(defpackage :fprog
  (:use :cl)
  (:export apply-to-list
	   mapcar-if
	   remove-if
	   insert-if)
  (:shadow remove-if))

(in-package :fprog)

(defmacro apply-to-list (list predicate function &key (first-only nil)
			 (skip-to-next t) (lookahead t))
  "Given an input LIST, replicate as much of its structure as possible
while applying some kind of transform on its value.

  For every member of the list where PREDICATE returns T, FUNCTION is called
with the list whose CAR is that member; FUNCTION should return the list which
will be substituted at that point (this makes it possible to remove, change or
insert the matched cell).  If FIRST-ONLY is NIL, this is only done for every
cell that matches.  Note: Be very careful when setting FIRST-ONLY to T, for if
FUNCTION returns a new list that also matches PREDICATE, an infinitely
recursive loop can occur.  If SKIP-TO-NEXT is T, scanning the function
contains with the CDR of the value returned by FUNCTION, which can never lead
to infinite recursion.  If LOOKAHEAD is T, the list is prescanned to see if
PREDICATE matches, otherwise copying is done regardless of whether there is a
match in LIST or not; this mimics the behavior of CL:MAPCAR and is better for
very short lists.

  This function depends on the following contract with the caller:

  1. The input LIST is immutable after any call to APPLY-TO-LIST until
     the end of the program.

  2. The returned LIST is likewise immutable.

  The memory savings offered by this function comes at two costs: The first is
the subsequent immutability of the input data, and the second is an increase
in functional complexity.  Specifically, while CL:MAPCAR is O(N) for a given
list, FPROG:APPLY-TO-LIST -- when used to implement a sharing form of MAPCAR,
such as FPROG:MAPCAR-IF -- has complexity O(N) in the best case, and O(2N) in
the worst case when LOOKAHEAD is T, otherwise it is also O(N) (where an
element to be substituted occurs at the very end of the list).

  Now, the cost of speed in the worst case can lead to dramatic improvements
in memory usage in the average case, with an attendant speed advantage.  Take
the case of a list which is 500 elements long.  In my environment, here are
the timings for using MAPCAR to generate a new list from an old one where only
one cons cell needs to be changed.  These times were determined by calling the
same code repeatedly 1,000,000 times (that code is near the end of this file,
in the function TIMING-TESTS):

  Evaluation took:
    8.367 seconds of real time
    7.931782 seconds of user run time
    0.342331 seconds of system run time
    [Run times include 2.679 seconds GC run time.]
    0 calls to %EVAL
    0 page faults and
    4,024,029,056 bytes consed.

That's 4 gigabytes of memory, probably to be expected.  The only reason this
doesn't blow the heap is because all of the intermediate results are being
thrown away, making a lot of the cons'ing \"free\".  If the results are kept,
the MAPCAR solution becomes impossible without dramatically increasing Lisp's
heap size.

  The memory and time costs of using MAPCAR in this example are constant no
matter whether the cons cell is substituted at the beginning, middle or end of
the 500 element list.  To compare, here are the time and memory statistics
from FPROG:MAPCAR-IF for the same data, in all three cases (best, average,
worst):

  Evaluation took:
    3.478 seconds of real time
    3.474324 seconds of user run time
    0.003887 seconds of system run time
    [Run times include 0.026 seconds GC run time.]
    0 calls to %EVAL
    0 page faults and
    40,007,952 bytes consed.

In the best case, memory usage is reduced by two orders of magnitude, with an
appreciable boost in speed.  If the results of this case are saved (using
COLLECT in the LOOP instead of DO), the speed savings can become dramatic.
Note also that except for the immutability constraints, the results from the
two different approaches are EQUAL.

  Evaluation took:
    7.495 seconds of real time
    7.272269 seconds of user run time
    0.173947 seconds of system run time
    [Run times include 1.416 seconds GC run time.]
    0 calls to %EVAL
    0 page faults and
    2,032,015,008 bytes consed.

In the average case (middle of the list), memory usage is cut in half, while
runtime speed is still faster.  The cons'ing of CL:MAPCAR also gets more
expensive the more the results are kept, so this trivial speed tests -- where
no results are saved -- is not exactly fair between the two.  But even still
FPROG:MAPCAR-IF is doing well.

  Evaluation took:
    11.343 seconds of real time
    10.969349 seconds of user run time
    0.327477 seconds of system run time
    [Run times include 2.679 seconds GC run time.]
    0 calls to %EVAL
    0 page faults and
    4,024,030,568 bytes consed.

Finally, the pathological case, where MAPCAR-IF degenerates into an exact
duplicate of MAPCAR.  Memory use is the same, but speed is much slower because
the call to MEMBER-IF is searching the entire list before we decide that all
of it needs duplication.


  Below are possible FUNCTION arguments relating to the three canonical uses
of APPLY-TO-LIST, plus the names of provided convenience functions which offer
simple implementations of them:

  1. Modify an existing element:  #'(lambda (list) (cons new-cons (cdr list)))

     Use: (FPROG:MAPCAR-IF predicate function list)

  2. Remove an existing element:  #'(lambda (list) (cdr list))

     Use: (FPROG:REMOVE-IF predicate list)

  3. Add a new element:           #'(lambda (list) (cons new-cons list))

     Use: (FPROG:INSERT-IF predicate function list)

Note: When removing elements, SKIP-TO-NEXT must be set to NIL, otherwise
further matching elements might be missed.  For modifying and adding elements,
SKIP-TO-NEXT is likely to always be T (the default).


  The functionality offered by APPLY-TO-LIST is that every cons cell from the
original LIST, after the last matching member, is shared entirely.  This is
quite different from COPY-LIST, which creates new cons cells for every
position -- even those that do not require a unique structure.  For example,
consider the following list:

  (defparameter *alist* '((a . 1) (b . 2) (e . 3) (f . 6) (g . 7)))

  The idea is to return another version of this immutable list, while sharing
as much structure as possible -- because the return value is also considered
immutable.  The following function call achieves this, using the Modify
pattern from above:

  (apply-to-list *alist* #'(lambda (member) (eq 'e (car member)))
                         #'(lambda (list) (cons (cons (caar list) 5)
                                           (cdr list))))
    => '((a . 1) (b . 2) (e . 5) (f . 6) (g . 7))

  In the returned list, 15 atoms are shared with the original, while one new
cons cell and one new atom are created:

  1, 2, 3:         (a . 1)
  4, 5, 6:         (b . 2)
  7:               e
  8, 9, 10 11:     ((f . 6) ...)
  12, 13, 14, 15:  ((g . 7))

  The usual practice of calling MAPCAR and changing the incorrect element
would have result in sharing only 13 atoms.  That code might have looked like
this:

  (mapcar #'(lambda (cell)
              (if (eq 'e (car cell))
                  (cons (car cell) 5)
                  cell)) *alist*)

Further, while a discrepancy of 2 cons cells may not seem like much in this
example, the difference increases by one for every cell beyond the cell that
matches.  Thus, if the input list had contained 100 cells beyond (e . 3), the
difference would have been 102 cells, and not merely 2.

  Finally, in our example exactly 4 new cons cells and 1 new atom were created
as a result of the call:

  1: ((a . 1) ...)
  2: ((b . 2) ...)
  3: ((e . 5) ...)
  4: (e . 5)
  5: 5

This is the minimum amount of new information required to represent a new
structure where the only change is that 'e' is paired with 5 instead of 3.


  The idea of APPLY-TO-LIST is to support efficient functional programming,
whereat immutable outputs are derived from immutable inputs by efficiently
sharing as much structure as possible -- resulting in the least new memory
allocated.  In cases where no references are held, this offers little gain
over advanced generational garbage collection (such as lists passed within a
recursive function); but where the results are held over the longer term, such
as a series of computed values stored in a results list, the savings of this
function can become substantial.  It was exactly this kind of sitation that
motivated APPLY-TO-LIST: it made it possible to reduce overall memory
consumption by a factor of 20, without introducing any additional complexity
into the calling code."
  (let ((func-sym (gensym))
	(pred-sym (gensym))
	(new-list-sym (gensym))
	(list-sym (gensym))
	(outer-sym (gensym))
	(last-cell-sym (gensym)))
    `(block ,outer-sym
       (let* ((,list-sym ,list)
	      (,new-list-sym ,list-sym)
	      (,pred-sym (the function ,predicate))
	      (,func-sym (the function ,function))
	      ,last-cell-sym)
	 (loop while ,list-sym do
	    ;; Scan ahead to see if it's even worth copying structure; if we
	    ;; find a match, we must copy cells (like copy-list) until we
	    ;; reach the match.  If there is no match, no copying is needed
	    ;; and the rest can be shared just as it is.
	    (,@(if lookahead
		   `(let (next-match)
		      (unless (setf next-match
				    (member-if ,pred-sym ,list-sym))
			(return)))
		   '(progn))
	       (loop while (and ,list-sym
				(not ,(if lookahead
					  `(eq next-match ,list-sym)
					  `(funcall ,pred-sym
						    (car ,list-sym))))) do
		    (setf ,last-cell-sym
			  (if ,last-cell-sym
			      (setf (cdr ,last-cell-sym) (list (car ,list-sym)))
			      (setf ,new-list-sym (list (car ,list-sym))))
			  ,list-sym (cdr ,list-sym)))
	       (unless ,list-sym (return))
	       ,(if first-only
		    `(if ,last-cell-sym
			 (progn
			   (setf (cdr ,last-cell-sym)
				 (funcall ,func-sym ,list-sym))
			   (return))
			 (return-from ,outer-sym
			   (funcall ,func-sym ,list-sym)))
		    `(let ((remainder (funcall ,func-sym ,list-sym)))
		       (setf ,last-cell-sym
			     (if ,last-cell-sym
				 (setf (cdr ,last-cell-sym) remainder)
				 (setf ,new-list-sym remainder))
			     ,list-sym ,(if skip-to-next
					    '(cdr remainder)
					    'remainder))))))
	 ,new-list-sym))))

(defun mapcar-if (predicate function list)
  (apply-to-list list predicate
		 #'(lambda (cell)
		     (cons (funcall (the function function)
				    (car cell))
			   (cdr cell)))))

(defun remove-if (predicate list)
  (apply-to-list list predicate #'(lambda (cell) (cdr cell))
		 :skip-to-next nil))

(defun insert-if (predicate function list)
  (apply-to-list list predicate
		 #'(lambda (cell)
		     (cons (funcall (the function function)
				    (car cell))
			   cell))))

(defun timing-tests (&optional (loop-count 100000))
  (declare (type fixnum loop-count))
  (let* ((first-foo (intern "FOO-1"))
	 (middle-foo (intern "FOO-250"))
	 (last-foo (intern "FOO-499"))
	 (short-list `((,first-foo . 1)
		       (,(intern "FOO-100") . 100)
		       (,middle-foo . 250)
		       (,(intern "FOO-400") . 400)
		       (,last-foo . 499)))
	 (long-list
	  (loop for i from 0 to 500
	     collect (cons (intern (format nil "FOO-~D" i)) i))))

    ;; Speed test when the item to be replaced is at the beginning of the
    ;; list.
    (format t "CL:MAPCAR BEGINNING SHORT-LIST:~%")
    (time (loop repeat loop-count do
	       (cl:mapcar #'(lambda (cell)
			      (if (eq (car cell) first-foo)
				  (cons (car cell) 1000)
				  cell))
			  short-list)))

    (format t "~%CL:MAPCAR BEGINNING LONG-LIST:~%")
    (time (loop repeat loop-count do
	       (cl:mapcar #'(lambda (cell)
			      (if (eq (car cell) first-foo)
				  (cons (car cell) 1000)
				  cell))
			  long-list)))

    (format t "~%FPROG:MAPCAR-IF BEGINNING SHORT-LIST:~%")
    (time (loop repeat loop-count do
	       (mapcar-if #'(lambda (cell) (eq (car cell) first-foo))
			  #'(lambda (cell) (cons (car cell) 1000))
			  short-list)))

    (format t "~%FPROG:MAPCAR-IF BEGINNING LONG-LIST:~%")
    (time (loop repeat loop-count do
	       (mapcar-if #'(lambda (cell) (eq (car cell) first-foo))
			  #'(lambda (cell) (cons (car cell) 1000))
			  long-list)))

    ;; Speed test when the item to be replaced is in the middle of the list.
    (format t "~%CL:MAPCAR MIDDLE SHORT-LIST:~%")
    (time (loop repeat loop-count do
	       (cl:mapcar #'(lambda (cell)
			      (if (eq (car cell) middle-foo)
				  (cons (car cell) 1000)
				  cell))
			  short-list)))

    (format t "~%CL:MAPCAR MIDDLE LONG-LIST:~%")
    (time (loop repeat loop-count do
	       (cl:mapcar #'(lambda (cell)
			      (if (eq (car cell) middle-foo)
				  (cons (car cell) 1000)
				  cell))
			  long-list)))

    (format t "~%FPROG:MAPCAR-IF MIDDLE SHORT-LIST:~%")
    (time (loop repeat loop-count do
	       (mapcar-if #'(lambda (cell) (eq (car cell) middle-foo))
			  #'(lambda (cell) (cons (car cell) 1000))
			  short-list)))

    (format t "~%FPROG:MAPCAR-IF MIDDLE LONG-LIST:~%")
    (time (loop repeat loop-count do
	       (mapcar-if #'(lambda (cell) (eq (car cell) middle-foo))
			  #'(lambda (cell) (cons (car cell) 1000))
			  long-list)))

    
    ;; Speed test when the item to be replaced is at the end of the list.
    (format t "~%CL:MAPCAR END SHORT-LIST:~%")
    (time (loop repeat loop-count do
	       (cl:mapcar #'(lambda (cell)
			      (if (eq (car cell) last-foo)
				  (cons (car cell) 1000)
				  cell))
			  short-list)))

    (format t "~%CL:MAPCAR END LONG-LIST:~%")
    (time (loop repeat loop-count do
	       (cl:mapcar #'(lambda (cell)
			      (if (eq (car cell) last-foo)
				  (cons (car cell) 1000)
				  cell))
			  long-list)))

    (format t "~%FPROG:MAPCAR-IF END SHORT-LIST:~%")
    (time (loop repeat loop-count do
	       (mapcar-if #'(lambda (cell) (eq (car cell) last-foo))
			  #'(lambda (cell) (cons (car cell) 1000))
			  short-list)))

    (format t "~%FPROG:MAPCAR-IF END LONG-LIST:~%")
    (time (loop repeat loop-count do
	       (mapcar-if #'(lambda (cell) (eq (car cell) last-foo))
			  #'(lambda (cell) (cons (car cell) 1000))
			  long-list)))
    (values)))

(provide 'fprog)

;;; fprog.lisp ends here
