;; Code for Project Euler Problem 60.
;;
;; Code author: Russell A. Edson
;; Date last modified: 16/09/2021

;; Here we want to find the minimum-sum set of prime numbers where
;; each of the primes can be pairwise concatenated with one another
;; to produce another prime (e.g. 3 and 7 concatenate to 37 and 73,
;; both of which are prime numbers).
;;
;; The pain with this problem is that there are not really any
;; mathematical short-cuts we can take (that we currently know of).
;; We just need to throw computational power at this problem until
;; it buckles. We can be clever about the implementation to try to
;; squeeze out as much performance as possible, though.

;; First, we'll define some functions and infrastructure for testing
;; for primality. In this case we expect our list of primes to get
;; quite large and to be frequently checked, so it behooves us to
;; set up a reasonably efficient implementation here. In this case
;; we'll use the :cl-containers library and binary search trees
;; to store the primes (and non-primes) that we find so that we can
;; memoize our check for primality most efficiently.
(ql:quickload :cl-containers)
(defvar *primes* (make-instance 'cl-containers:binary-search-tree))
(defvar *non-primes* (make-instance 'cl-containers:binary-search-tree))

;; To further speed things up, we'll throw in some compile-time
;; optimisations and declare types here, too. Assuming/hoping that
;; we can find this set of five primes within the numbers between
;; 2 and 99,999 say, then we can restrict ourselves to the fixnums
;; without needing to rely on the arbitrary-precision integer
;; arithmetic.
(declaim (ftype (function (fixnum) boolean) is-prime?))
(defun is-prime? (n)
  "True if the given number N is prime, False if not."
  (declare (optimize (safety 0) (debug 0) (space 0) (speed 3)))
  (declare (type fixnum n))
  (the boolean (if (<= n 1)
		   nil
		   (cond ((cl-containers:find-item *primes* n) t)
			 ((cl-containers:find-item *non-primes* n) nil)
			 (t (let ((primep t))
			      (block trial-division
				(loop for m from 2 upto (isqrt n) do
				  (if (zerop (mod n m))
				      (progn
					(setf primep nil)
					(return-from trial-division)))))
			      (if primep
				  (cl-containers:insert-item *primes* n)
				  (cl-containers:insert-item *non-primes* n))
			      primep))))))

(time (is-prime? 999999999989))
;;=> Evaluation took:
;;=>   0.027 seconds of real time
;;=>   0.015625 seconds of total run time (0.015625 user, 0.000000 system)
;;=>   59.26% CPU
;;=>   60 lambdas converted
;;=>   71,002,709 processor cycles
;;=>   3,755,296 bytes consed
;;=>
;;=> T

(time (is-prime? 999999999989))
;;=> Evaluation took:
;;=>   0.000 seconds of real time
;;=>   0.000000 seconds of total run time (0.000000 user, 0.000000 system)
;;=>   100.00% CPU
;;=>   62,324 processor cycles
;;=>   0 bytes consed
;;=>
;;=> T

;; Next we can define some utility functions for concatenating numbers
;; together and checking for primality of the resulting pairwise
;; concatenations. Again, to speed things up a bit, we can declare
;; types for the input and output, and turn off type-safety and checking
;; in the optimisations.
(declaim (ftype (function (fixnum fixnum) fixnum) concatenate-numbers))
(defun concatenate-numbers (a b)
  "Concatenate the digits of A and B together to produce a new number."
  (declare (optimize (safety 0) (debug 0) (space 0) (speed 3)))
  (declare (type fixnum a b))
  (the fixnum (values (parse-integer (format nil "~d~d" a b)))))

(declaim (ftype (function (fixnum fixnum) boolean) concatenations-are-prime?))
(defun concatenations-are-prime? (a b)
  "True if the concatenations A|B and B|A are both prime numbers."
  (declare (optimize (safety 0) (debug 0) (space 0) (speed 3)))
  (declare (type fixnum a b))
  (the boolean (and (is-prime? (concatenate-numbers a b))
		    (is-prime? (concatenate-numbers b a)))))

(time (concatenations-are-prime? 7 109))
;;=> Evaluation took:
;;=>   0.001 seconds of real time
;;=>   0.000000 seconds of total run time (0.000000 user, 0.000000 system)
;;=>   0.00% CPU
;;=>   391,164 processor cycles
;;=>   0 bytes consed
;;=>
;;=> T

;; Now, we are primarily interested in prime numbers up to some upper
;; limit (let's say an upper bound of 50000 to start with). Our strategy
;; will be to find at least one 5-set of pairwise concatenating primes
;; in these limits, and then adjust the limits accordingly after that.
;; Our first loop will be to generate a list of all of the prime numbers
;; below 50,000.
(defvar upper-bound 50000)
(time
 (defvar primes-list
   (loop for n from 2 upto upper-bound
	 when (is-prime? n)
	   collect n)))
;;=> Evaluation took:
;;=>   86.461 seconds of real time
;;=>   86.453125 seconds of total run time (86.453125 user, 0.000000 system)
;;=>   99.99% CPU
;;=>   224,107,959,935 processor cycles
;;=>   12,051,728 bytes consed
;;=>
;;=> PRIMES-LIST

(length primes-list)
;;=> 5133

;; Now we loop on this list of primes, pairwise-concatenating each set
;; of 5 primes until we find the first set. This set likely won't be
;; the lowest sum set, but we can use its sum to make sure that we're
;; only checking concatenations for primes with a smaller (or equal)
;; sum. We'll then keep minimising by searching for smaller sum sets.
;; (This is still a time-expensive operation, particularly on a single
;; core, so perhaps make a jug of coffee or something while it runs.)
(time
 (defvar prime-5set
   (let ((total-primes (length primes-list))
	 (min-primes-sum (* 5 (car (last primes-list))))
	 (5sets nil))
     (loop
       for i from 1 below total-primes
       for p-i = (elt primes-list i)
       when (<= p-i min-primes-sum)
	 do
	    (loop
	      for j from (1+ i) below total-primes
	      for p-j = (elt primes-list j)
	      when (<= (+ p-i p-j) min-primes-sum)
		when (concatenations-are-prime? p-i p-j)
		  do
		     (loop
		       for k from (1+ j) below total-primes
		       for p-k = (elt primes-list k)
		       when (<= (+ p-i p-j p-k) min-primes-sum)
			 when (and (concatenations-are-prime? p-i p-k)
				   (concatenations-are-prime? p-j p-k))
			   do
			      (loop
				for m from (1+ k) below total-primes
				for p-m = (elt primes-list m)
				when (<= (+ p-i p-j p-k p-m) min-primes-sum)
				  when (and (concatenations-are-prime? p-i p-m)
					    (concatenations-are-prime? p-j p-m)
					    (concatenations-are-prime? p-k p-m))
				    do
				       (loop
					 for n from (1+ m) below total-primes
					 for p-n = (elt primes-list n)
					 when (<= (+ p-i p-j p-k p-m p-n)
						  min-primes-sum)
					   when (and (concatenations-are-prime?
						      p-i p-n)
						     (concatenations-are-prime?
						      p-j p-n)
						     (concatenations-are-prime?
						      p-k p-n)
						     (concatenations-are-prime?
						      p-m p-n))
					     do
						(setf min-primes-sum
						      (+ p-i p-j p-k p-m p-n))
						(push (list p-i p-j p-k p-m p-n)
						      5sets))))))
     5sets)))
;;=> Evaluation took:
;;=>   87518.304 seconds of real time
;;=>   87442.421875 seconds of total run time (87424.859375 user, 17.562500 system)
;;=>   [ Run times consist of 9.372 seconds GC time, and 87433.050 seconds non-GC time. ]
;;=>   99.91% CPU
;;=>   226,848,114,709,102 processor cycles
;;=>   31,848,497,216 bytes consed
;;=>
;;=> PRIME-5SET

prime-5set
;;=> ((13 5197 5701 6733 8389) (7 1237 2341 12409 18433)
;;=>  (3 5323 10357 29587 31231) (3 3119 9887 36263 48731))

;; After all of that, we'll have the minimum sum set of concatenating
;; primes as the first element in the list prime-5set. So we simply
;; sum those up to get our answer.
(reduce #'+ (car prime-5set))
;;=> 26033
