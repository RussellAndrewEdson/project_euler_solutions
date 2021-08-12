;; Code for Project Euler Problem 35.
;;
;; Code author: Russell A. Edson
;; Date last modified: 12/08/2021

;; We seek all of the 'circular' primes under 1000000, where the
;; circular primes are those for which all rotations of the digits
;; are themselves prime, e.g. 197, 971, 719 are all prime so
;; 197 (and 971, and 719) is a circular prime.

;; Note that in this problem we will likely be testing the same numbers
;; for primality multiple times during the check for circular
;; primes, and so it behooves us to memoize (i.e. keep track of the
;; different prime numbers that we've already found). So we can define
;; a memoized prime-checking function as follows using a closure:
(defvar memoized-primes
  (let ((primes-list nil)
	(not-primes-list nil))
    (lambda (n)
      (cond ((member n primes-list) t)
	    ((member n not-primes-list) nil)
	    (t (if (<= n 1)
		   nil
		   (let ((primep t))
		     (loop for m from 2 upto (floor (sqrt n)) do
		       (if (zerop (mod n m))
			   (setf primep nil)))
		     (if primep
			 (cons n primes-list)
			 (cons n not-primes-list))
		     primep)))))))

(defun is-prime? (n)
  "True if N is a prime number."
  (funcall memoized-primes n))

(is-prime? 2)
;;=> T

(is-prime? 17)
;;=> T

(is-prime? 16)
;;=> NIL

;; Now we can exploit the memoization of our is-prime? function and
;; code a straightforward yet efficient check for circular primes.
;; We'll do this by building up our check out of smaller utility
;; functions in the usual way (and defer to the alexandria library
;; for the rotate function code):
(ql:quickload '(:alexandria))

(defun digits (number)
  "Return the set of digits in the given NUMBER."
  (mapcar #'digit-char-p (coerce (write-to-string number) 'list)))

(defun combine-digits (digits)
  "Return the number obtained by combining DIGITS together."
  (values (parse-integer (format nil "~{~d~}" digits))))

(defun all-rotations (number)
  "Return a list of all digit rotations of the given NUMBER."
  (let ((number-digits (digits number)))
    (loop for shift from 0 upto (1- (length number-digits))
	  collect (combine-digits (alexandria:rotate (copy-list number-digits)
						     shift)))))

(all-rotations 12345)
;;=> (12345 51234 45123 34512 23451)

(defun is-circular-prime? (n)
  "True if N is a circular prime number."
  (reduce (lambda (a b) (and a b))
	  (mapcar #'is-prime? (all-rotations n))))

(is-circular-prime? 197)
;;=> T

;; Finally, we loop up to 1000000, collecting the circular primes.
(time
 (defvar circular-primes
   (loop for n from 2 below 1000000
	 when (is-circular-prime? n)
	   collect n)))
;;=> Evaluation took:
;;=>   152.336 seconds of real time
;;=>   117.687500 seconds of total run time (117.031250 user, 0.656250 system)
;;=>   [ Run times consist of 0.580 seconds GC time, and 117.108 seconds non-GC time. ]
;;=>   77.26% CPU
;;=>   394,857,271,883 processor cycles
;;=>   4,107,059,296 bytes consed
;;=>
;;=> CIRCULAR-PRIMES

circular-primes
;;=> (2 3 5 7 11 13 17 31 37 71 73 79 97 113 131 197 199 311 337
;;=>  373 719 733 919 971 991 1193 1931 3119 3779 7793 7937 9311
;;=>  9377 11939 19391 19937 37199 39119 71993 91193 93719 93911
;;=>  99371 193939 199933 319993 331999 391939 393919 919393
;;=>  933199 939193 939391 993319 999331)

(length circular-primes)
;;=> 55
