;; Code for Project Euler Problem 37.
;;
;; Code author: Russell A. Edson
;; Date last modified: 16/08/2021

;; In this problem we want to find the truncatable primes: that is,
;; primes in which you can truncate from left-to-right and
;; right-to-left such that you keep getting a prime number as you
;; go, e.g. like 3797:
;;   3797         3797
;;    797         379
;;     97         37
;;      7         3
;; are all primes. We are given that there are only eleven of these
;; numbers (2, 3, 5, 7 don't count as truncatable primes).

;; To start with, we'll code a simple check for prime numbers using
;; a classic trial division algorithm:
(defun is-prime? (n)
  "True if the given number N is prime, False if not."
  (if (= n 1)
      nil
      (let ((primep t))
	(loop for m from 2 upto (floor (sqrt n)) do
	  (if (zerop (mod n m))
	      (setf primep nil)))
	primep)))

(is-prime? 3797)
;;=> T

;; Rather than do any exhaustive search of the problem space
;; (for which we're not even sure what the bounds are), we'll
;; instead build up the solution piece by piece.
;;
;; We know there are eleven such truncatable primes. We're
;; given 3797 for free, and actually we're also given 797 too:
;;   797     797
;;    97     79
;;     7     7
;; since we can show that 79 is prime:
(is-prime? 79)
;;=> T

(defvar truncatable-primes (list 3797 797))

;; Now observe the following. The leftmost and rightmost digits
;; of our truncatable primes can only be 2, 3, 5, or 7, since we
;; must inevitably end at one of those numbers when we truncate.
;; Further, 2 cannot be a rightmost digit: in such a case, we'd
;; end up with a multi-digit number ending in a 2, which
;; would necessarily be divisible by 2 and hence not be prime.
;; The digits 1 and 9 can appear as middle digits in the number,
;; but not as leftmost or rightmost digits (since neither 1 nor 9
;; are prime numbers). No other digits can appear anywhere, since
;; they would give us an even number at some point in the truncation.
;;
;; So this gives us our recipe for building such truncatable primes:
;; rather than loop through some nebulously-defined problem space,
;; we'll build them up from these rules and atoms, and stop when we
;; have the eleven truncatable primes that we know we're to find.
(defvar leftmost (list 2 3 5 7))
(defvar rightmost (list 3 5 7))
(defvar middle (list 1 3 5 7 9))

;; We'll define some functions for manipulating digits of numbers:
(defun digits (number)
  "Return the set of digits in the given NUMBER."
  (mapcar #'digit-char-p (coerce (write-to-string number) 'list)))

(defun combine-digits (digits)
  "Return the number we get by concatenating the given DIGITS together."
  (values (parse-integer (format nil "~{~d~}" digits))))

(digits 12345)
;;=> (1 2 3 4 5)

(combine-digits (cons 4 (list 3 2 1)))
;;=> 4321

;; Now we'll use these to define a function to check for a
;; truncatable prime:
(defun is-truncatable-prime? (n)
  "True if N is a truncatable prime, False if not."
  (if (is-prime? n)
      (let ((left-truncate n)
	    (right-truncate n)
	    (truncatable-prime t))
	(loop while (and truncatable-prime
			 (> (length (digits left-truncate)) 1)
			 (> (length (digits right-truncate)) 1))
	      do
		 (setf left-truncate
		       (combine-digits (cdr (digits left-truncate))))
		 (setf right-truncate
		       (combine-digits (butlast (digits right-truncate))))
		 (setf truncatable-prime
		       (and (is-prime? left-truncate)
			    (is-prime? right-truncate))))
	truncatable-prime)
      nil))

(is-truncatable-prime? 3797)
;;=> T

(is-truncatable-prime? 797)
;;=> T

;; So our strategy will be as follows: we'll take each of the leftmost
;; digits, combine them with each of the rightmost digits, and then add
;; in middle digits. Observe that we've already found a couple of
;; 2-digit truncatable primes without needing any middle digits yet:
(defvar candidates
  (loop for left in leftmost
	append
	(loop for right in rightmost
	      collect (combine-digits (list left right)))))

(mapcar (lambda (number) (list number (is-truncatable-prime? number)))
	candidates)
;;=> ((23 T) (25 NIL) (27 NIL) (33 NIL) (35 NIL) (37 T) (53 T)
;;=>  (55 NIL) (57 NIL) (73 T) (75 NIL) (77 NIL))

(setf truncatable-primes
      (append truncatable-primes (list 23 37 53 73)))

truncatable-primes
;;=> (3797 797 23 37 53 73)

;; So we've found 6 truncatable primes: 5 more left. So now we add in
;; each of the middle digits to every one of the 2-digit candidates
;; to see if we've generated new truncatable primes (we include even
;; those 2-digit numbers that were not truncatable primes, since e.g.
;; 77 is not prime, but inserting 9 gives 797 which is a truncatable
;; prime):
(defun add-in-middles ()
  "Expand the candidates list by adding in middle digits."
  (setf candidates
	(loop for number in candidates
	      append
	      (loop for mid in middle
		    collect
		    (combine-digits
		     (append (list (car (digits number))
				   mid)
			     (cdr (digits number))))))))

(add-in-middles)
(mapcar (lambda (number) (list number (is-truncatable-prime? number)))
	candidates)
;;=> ((213 NIL) (233 NIL) (253 NIL) (273 NIL) (293 NIL) (215 NIL)
;;=>  (235 NIL) (255 NIL) (275 NIL) (295 NIL) (217 NIL) (237 NIL)
;;=>  (257 NIL) (277 NIL) (297 NIL) (313 T) (333 NIL) (353 NIL) (373 T)
;;=>  (393 NIL) (315 NIL) (335 NIL) (355 NIL) (375 NIL) (395 NIL)
;;=>  (317 T) (337 NIL) (357 NIL) (377 NIL) (397 NIL) (513 NIL)
;;=>  (533 NIL) (553 NIL) (573 NIL) (593 NIL) (515 NIL) (535 NIL)
;;=>  (555 NIL) (575 NIL) (595 NIL) (517 NIL) (537 NIL) (557 NIL)
;;=>  (577 NIL) (597 NIL) (713 NIL) (733 NIL) (753 NIL) (773 NIL)
;;=>  (793 NIL) (715 NIL) (735 NIL) (755 NIL) (775 NIL) (795 NIL)
;;=>  (717 NIL) (737 NIL) (757 NIL) (777 NIL) (797 T))

;; So we've found some new truncatable primes here too:
(setf truncatable-primes
      (append truncatable-primes (list 313 373 317)))

truncatable-primes
;;=> (3797 797 23 37 53 73 313 373 317)

;; Just two left to find, so we'll repeat the process again:
(add-in-middles)
(remove-if-not #'is-truncatable-prime? candidates)
;;=> (3137 3797)

(setf truncatable-primes (cons 3137 truncatable-primes))

truncatable-primes
;;=> (3137 3797 797 23 37 53 73 313 373 317)

;; One more truncatable prime left to find, so we go again:
(add-in-middles)
(remove-if-not #'is-truncatable-prime? candidates)
;;=> NIL

;; We didn't find any that time, so once more:
(add-in-middles)
(remove-if-not #'is-truncatable-prime? candidates)
;;=> (739397)

;; So we've found our 11th and last truncatable prime. We can now
;; sum them up to get the desired result.
(setf truncatable-primes (cons 739397 truncatable-primes))

truncatable-primes
;;=> (739397 3137 3797 797 23 37 53 73 313 373 317)

(reduce #'+ truncatable-primes)
;;=> 748317
