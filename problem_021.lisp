;; Code for Project Euler Problem 21.
;;
;; Code author: Russell A. Edson
;; Date last modified: 27/07/2021

;; We define d(n) to be the sum of the proper divisors of n, which we
;; can find through a straightforward trial division algorithm:
(defun proper-divisors (n)
  "Return the proper divisors of the natural number N."
  (let ((divisors nil))
    (loop for i from 1 below n do
      (if (zerop (mod n i))
	  (setf divisors (cons i divisors))))
    divisors))

(proper-divisors 220)
;;=> (110 55 44 22 20 11 10 5 4 2 1)

(defun d (n)
  "Return the sum of all of the proper divisors of N."
  (reduce #'+ (proper-divisors n)))

(d 220)
;;=> 284

;; With d(n) defined, we can define a test for amicable numbers:
(defun amicable-numbers? (a b)
  "True if A and B are amicable numbers, False otherwise."
  (cond ((= a b) nil)
	((and (= (d a) b) (= a (d b))) t)
	(t nil)))

(amicable-numbers? 123 456)
;;=> NIL

(amicable-numbers? 220 284)
;;=> T

;; But actually, naively looping over every pair of numbers under 10000
;; and checking whether they are amicable with our test takes a long
;; time, and we'd end up doing a lot of redundant computation. Instead
;; it's quicker to just loop once like so:
(defvar amicable-below-10000
  (let ((pairs nil))
    (loop for a from 2 below 10000
	  with b
	  do
	     (setf b (d a))
	     (if (and (/= a b) (= (d b) a))
		 (setf pairs (cons (list a b) pairs))))
    pairs))

amicable-below-10000
;;=> ((6368 6232) (6232 6368) (5564 5020) (5020 5564) (2924 2620)
;;=>  (2620 2924) (1210 1184) (1184 1210) (284 220) (220 284))

;; Finally, we sum all of the amicable numbers up (noting that we've
;; double-counted them, so we want to remove the duplicates first).
(reduce #'+
	(remove-duplicates
	 (loop for pair in amicable-below-10000 append pair)))
;;=> 31626
