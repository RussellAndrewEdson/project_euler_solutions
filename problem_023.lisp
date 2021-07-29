;; Code for Project Euler Problem 23.
;;
;; Code author: Russell A. Edson
;; Date last modified: 29/07/2021

;; We want the sum of all of the positive integers that cannot be
;; written as the sum of two abundant numbers. We are given that
;; every integer greater than 28123 can be written as the sum of
;; two abundant numbers, so this serves as an upper bound: our
;; strategy will be to determine the abundant numbers between
;; 1 and 28123 and then compute the pairwise sums of each. Summing
;; whatever numbers are leftover will give us our answer.

;; We first code a function to find the proper divisors of a given
;; natural number:
(defun proper-divisors (n)
  "Return the proper divisors of the natural number N."
  (let ((divisors nil))
    (loop for i from 1 below n do
      (if (zerop (mod n i))
	  (setf divisors (cons i divisors))))
    divisors))

(proper-divisors (1- 28123))
;;=> (14061 9374 4687 654 327 258 218 129 109 86 43 6 3 2 1)

;; We can then define a test for abundant numbers:
(defun abundant? (n)
  "True if N is an abundant number, False if not."
  (< n (reduce #'+ (proper-divisors n))))

(abundant? 12)
;;=> T

;; Now we generate a list of all of the abundant numbers less
;; than 28123 (which will likely give us more than we need, but we're
;; being lazy).
(defvar abundant-numbers
  (loop for n from 1 upto 28123
	when (abundant? n) collect n))

(length abundant-numbers)
;;=> 6965

;; Now we loop over this list, exhaustively summing every pair of
;; numbers together to get the sums (discarding those sums that are
;; larger than 28123). This takes a while...
(defvar sums
  (let ((collected-sums nil))
    (loop for i from 0 below (length abundant-numbers) do
      (loop for j from i below (length abundant-numbers) do
	(let ((sum (+ (elt abundant-numbers i) (elt abundant-numbers j))))
	  (if (and (<= sum 28123)
		   (null (position sum collected-sums)))
	      (setf collected-sums
		    (cons sum collected-sums))))))
    collected-sums))
(setf sums (sort sums #'<))

(length sums)
;;=> 26667

;; Finally, to find the non-abundant sums, we can simply take the
;; difference between the sums list and a list of the numbers between
;; 1 and 28123, and then get their sum.
(defvar non-abundant-sums
  (set-difference (loop for n from 1 upto 28123 collecting n)
		  sums))

(length non-abundant-sums)
;;=> 1456

(reduce #'+ non-abundant-sums)
;;=> 4179871
