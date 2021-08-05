;; Code for Project Euler Problem 30.
;;
;; Code author: Russell A. Edson
;; Date last modified: 05/08/2021

;; We seek the sum of all the numbers that can be written as the sum
;; of fifth powers of their digits, e.g.
;;   abcd = a^5 + b^5 + c^5 + d^5.
;; We can loop over these in a straightforward manner if we can deduce
;; a stopping point. We have no a priori restriction on digits;
;; however, we do have limits on the (arbitrarily long) right-hand side.
;; Consider the following:
;;   9^5 = 59049
;;   9^5 + 9^5 = 118098
;;   9^5 + 9^5 + 9^5 = 177147
;;   9^5 + 9^5 + 9^5 + 9^5 = 236196
;;   9^5 + 9^5 + 9^5 + 9^5 + 9^5 = 295245
;;   9^5 + 9^5 + 9^5 + 9^5 + 9^5 + 9^5 = 354294
;;
;; So our upper limit is 6 digits; the maximum possible right-hand side
;; for six digits is 354294, a six-digit number. It is impossible to
;; write numbers larger than this (e.g. seven- or eight- digit numbers
;; for example) as a sum of fifth powers of their digits because the
;; right-hand side will never be large enough.

;; We code a simple function to extract the digits of a given number:
(defun digits (number)
  "Return the set of digits in the given NUMBER."
  (mapcar #'digit-char-p (coerce (write-to-string number) 'list)))

;; And then the loop, now that we know the stopping point, is
;; very straightforward (if a little overkill).
(defvar sum-numbers
  (loop for n from 2 upto 999999
	when (= n (reduce #'+ (mapcar (lambda (base) (expt base 5))
				      (digits n))))
	  collect n))
sum-numbers
;;=> (4150 4151 54748 92727 93084 194979)

(reduce #'+ sum-numbers)
;;=> 443839
