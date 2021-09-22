;; Code for Project Euler Problem 63.
;;
;; Code author: Russell A. Edson
;; Date last modified: 22/09/2021

;; Here we want to count the number of n-digit positive integers that
;; are also nth powers. We'll do this in a straightforward way by
;; looping over a^n for appropriately-bounded a and n, and counting
;; the numbers as we go.

;; First, we'll need a function to count the digits:
(defun digits (number)
  "Return the set of digits in the given NUMBER."
  (mapcar #'digit-char-p (coerce (write-to-string number) 'list)))

(defun count-digits (number)
  "Return the total number of digits in NUMBER."
  (length (digits number)))

;; And next, we'll reason about some appropriate choices of
;; bounds for a and n. Note that in general, 10^n for positive
;; n will have (n+1) digits:
;;   10^1 = 10
;;   10^2 = 100
;;   10^3 = 1000
;;    ...
;; So in fact we only need to loop a between 1 and 9, since when
;; a is greater than or equal to 10, we will always have too many
;; digits in the result.
;;
;; n is trickier to reason about, but in the 'opposite' way we can
;; notice that as soon as the number of digits in the result
;; falls below the exponent, we can never catch up (since we're
;; limited to our base, a, being between 1 and 9). For example:
(count-digits (expt 9 20))
;;=> 20
(count-digits (expt 9 25))
;;=> 24
(count-digits (expt 9 50))
;;=> 48
(count-digits (expt 8 25))
;;=> 23
(count-digits (expt 8 50))
;;=> 46

;; That is, somewhere between 20 and 50, the exponent exceeds the
;; digit count and stays that way as n increases. So even being
;; conservative we can take 50 as an upper bound for our n loop and
;; be sure that we've got an exhaustive coverage of potential solutions.
;;
;; So: that's it. We simply implement this loop, counting up the
;; numbers that are nth powers with n digits.
(loop for a from 1 upto 9
      summing
      (loop for n from 1 upto 50
	    when (= (count-digits (expt a n)) n)
	      count n))
;;=> 49
