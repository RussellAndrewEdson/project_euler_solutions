;; Code for Project Euler Problem 97.
;;
;; Code author: Russell A. Edson
;; Date last modified: 23/09/2021

;; Here we want to find the last 10 digits of the large non-Mersenne
;; prime number 28433 x 2^7830457 + 1. This looks like a complex task
;; at the outset, but we can solve it in a jiffy using some modulo
;; arithmetic.

;; Ignore the + 1 for the moment and let's focus on the 'simpler'
;; problem of finding the last 10 digits of the product
;;   28433 x 2^7830457.
;; (We can trivially add the + 1 back at the end once we know the
;; last 10 digits of the product, anyway.)
;;
;; Finding the last 10 digits is tantamount to finding
;;   (28433 x 2^7830457) mod 10^10,
;; and products in modulo arithmetic are particularly convenient
;; because we have that
;;   (a * b) mod n = (a mod n) * (b mod n).
;;
;; So we can simplify the modulus of the product,
;;   (28433 x 2^7830457) mod 10^10
;;     = (28433 mod 10^10) * (2^7830457 mod 10^10).
;; This expression is much easier to compute. Now 2^7830457 is still
;; quite large, but it's a straightforward doubling (or leftwise bit
;; shifting) operation that takes no time at all on a modern computer,
;; and if we take the modulus at each step then we never have a number
;; that exceeds 10 digits so we're efficiently using space too. Then
;; we multiply by (28433 mod 10^10), which is just 28433 because it
;; has less than 10 digits.
;;
;; So, remembering to add back the one at the end, the last ten digits
;; of 28433 x 2^7830457 + 1 are computed to be:
(time
 (let ((result 1))
   (loop for i from 1 upto 7830457 do
     (setf result (mod (* 2 result) (expt 10 10))))
   (mod (1+ (* 28433 result)) (expt 10 10))))
;;=> Evaluation took:
;;=>   0.134 seconds of real time
;;=>   0.125000 seconds of total run time (0.125000 user, 0.000000 system)
;;=>   93.28% CPU
;;=>   346,417,259 processor cycles
;;=>   0 bytes consed
;;=>
;;=> 8739992577
