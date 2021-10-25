;; Code for Project Euler Problem 71.
;;
;; Code author: Russell A. Edson
;; Date last modified: 25/10/2021

;; Here we want to find the reduced proper fraction n/d immediately
;; to the left of 3/7 if we ordered all of the fractions for
;; n < d < 1,000,000 in increasing order. We want to report the
;; numerator of such a fraction.

;; This problem is actually surprisingly easy as given. We're
;; interested in a number only slightly less than 3/7 and we can
;; take our denominators as large as 1,000,000, so our first
;; instinct is to try to rewrite the fraction 3/7 with a denominator
;; as close to 1,000,000 as possible.

;; Integer-dividing by 7 gives us:
(truncate (/ 1000000 7))
;;=> 142857
;;=> 1/7

;; That is, 1000000 = 7*142857 + 1. The remainder here is as small
;; as it can be without the problem being trivial, so we need not
;; do any more work: We note that 7*142857 = 999,999, and since
(* 3 142857)
;;=> 428571

;; ...then that means that 428571/999999 = 3/7, and the number
;; immediately less than this, 428570/999999, will necessarily
;; be our answer. There can be no number between 425870/999999
;; and 428571/999999 if we restrict our denominator to be less
;; than 1,000,000, and so we're done.

;; (In the case that the problem wasn't this easy, then this process
;; still gives us a useful lower bound. For example, suppose we
;; considered d <= 1,500,000 instead. Then integer-division by 7
;; gives us
(truncate (/ 1500000 7))
;;=> 214285
;;=> 5/7

;; i.e. 1500000 = 7*214285 + 5, and since
(* 3 214285)
;;=> 642855

;; ...then 3/7 = 642855/1499995. So if 642854/1499995 wasn't the
;; leftmost fraction, then we need only check a small handful of
;; fractions that could possibly exist between 642854/1499995 and
;; 642855/1499995 with a denominator below 1500000:
;;   642854/1499996, 642855/1499996, 642856/1499996,
;;   642854/1499997, 642855/1499997, 642856/1499997, 642857/1499997,
;;   ... and so on.)
