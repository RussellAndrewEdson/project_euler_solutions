;; Code for Project Euler Problem 29.
;;
;; Code author: Russell A. Edson
;; Date last modified: 06/08/2021

;; We want to determine the number of distinct terms in the sequence
;; a^b for 2 <= a, b <= 100. We can straightforwardly code this in
;; Common Lisp in a readable nested loop.
(time
 (defvar distinct-terms
   (remove-duplicates
    (loop for a from 2 to 100
	  append
	  (loop for b from 2 to 100 collect (expt a b))))))
;;-> Evaluation took:
;;=>   0.002 seconds of real time
;;=>   0.000000 seconds of total run time (0.000000 user, 0.000000 system)
;;=>   0.00% CPU
;;=>   5,223,140 processor cycles
;;=>   2,245,360 bytes consed
;;=>
;;=> DISTINCT-TERMS

(length distinct-terms)
;;=> 9183
