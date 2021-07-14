;; Code for Project Euler Problem 3.
;;
;; Code author: Russell A. Edson
;; Date last modified: 14/07/2021

;; We want to find the largest prime factor for the number
;; 600851475143. This is still a (relatively) small
;; number that is well-within capabilities of brute-forcing
;; with modern computation. So we code up a straightforward
;; trial division algorithm to find prime factors...

(defun prime-factorization (n)
  "Return a prime factorization of the natural number N."
  (let ((primes '())
	(factor 2))
    (loop while (> n 1)
	  do (cond ((zerop (mod n factor))
		    (setf primes (cons factor primes))
		    (setf n (/ n factor)))
		   (t (incf factor))))
    primes))

;; Test that the prime factors of 13195 are 5, 7, 13 and 29:
(prime-factorization 13195)
;;=> (29 13 7 5)

;; And then simply run to find the prime factors of 600851475143.
;; For interest we can time the function and see that even for
;; this 'large-ish' number, the factorization is near instantaneous:
(time (prime-factorization 600851475143))
;;=> Evaluation took:
;;=>   0.000 seconds of real time
;;=>   0.000000 seconds of total run time (0.000000 user, 0.000000 system)
;;=>   100.00% CPU
;;=>   270,798 processor cycles
;;=>   0 bytes consed
;;=>
;;=> (6857 1471 839 71)
