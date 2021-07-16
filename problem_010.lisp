;; Code for Project Euler Problem 10.
;;
;; Code author: Russell A. Edson
;; Date last modified: 16/07/2021

;; We want the sum of the primes below two million, so we'll need
;; to generate primes up to two million first. We can do this
;; straightforwardly by iterating through a list of the prime
;; numbers and summing them up as we go.

(defun is-prime? (n)
  "True if the given number N is prime, False if not."
  (if (= n 1)
      nil
      (let ((primep t))
	(loop for m from 2 upto (floor (sqrt n)) do
	  (if (zerop (mod n m))
	      (setf primep nil)))
	primep)))

(defun next-prime (n)
  "Returns a prime number greater than the given number N."
  (let ((n (1+ n)))
    (loop while (not (is-prime? n)) do (incf n))
    n))

(let ((prime 2)
      (prime-sum 0))
  (loop while (< prime 2000000) do
    (setf prime-sum (+ prime-sum prime))
    (setf prime (next-prime prime)))
  prime-sum)
;;=> 142913828922
