;; Code for Project Euler Problem 27.
;;
;; Code author: Russell A. Edson
;; Date last modified: 06/08/2021

;; We consider quadratics of the form
;;   n^2 + a*n + b, for -1000 < a < 1000 and -1000 <= b <= 1000.
;; For certain combinations of a and b these formulae generate
;; successive prime numbers starting from n=0, and we want to
;; determine the coefficients a and b that produce the longest
;; such sequence of prime numbers (and then compute a*b).

;; First, we code a function to test for primes:
(defun is-prime? (n)
  "True if the given number N is prime, False if not."
  (if (<= n 1)
      nil
      (let ((primep t))
	(loop for m from 2 upto (floor (sqrt n)) do
	  (if (zerop (mod n m))
	      (setf primep nil)))
	primep)))

(is-prime? 17)
;;=> T

;; And then we code a function that counts the number of
;; consecutive primes for the quadratic given a and b:
(defun consecutive-primes (a b)
  "Return the number of consecutive primes for n^2+A*n+B, n=0,1,2,...."
  (loop for n = 0 then (1+ n)
	while (is-prime? (+ (* n n) (* a n) b))
	count n))

(consecutive-primes 1 41)
;;=> 40

(consecutive-primes -79 1601)
;;=> 80

;; Finally, we simply loop over all a in (-1000, 1000),
;; b in [-1000, 1000], and find the largest number of consecutive
;; primes.
(let ((max-primes 0)
      (max-pair nil))
  (loop for a from -999 upto 999 do
    (loop for b from -1000 upto 1000 do
      (let ((num-primes (consecutive-primes a b)))
	(if (>= num-primes max-primes)
	    (progn
	      (setf max-primes num-primes)
	      (setf max-pair (list a b)))))))
  (list max-pair max-primes (reduce #'* max-pair)))
;;=> ((-61 971) 71 -59231)
