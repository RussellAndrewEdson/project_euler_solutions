;; Code for Project Euler Problem 47.
;;
;; Code author: Russell A. Edson
;; Date last modified: 24/08/2021

;; We want to find the first four consecutive integers to have four
;; distinct prime factors each (and then return the first number).

;; First, we code a function to compute the prime factors of a given
;; number and return them in a list:
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

(prime-factorization 14)
;;=> (7 2)

(prime-factorization 15)
;;=> (5 3)

;; The numbers 14 and 15 are the first two consecutive integers
;; to have two distinct prime factors each (e.g. since 2, 3, 4,
;; and 5 each only have one prime factor, 6 has two prime factors
;; but 7 only has one, 8 and 9 only have one distinct prime factor
;; each, 10 has two but 11 has only one, 12 has two but 13 has one).
;;
;; We are only interested in the first set of consecutive numbers,
;; so a simple loop suffices here. For some modest improvements in
;; efficiency we can be smart about how we store previously-computed
;; prime factors so that we're not recomputing them on every iteration.
(defun first-n-with-n-distinct-prime-factors (n)
  "Return the first N consecutive numbers with N prime factors."
  (let* ((consecutives (loop for i from 2 below (+ 2 n) collect i))
	 (prime-factors
	   (mapcar (lambda (m) (remove-duplicates (prime-factorization m)))
		   consecutives))
	 (n-distinct-each?
	   (mapcar (lambda (factors) (= (length factors) n))
		   prime-factors)))
    (loop while (null (reduce (lambda (a b) (and a b)) n-distinct-each?)) do
      (let* ((next-number (1+ (car (last consecutives))))
	     (next-factors
	       (remove-duplicates (prime-factorization next-number))))
	(setf consecutives (append (cdr consecutives) (list next-number)))
	(setf prime-factors (append (cdr prime-factors) (list next-factors)))
	(setf n-distinct-each? (append (cdr n-distinct-each?)
				       (list (= (length next-factors) n))))))
    consecutives))

(first-n-with-n-distinct-prime-factors 2)
;;=> (14 15)

(time (first-n-with-n-distinct-prime-factors 3))
;;=> Evaluation took:
;;=>   0.002 seconds of real time
;;=>   0.000000 seconds of total run time (0.000000 user, 0.000000 system)
;;=>   0.00% CPU
;;=>   3,924,556 processor cycles
;;=>   131,072 bytes consed
;;=>
;;=> (644 645 646)

(time (first-n-with-n-distinct-prime-factors 4))
;;=> Evaluation took:
;;=>   22.571 seconds of real time
;;=>   22.453125 seconds of total run time (22.343750 user, 0.109375 system)
;;=>   99.48% CPU
;;=>   58,506,777,412 processor cycles
;;=>   38,928,384 bytes consed
;;=>
;;=> (134043 134044 134045 134046)
