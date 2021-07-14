;; Code for Project Euler Problem 7.
;;
;; Code author: Russell A. Edson
;; Date last modified: 14/07/2021

;; We want the 10001st prime number. It is most straightforward
;; to simply generate a list of prime numbers and grab the
;; 10001st prime from that list. For some extra fun though, we can
;; implement a simple memoized prime generator that stores the
;; previously-computed primes for efficient lookup.

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

;; Implement a memoized prime generator as a closure (and wrap it in
;; a nice function)
(defvar nth-prime
  (let ((primes-list (list 2)))
    (lambda (n)
      (if (<= n (length primes-list))
	  (nth (1- n) primes-list)
	  (progn
	    (loop while (> n (length primes-list)) do
	      (setf primes-list
		    (append primes-list
			    (list (next-prime (car (last primes-list)))))))
	    (car (last primes-list)))))))

(defun nth-prime (n)
  "Return the Nth prime."
  (funcall nth-prime n))

;; And we can get the 10001st prime (and also see the cool memoization
;; and fast subsequent lookups in effect):
(time (nth-prime 10001))
;;=> Evaluation took:
;;=>   0.893 seconds of real time
;;=>   0.968750 seconds of total run time (0.781250 user, 0.187500 system)
;;=>   [ Run times consist of 0.093 seconds GC time, and 0.876 seconds non-GC time. ]
;;=>   108.51% CPU
;;=>   2,314,333,345 processor cycles
;;=>   800,234,896 bytes consed
;;=>
;;=> 104743

(time (nth-prime 10001))
;;=> Evaluation took:
;;=>   0.000 seconds of real time
;;=>   0.000000 seconds of total run time (0.000000 user, 0.000000 system)
;;=>   100.00% CPU
;;=>   114,000 processor cycles
;;=>   0 bytes consed
;;=> 
;;=> 104743
