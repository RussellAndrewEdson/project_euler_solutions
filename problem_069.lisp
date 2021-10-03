;; Code for Project Euler Problem 69.
;;
;; Code author: Russell A. Edson
;; Date last modified: 03/10/2021

;; Here we want to find the 'Totient maximum' for n <= 1,000,000,
;; where the Totient function phi(n) is defined to be the number
;; of natural numbers less than n which are relatively prime to n,
;; and we seek the maximum ratio n/phi(n).

;; We attack this problem in a straightforward series of steps.
;; First, we want to know when two numbers are relatively prime;
;; i.e. when they contain no common factors except 1 (or their
;; greatest common divisor, gcd, is 1). Luckily Common Lisp
;; already includes gcd as a standard library function, but we
;; could have coded up the Euclidean algorithm if that weren't
;; the case.
;;
;; So our check for relative primes is trivial. Now, we expect
;; that this function will be called very often as we loop to
;; find phi(n) for all n <= 1,000,000, so we can set up some
;; optimisations to improve the efficiency given that we know
;; we're never dealing with numbers larger than 1,000,000 or so:
(declaim
 (ftype (function ((integer 1 4611686018427387903)
		   (integer 1 4611686018427387903))
		  boolean)
	relatively-prime?))
(defun relatively-prime? (a b)
  "True if the given numbers A and B are relatively prime."
  (declare (optimize (safety 0) (debug 0) (space 0) (speed 3)))
  (declare (type (integer 1 4611686018427387903) a b))
  (the boolean (= 1 (gcd a b))))

(time (relatively-prime? 12341 98779))
;;=> Evaluation took:
;;=>   0.000 seconds of real time
;;=>   0.000000 seconds of total run time (0.000000 user, 0.000000 system)
;;=>   100.00% CPU
;;=>   3,242 processor cycles
;;=>   0 bytes consed
;;=>
;;=> T

;; With our check for relative primality, we can now code the function
;; phi(n) which counts the relative primes below n:
(declaim (ftype (function ((integer 1 4611686018427387903))
			  (integer 1 4611686018427387903))
		phi))
(defun phi (n)
  "Return the number of relative primes below N."
  (declare (optimize (safety 0) (debug 0) (space 0) (speed 3)))
  (declare (type (integer 1 4611686018427387903) n))
  (the (integer 1 4611686018427387903)
       (loop for m of-type (integer 1 4611686018427387903) from 1 below n
	     when (relatively-prime? m n)
	       count m)))

(time (phi 999999))
;;=> Evaluation took:
;;=>   0.126 seconds of real time
;;=>   0.125000 seconds of total run time (0.125000 user, 0.000000 system)
;;=>   99.21% CPU
;;=>   327,861,539 processor cycles
;;=>   0 bytes consed
;;=>
;;=> 466560

;; Finally, we simply loop across all of the n from 2 to 1,000,000,
;; keeping track of the maximum value of the ratio n/phi(n) as we go.
;; (This still takes quite a bit of time though...)
(time
 (let ((max-n (cons 1 1)))
   (loop for n from 2 upto 1000000 do
     (let ((new-ratio (/ n (phi n))))
       (if (> new-ratio (cdr max-n))
	   (setf max-n (cons n new-ratio)))))
   max-n))
;;=> Evaluation took:
;;=>   74002.749 seconds of real time
;;=>   73891.734375 seconds of total run time (73888.625000 user, 3.109375 system)
;;=>   99.85% CPU
;;=>   191,815,353,148,578 processor cycles
;;=>   32,014,272 bytes consed
;;=>
;;=> (510510 . 17017/3072)
