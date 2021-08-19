;; Code for Project Euler Problem 46.
;;
;; Code author: Russell A. Edson
;; Date last modified: 19/08/2021

;; Here we seek a counterexample to Goldbach's 'other conjecture' that
;; every odd composite (i.e. not prime) number can be written as the
;; sum of a prime and twice a square number. So we want to find the
;; smallest odd composite number where this isn't the case.

;; A straightforward way to do this is simply to grow lists of primes
;; and square numbers as we loop to larger and larger composite numbers.
;; Since the numbers are positive and we are looking at sums, then we
;; only ever have to check the primes and squares less than the
;; given composite number.
;;
;; So we'll grow a memoized list of primes using a closure:
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
  "Returns the next prime number greater than the given number N."
  (let ((n (1+ n)))
    (loop while (not (is-prime? n)) do (incf n))
    n))

(defvar memoized-primes
  (let ((primes (list 2)))
    (lambda (n)
      (if (<= n (length primes))
	  (nth (1- n) primes)
	  (progn
	    (loop while (> n (length primes)) do
	      (setf primes
		    (append primes
			    (list (next-prime (car (last primes)))))))
	    (car (last primes)))))))

(defun nth-prime (n)
  "Return the Nth prime number."
  (funcall memoized-primes n))

(nth-prime 20)
;;=> 71

;; We'll now proceed as follows:
;; Given the next odd number (starting from e.g. 35, since we know that
;; the odd composite numbers up to 33 can be written as the sum of
;; a prime and a doubled square number), we'll first confirm that the
;; number is not a prime number, and then we'll loop across the primes
;; and the doubled square numbers less than this number. If we can
;; find a pair that sums to the composite number, we'll check the next
;; odd number. If not, then we're done: we've found our counterexample.
(time
 (let ((counterexample nil)
       (odd-number 35))
   (loop while (null counterexample) do
     (if (is-prime? odd-number)
	 (setf odd-number (+ odd-number 2))
	 (let* ((prime-index 1)
		(prime (nth-prime prime-index))
		(sum-pair nil))
	   (loop while (< prime odd-number) do
	     (let* ((square-index 1)
		    (double-square (* 2 square-index square-index)))
	       (loop while (<= double-square (- odd-number prime)) do
		 (if (= odd-number (+ prime double-square))
		     (setf sum-pair (+ prime double-square)))
		 (incf square-index)
		 (setf double-square (* 2 square-index square-index))))
	     (incf prime-index)
	     (setf prime (nth-prime prime-index)))
	   (if (null sum-pair)
	       (setf counterexample odd-number)
	       (setf odd-number (+ odd-number 2))))))
   counterexample))
;;=> Evaluation took:
;;=>   1.211 seconds of real time
;;=>   1.218750 seconds of total run time (1.218750 user, 0.000000 system)
;;=>   100.66% CPU
;;=>   3,138,511,495 processor cycles
;;=>   4,620,288 bytes consed
;;=>
;;=> 5777
