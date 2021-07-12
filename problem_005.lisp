;; Code for Project Euler Problem 5.
;;
;; Code author: Russell A. Edson
;; Date last modified: 12/07/2021

;; As given, 2520 is the Least Common Multiple of the numbers 1-10,
;; since 2520 = 2 * 2 * 2 * 3 * 3 * 5 * 7, the product of the highest
;; powers of prime numbers for the range 1-10. This suggests a way
;; to solve our problem: we compute a prime factorization for each
;; of the numbers in the range of interest, and then multiply only
;; the highest powers of the prime factors together.

;; We can code up a quick trial division algorithm:
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

(defun prime-powers (n)
  "Return a list of (p . q) cells for primes p^q that factorize N."
  (let ((factorization (prime-factorization n)))
    (loop for prime in (remove-duplicates factorization)
	  collect
	  (cons prime
		(count prime factorization)))))


;; And then we simply collect up all of the prime powers for the
;; numbers 1 through 20 and retain only the largest prime powers.
(defun least-common-multiple (nums)
  "Return the least common multiple for the given NUMS."
  (let* ((factorizations (mapcan #'prime-powers nums))
	 (highest-powers
	   (loop for prime in (remove-duplicates
			       (mapcar #'car factorizations))
		 collect
		 (cons prime
		       (loop for factor in (remove-if-not
					    (lambda (p) (= (car p) prime))
					    factorizations)
			     maximize (cdr factor))))))
    (reduce #'* (mapcar (lambda (factor) (expt (car factor) (cdr factor)))
			highest-powers))))

(least-common-multiple (loop for n from 1 to 20 collect n))
;;=> 232792560
