;; Code for Project Euler Problem 51.
;;
;; Code author: Russell A. Edson
;; Date last modified: 30/08/2021

;; Here we looks at digit substitutions in prime numbers. For
;; instance, given a prime number
;;   56663
;; and a substitution pattern
;;   _***_
;; where '_' keeps the number but '*' replaces with a new digit,
;; we generate the family 50003, 51113, 52223, ..., 59993, and we
;; count the primes in that substitution family.
;;
;; We seek the smallest prime that is part of an 8-prime family.
;; Importantly, note that this immediately restricts our search
;; space: we do not need to enumerate all possible substitution
;; patterns. e.g. for the prime 56663, we need only check three
;; substitution patterns:
;;   *____, _***_, and ____*,
;; one pattern for each unique digit in the prime. Specifically,
;; we do not need to check arbitrary patterns like *_*_*, since
;; we require that the prime itself be part of the substitution
;; family which means it must adhere to that pattern too.
;;
;; So we can already code a function to generate the substitution
;; patterns given a prime number.
(defun digits (number)
  "Return the set of digits in the given NUMBER."
  (mapcar #'digit-char-p (coerce (write-to-string number) 'list)))

(defun substitution-patterns (number)
  "Return the list of permissible substitution patterns for NUMBER."
  (let* ((number-digits (digits number)))
    (loop for unique-digit in (remove-duplicates number-digits)
	  collect
	  (format nil "~{~a~}" 
		  (mapcar (lambda (digit)
			    (if (equalp digit unique-digit) "*" "_"))
			  number-digits)))))

(substitution-patterns 56663)
;;=> ("*____" "_***_" "____*")

;; So given a number and a substitution pattern, we now code a
;; function to generate a list of new numbers from substituting
;; the digits 0-9 at the * locations (keeping in mind that we
;; ignore the 0 digit substitution if the pattern contains a leading *:
(defun substitutions (number pattern)
  "Return the new numbers from subbing digits of NUMBER at PATTERN."
  (let ((number-digits (digits number))
	(pattern (coerce pattern 'list))
	(new-digits (loop for n from 0 upto 9 collect n)))
    ;; No 0's if the pattern has a leading *
    (if (equalp (car pattern) #\*)
	(setf new-digits (cdr new-digits)))
    (loop for digit-to-sub in new-digits
	  collect
	  (let ((substitution number-digits))
	    (loop for index from 0 below (length pattern) do
	      (if (equalp (nth index pattern) #\*)
		  (setf (nth index substitution) digit-to-sub)))
	    (parse-integer (format nil "~{~d~}" substitution))))))

(substitutions 56663 "*____")
;;=> (16663 26663 36663 46663 56663 66663 76663 86663 96663)

(substitutions 56663 "_***_")
;;=> (50003 51113 52223 53333 54443 55553 56663 57773 58883 59993)

;; At this point we have a function to get all of the substitution
;; patterns, and a function to perform all of the substitutions that
;; we eventually loop over. All we need now is the infrastructure for
;; testing primality in an efficient way (noting that we'll likely
;; be checking many primes repeatedly, so it is worth the effort to
;; memoize the process a little bit).
;;
;; So we'll keep lists of primes and not-primes in a closure,
;; as in Problem 35:
(defvar memoized-primes
  (let ((primes-list nil)
	(not-primes-list nil))
    (lambda (n)
      (cond ((member n primes-list) t)
	    ((member n not-primes-list) nil)
	    (t (if (<= n 1)
		   nil
		   (let ((primep t))
		     (loop for m from 2 upto (floor (sqrt n)) do
		       (if (zerop (mod n m))
			   (setf primep nil)))
		     (if primep
			 (cons n primes-list)
			 (cons n not-primes-list))
		     primep)))))))

(defun is-prime? (n)
  "True if N is a prime number."
  (funcall memoized-primes n))

(is-prime? 56663)
;;=> T

;; Finally, we can combine all of these pieces and write our program.
;; We know that numbers below 56003 do not have an eight-prime family,
;; so we'll start here and loop until we've found a prime number
;; that does have an eight-prime family of substitutions, keeping in
;; mind that if we ever have 3 or more non-primes in the substitution
;; list, we cannot have our eight-prime family and so we move to the
;; next one.
(time
 (let ((solution nil)
       (n 56004))
   (loop while (not solution) do
     (if (is-prime? n)
	 (let ((all-substitutions
		 (mapcar (lambda (pattern) (substitutions n pattern))
			 (substitution-patterns n))))
	   (loop for family in all-substitutions do
	     (let ((primes 0))
	       (block prime-test
		 (let ((non-primes 0))
		   (loop for number in family do
		     (if (is-prime? number)
			 (incf primes)
			 (incf non-primes))
		     (if (>= non-primes 3)
			 (return-from prime-test)))))
	       (if (= primes 8)
		   (setf solution (cons family solution)))))))
     (incf n))
   solution))
;;=> Evaluation took:
;;=>   0.967 seconds of real time
;;=>   0.968750 seconds of total run time (0.968750 user, 0.000000 system)
;;=>   100.21% CPU
;;=>   2,506,084,663 processor cycles
;;=>   156,086,480 bytes consed
;;=>
;;=> ((121313 222323 323333 424343 525353 626363 727373 828383 929393))
