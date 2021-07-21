;; Code for Project Euler Problem 12.
;;
;; Code author: Russell A. Edson
;; Date last modified: 21/07/2021

;; We code up a function to perform a factorization of a given
;; number (albeit a naive implementation, but we still have
;; a lot of leeway for inefficiency here):
(defun factorization (n)
  "Return a factorization of the natural number N."
  (loop for factor from 1 upto (floor (sqrt n))
	when (zerop (mod n factor))
	  append (remove-duplicates (list factor (/ n factor)))))

(factorization 144)
;;=> (1 144 2 72 3 48 4 36 6 24 8 18 9 16 12)

;; And then it is a straightforward matter to loop over the triangle
;; numbers, counting the number of factors for each until we find
;; the first triangle number with over 500 divisors.
(let ((n 0))
  (loop for inc = 1 then (1+ inc)
	while (< (length (factorization n)) 500)
	do (setf n (+ n inc)))
  n)
;;=> 76576500
