;; Code for Project Euler Problem 20.
;;
;; Code author: Russell A. Edson
;; Date last modified: 26/07/2021

;; Given Common Lisp's arbitrary-length integers, computing the sum
;; of the digits in 100 factorial is straightforward.
(defun factorial (n)
  "Return N factorial."
  (loop for m from 1 to n
	and prod = 1 then (* prod m)
	finally (return prod)))

(factorial 10)
;;=> 3628800

(defun digits (number)
  "Return the set of digits in the given NUMBER."
  (mapcar #'digit-char-p (coerce (write-to-string number) 'list)))

(reduce #'+ (digits (factorial 10)))
;;=> 27

(reduce #'+ (digits (factorial 100)))
;;=> 648
