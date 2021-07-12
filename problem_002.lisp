;; Code for Project Euler Problem 2.
;;
;; Code author: Russell A. Edson
;; Date last modified: 12/07/2021

;; 4 million is still a small number in the face of modern computation.
;; We can still approach this problem by building a Fibonacci number
;; generator, computing terms up to 4 million, and then filtering to
;; the even numbers for the summation.

(defun fibonacci-upto (n)
  "Return the list of Fibonacci numbers up to N, starting from 1 and 2."
  (let ((fibonacci-numbers '(2 1))
	(next-num (+ 2 1)))
    (loop while (< next-num n)
	  do (setf fibonacci-numbers
		   (cons next-num fibonacci-numbers))
	     (setf next-num (+ (car fibonacci-numbers)
			       (cadr fibonacci-numbers))))
    (reverse fibonacci-numbers)))

(defvar even-fibs (remove-if-not #'evenp (fibonacci-upto 4000000)))
;;=> (2 8 34 144 610 2584 10946 46368 196418 832040 3524578)

(reduce #'+ even-fibs)
;;=> 4613732
