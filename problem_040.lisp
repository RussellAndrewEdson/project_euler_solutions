;; Code for Project Euler Problem 40.
;;
;; Code author: Russell A. Edson
;; Date last modified: 05/08/2021

;; We want to find the product of certain digits of the decimal
;; part of Champernowne's constant. The constant is constructed
;; by concatenating successive positive integers, which suggests
;; for us a straightforward way to generate the digits. (We'll
;; additionally memoize through closures for efficiency.)

(defun digits (number)
  "Return the set of digits in the given NUMBER."
  (mapcar #'digit-char-p (coerce (write-to-string number) 'list)))

(defvar champernowne-functions
  (let ((champernowne-nums nil)
	(next-num 1))
    (list
     ;; Memoize nth Champernowne digit
     (lambda (n)
       (if (<= n (length champernowne-nums))
	   (nth (1- n) champernowne-nums)
	   (progn
	     (loop while (> n (length champernowne-nums)) do
	       (setf champernowne-nums
		     (append champernowne-nums (digits next-num)))
	       (incf next-num))
	     (nth (1- n) champernowne-nums))))

     ;; Memoize printing Champernowne's constant up to n digits
     (lambda (n)
       (if (<= n (length champernowne-nums))
	   (format nil "0.~{~d~}" (subseq champernowne-nums 0 n))
	   (progn
	     (loop while (> n (length champernowne-nums)) do
	       (setf champernowne-nums
		     (append champernowne-nums (digits next-num)))
	       (incf next-num))
	     (format nil "0.~{~d~}" (subseq champernowne-nums 0 n))))))))

(defun champernowne-digit (n)
  "Return the Nth digit of the decimal part of Champernowne's constant."
  (funcall (car champernowne-functions) n))

(defun champernowne (n)
  "Return a string containing Champernowne's constant up to N decimals."
  (funcall (cadr champernowne-functions) n))

(champernowne-digit 50)
;;=> 3

(champernowne 50)
;;=> "0.12345678910111213141516171819202122232425262728293"

;; Then we can exploit the memoization in computing the product by
;; making sure that we compute the largest Champernowne digit first.
(time (champernowne-digit 1000000))
;;=> Evaluation took:
;;=>   1669.139 seconds of real time
;;=>   1673.046875 seconds of total run time (820.781250 user, 852.265625 system)
;;=>   [ Run times consist of 800.222 seconds GC time, and 872.825 seconds non-GC time. ]
;;=>   100.23% CPU
;;=>   4,326,421,161,484 processor cycles
;;=>   1,397,726,752,496 bytes consed
;;=>
;;=> 1

(time (reduce #'* (mapcar #'champernowne-digit
			  (list 1000000 100000 10000 1000 100 10 1))))
;;=> Evaluation took:
;;=>   0.018 seconds of real time
;;=>   0.015625 seconds of total run time (0.015625 user, 0.000000 system)
;;=>   88.89% CPU
;;=>   46,728,730 processor cycles
;;=>   0 bytes consed
;;=>
;;=> 210
