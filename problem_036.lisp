;; Code for Project Euler Problem 36.
;;
;; Code author: Russell A. Edson
;; Date last modified: 10/08/2021

;; In this problem we are looking at palindromes in base-10 and base-2,
;; so we'll first define some functions that allow us to check for
;; palindromes and convert bases.
(defun digits (number)
  "Return the set of digits in the given NUMBER."
  (mapcar #'digit-char-p (coerce (write-to-string number) 'list)))

(defun binary-representation (number)
  "Return the binary representation of the given NUMBER."
  (let ((remainder number)
	(binary nil))
    (loop while (> remainder 0) do
      (multiple-value-bind (half leftover) (truncate (/ remainder 2))
	(setf remainder half)
	(setf binary (cons (if (zerop leftover) 0 1) binary))))
    binary))

(defun palindrome? (list)
  "True if the given LIST represents a palindrome in its elements."
  (equalp list (reverse list)))

(palindrome? (print (digits 585)))
;;=> (5 8 5) 
;;=> T

(palindrome? (print (binary-representation 585)))
;;=> (1 0 0 1 0 0 1 0 0 1) 
;;=> T

;; We want to find the numbers less than 1000000 that are palindromic
;; in both base-10 and base-2.
(defun double-base-palindrome? (number)
  "True if the given NUMBER is palindromic in base-10 and base-2."
  (and (palindrome? (digits number))
       (palindrome? (binary-representation number))))

(double-base-palindrome? 585)
;;=> T

(time
 (defvar palindrome-numbers
   (loop for n from 1 upto 1000000
	 when (double-base-palindrome? n)
	   collect n)))
;;=> Evaluation took:
;;=>   1.761 seconds of real time
;;=>   1.156250 seconds of total run time (0.875000 user, 0.281250 system)
;;=>   [ Run times consist of 0.219 seconds GC time, and 0.938 seconds non-GC time. ]
;;=>   65.64% CPU
;;=>   4,563,535,844 processor cycles
;;=>   332,866,384 bytes consed
;;=>
;;=> PALINDROME-NUMBERS

palindrome-numbers
;;=> (1 3 5 7 9 33 99 313 585 717 7447 9009 15351 32223 39993 53235
;;=>  53835 73737 585585)

(reduce #'+ palindrome-numbers)
;;=> 872187
