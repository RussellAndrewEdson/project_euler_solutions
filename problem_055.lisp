;; Code for Project Euler Problem 55.
;;
;; Code author: Russell A. Edson
;; Date last modified: 30/08/2021

;; Here we are interested in finding Lychrel numbers: those numbers
;; which don't become palindromic numbers by adding the number to its
;; own reverse iteratively, for some finite number of iterations.
;; Pragmatically we assume that any number that reaches 50 iterations
;; without becoming a palindrome is a Lychrel number.

;; This problem is rather straightforward is we exploit Common Lisp's
;; arbitrary-length integers. We simply code up some routines for
;; manipulating digits and checking for a palindrome:
(defun digits (number)
  "Return the set of digits in the given NUMBER."
  (mapcar #'digit-char-p (coerce (write-to-string number) 'list)))

(defun combine-digits (digits)
  "Return the number formed by (re)-combining the given DIGITS."
  (values (parse-integer (format nil "~{~d~}" digits))))

(digits 12345)
;;=> (1 2 3 4 5)

(combine-digits '(9 8 7 6 5))
;;=> 98765

(defun reverse-number (number)
  "Return the reverse of the given NUMBER."
  (combine-digits (reverse (digits number))))

(defun palindrome? (number)
  "True if the given NUMBER is a palindrome."
  (let ((number-digits (digits number)))
    (equalp number-digits (reverse number-digits))))

(reverse-number 4213)
;;=> 3124

(palindrome? 4123)
;;=> NIL

(palindrome? (+ 4123 (reverse-number 4123)))
;;=> T

;; Now we can define an iterative procedure for checking if a given
;; number is a Lychrel number (where we assume it is unless it becomes
;; a palindrome through iterative addition with its reverse in under
;; fifty iterations).
(defun lychrel? (n)
  "True if the given number N is a Lychrel number."
  (let ((lychrelp t))
    (loop for iter from 1 below 50
	  while lychrelp do
	    (setf n (+ n (reverse-number n)))
	    (if (palindrome? n)
		(setf lychrelp nil)))
    lychrelp))

(lychrel? 349)
;;=> NIL

(lychrel? 196)
;;=> T

(loop for n from 1 below 10000 when (lychrel? n) count n)
;;=> 249
