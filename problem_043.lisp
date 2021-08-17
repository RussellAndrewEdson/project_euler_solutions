;; Code for Project Euler Problem 43.
;;
;; Code author: Russell A. Edson
;; Date last modified: 17/08/2021

;; We are again interested in pandigital numbers, as in e.g. Problem 38.
;; So we can modify some functions that we coded in that problem
;; for manipulating digits and checking for pandigital numbers:
(defun digits (number)
  "Return the set of digits in the given NUMBER."
  (mapcar #'digit-char-p (coerce (write-to-string number) 'list)))

(defun combine-digits (digits)
  "Return the number we get by concatenating the given DIGITS together."
  (values (parse-integer (format nil "~{~d~}" digits))))

(defun 0-9-pandigital? (number)
  "True if the given NUMBER contains the digits 0-9 in any order."
  (equalp
   (list 0 1 2 3 4 5 6 7 8 9)
   (sort (digits number) #'<)))

(0-9-pandigital? 1926473805)
;;=> T

;; Now we are specifically interested in 9-pandigital numbers that
;; have the following substring divisibility properties. We'll code
;; some utility functions to extract and test these substrings:
(defun substring-number (number indices)
  "Return the substring of NUMBER made from the given list of INDICES."
  (let ((number-digits (digits number)))
    (combine-digits (mapcar (lambda (index) (nth (1- index) number-digits))
			    indices))))

(substring-number 98765 '(3 4 5))
;;=> 765

(defun divisible-by? (number divisor)
  "True if NUMBER is divisible by DIVISOR."
  (zerop (mod number divisor)))

;; Now we code each of the substring divisibility properties in turn:
;; 1.
(defun d2d3d4-divisible-by-2? (number)
  "True if digits 2-4 of NUMBER, as a substring, are divisible by 2."
  (divisible-by? (substring-number number '(2 3 4)) 2))

;; 2.
(defun d3d4d5-divisible-by-3? (number)
  "True if digits 3-5 of NUMBER, as a substring, are divisible by 3."
  (divisible-by? (substring-number number '(3 4 5)) 3))

;; 3.
(defun d4d5d6-divisible-by-5? (number)
  "True if digits 4-6 of NUMBER, as a substring, are divisible by 5."
  (divisible-by? (substring-number number '(4 5 6)) 5))

;; 4.
(defun d5d6d7-divisible-by-7? (number)
  "True if digits 5-7 of NUMBER, as a substring, are divisible by 7."
  (divisible-by? (substring-number number '(5 6 7)) 7))

;; 5.
(defun d6d7d8-divisible-by-11? (number)
  "True if digits 6-8 of NUMBER, as a substring, are divisible by 11."
  (divisible-by? (substring-number number '(6 7 8)) 11))

;; 6.
(defun d7d8d9-divisible-by-13? (number)
  "True if digits 7-9 of NUMBER, as a substring, are divisible by 13."
  (divisible-by? (substring-number number '(7 8 9)) 13))

;; 7.
(defun d8d9d10-divisible-by-17? (number)
  "True if digits 8-10 of NUMBER, as a substring, are divisible by 17."
  (divisible-by? (substring-number number '(8 9 10)) 17))

(let ((number 1406357289))
  (and (0-9-pandigital? number)
       (d2d3d4-divisible-by-2? number)
       (d3d4d5-divisible-by-3? number)
       (d4d5d6-divisible-by-5? number)
       (d5d6d7-divisible-by-7? number)
       (d6d7d8-divisible-by-11? number)
       (d7d8d9-divisible-by-13? number)
       (d8d9d10-divisible-by-17? number)))
;;=> T

;; With these conditions defined, we can now plan our search across
;; all of the 0-9 pandigital numbers. An exhaustive search would
;; have us looking at all of the numbers between 1023456789 and
;; 9876543210, which is a huge amount of numbers! By contrast,
;; permuting the digits in 0, 1, 2, ..., 9 has us looking at the
;; (relatively) modest 9*9! = 3265920 combinations instead (i.e. 10!
;; for 10 digits, but we cannot start a number with a 0 so we don't
;; have to consider those cases at all), which is much more reasonable.
;; For this we'll defer to the efficient cl-permutation library:
(ql:quickload '(:cl-permutation))
(time
 (defvar special-pandigitals
   (let ((1-9-digits '(1 2 3 4 5 6 7 8 9))
	 (divisible-numbers nil))
     (loop for digit in 1-9-digits do
       (let ((available-digits (cons 0 (remove digit 1-9-digits))))
	 (cl-permutation:doperms (permutation 9)
	   (let ((number (combine-digits
			  (cons digit
				(cl-permutation:permute permutation
							available-digits)))))
	     (if (and (d2d3d4-divisible-by-2? number)
		      (d3d4d5-divisible-by-3? number)
		      (d4d5d6-divisible-by-5? number)
		      (d5d6d7-divisible-by-7? number)
		      (d6d7d8-divisible-by-11? number)
		      (d7d8d9-divisible-by-13? number)
		      (d8d9d10-divisible-by-17? number))
		 (setf divisible-numbers (cons number divisible-numbers)))))))
     divisible-numbers)))
;;=> Evaluation took:
;;=>   12.405 seconds of real time
;;=>   12.390625 seconds of total run time (12.234375 user, 0.156250 system)
;;=>   [ Run times consist of 0.346 seconds GC time, and 12.045 seconds non-GC time. ]
;;=>   99.89% CPU
;;=>   32,153,706,650 processor cycles
;;=>   8,248,314,736 bytes consed
;;=>
;;=> SPECIAL-PANDIGITALS

special-pandigitals
;;=> (4106357289 4160357289 4130952867 1430952867 1406357289 1460357289)

;; Finally, we're interested in the sum of these numbers.
(reduce #'+ special-pandigitals)
;;=> 16695334890
