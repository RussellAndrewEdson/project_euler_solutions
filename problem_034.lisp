;; Code for Project Euler Problem 34.
;;
;; Code author: Russell A. Edson
;; Date last modified: 07/08/2021

;; We seek the sum of all numbers that are equal to the sum of
;; the factorial of their digits. So the first step is to code
;; functions for extracting digits from a given number, and
;; computing the factorial:
(defun digits (number)
  "Return the set of digits in the given NUMBER."
  (mapcar #'digit-char-p (coerce (write-to-string number) 'list)))

(digits 12345)
;;=> (1 2 3 4 5)

(defun factorial (n)
  "Return the factorial of the given natural number N."
  (cond ((= n 0) 1)
	((= n 1) 1)
	(t (let ((product 1))
	     (loop for i from 2 upto n do
	       (setf product (* product i)))
	     product))))

(factorial 10)
;;=> 3628800

;; Now we want to find all numbers that are equal to the sum
;; of the factorial of their digits. We will loop for this,
;; but what's the stopping point?
;;
;; Consider the following:
(factorial 9)
;;=> 362880

;; So 9! = 362880, i.e. the largest factorial for a digit.
;; Then we have:
;;   2*9! = 725760
;;   3*9! = 1088640
;;   4*9! = 1451520
;;   5*9! = 1814400
;;   6*9! = 2177280
;;   7*9! = 2540160
;;   8*9! = 2903040
;; That is, the largest possible number you can make from
;; the sum of factorials of an 8-digit number is only
;; a 7-digit number. So no 8-digits numbers can ever be
;; equal to the sum of their digits (and most of the 7-digit
;; numbers are also out). We only need to loop up to 2540160.
;;
;; So for readability we'll code a function to sum up the
;; factorials of the digits of a given number, and then loop
;; to collect the numbers that are equal to this sum.
(defun sum-of-digit-factorials (number)
  "Return the sum of the factorials of the digits of NUMBER."
  (reduce #'+ (mapcar #'factorial (digits number))))

(sum-of-digit-factorials 145)
;;=> 145

(time
 (defvar the-numbers
   ;; e.g. ignoring 1! = 1 and 2! = 2 as they are not sums
   (loop for n from 10 upto (* 7 (factorial 9))
	 when (= n (sum-of-digit-factorials n))
	   collect n)))
;;=> Evaluation took:
;;=>   1.901 seconds of real time
;;=>   1.875000 seconds of total run time (1.640625 user, 0.234375 system)
;;=>   [ Run times consist of 0.141 seconds GC time, and 1.734 seconds non-GC time. ]
;;=>   98.63% CPU
;;=>   4,927,042,935 processor cycles
;;=>   922,090,336 bytes consed
;;=>
;;=> THE-NUMBERS

the-numbers
;;=> (145 40585)

(reduce #'+ the-numbers)
;;=> 40730
