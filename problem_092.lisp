;; Code for Project Euler Problem 92.
;;
;; Code author: Russell A. Edson
;; Date last modified: 02/09/2021

;; Here we look at number chains made by continuously adding
;; the square of the digits of a number to form a new number
;; until we arrive at a number that we have seen before in the
;; chain. We are given that every starting number eventually
;; arrives at either 1 or 89, at which point it gets stuck in
;; a loop.
;;
;; We want to determine how many numbers below 10,000,000 will
;; arrive at 89. If we're clever about how we code this, then
;; we can use memoization to cut back on the computation, since
;; if we see a number that we know ultimately arrives at 1 (or 89),
;; we don't even need to finish the rest of the chain.
;;
;; So we'll first define some functions for computing the sum of
;; the squared digits in a given number:
(defun digits (number)
  "Return the set of digits in the given NUMBER."
  (mapcar #'digit-char-p (coerce (write-to-string number) 'list)))

(defun sum-of-square-digits (number)
  "Return the sum of the squares of the digits in NUMBER."
  (reduce #'+ (mapcar (lambda (x) (* x x)) (digits number))))

(sum-of-square-digits 44)
;;=> 32

(sum-of-square-digits 9999999)
;;=> 567

;; For the numbers below 10,000,000, we will always have that the
;; sum of the square digits is also less than 10,000,000. So we'll
;; define a size 10,000,000 integer array to store our ending numbers,
;; and use this as a lookup when we're looping to be efficient.
(defvar end-numbers (make-array (list 10000000)
				:element-type :integer
				:initial-element 0))
(setf (aref end-numbers (1- 1)) 1)
(setf (aref end-numbers (1- 89)) 89)

;; We then simply loop for all of the numbers below 10 million,
;; adding the square digit results to our lookup table as we go.
(time
 (progn
   (loop for n from 1 upto 10000000 do
     (let ((lookup (aref end-numbers (1- n))))
       (if (= lookup 0)
	   (let ((numbers nil)
		 (current n)
		 (current-lookup lookup))
	     (loop while (= current-lookup 0) do
	       (push current numbers)
	       (setf current (reduce #'+ (mapcar (lambda (x) (* x x))
						 (digits current))))
	       (setf current-lookup (aref end-numbers (1- current))))
	     (push current numbers)
	     (loop for number in numbers do
	       (setf (aref end-numbers (1- number)) current-lookup))))))
   (loop for n from 1 upto 10000000
	 when (= 89 (aref end-numbers (1- n)))
	   count n)))
;;=> Evaluation took:
;;=>   6.349 seconds of real time
;;=>   6.343750 seconds of total run time (4.859375 user, 1.484375 system)
;;=>   [ Run times consist of 0.953 seconds GC time, and 5.391 seconds non-GC time. ]
;;=>   99.92% CPU
;;=>   16,457,571,216 processor cycles
;;=>   4,106,648,688 bytes consed
;;=>
;;=> 8581146
