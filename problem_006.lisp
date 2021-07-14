;; Code for Project Euler Problem 6.
;;
;; Code author: Russell A. Edson
;; Date last modified: 14/07/2021

;; We want the difference between the sum of squares a^2 + b^2 + ...
;; and the squared sum (a + b + ...)^2 for the first 100 natural
;; numbers. This is a straightforward function composition.

(defun square (x)
  "Return the square of X."
  (* x x))

(defun sum (nums)
  "Return the sum of the numbers in NUMS."
  (reduce #'+ nums))

(defun sum-of-squares (nums)
  "Return the sum of the squares for the numbers in the NUMS list."
  (sum (mapcar #'square nums)))

(defun square-of-sum (nums)
  "Return the square of the sum of the numbers in the NUMS list."
  (square (sum nums)))

(defvar 1-to-10 (loop for n from 1 to 10 collect n))

(sum-of-squares 1-to-10)
;;=> 385

(square-of-sum 1-to-10)
;;=> 3025

(let ((1-to-100 (loop for n from 1 to 100 collect n)))
  (- (square-of-sum 1-to-100) (sum-of-squares 1-to-100)))
;;=> 25164150
