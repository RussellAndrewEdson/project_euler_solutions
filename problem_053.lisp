;; Code for Project Euler Problem 53.
;;
;; Code author: Russell A. Edson
;; Date last modified: 23/08/2021

;; We want to count all of the values of nCr for 1 <= n <= 100
;; which are greater than 1 million, and we are given that n = 23
;; is the first n for which a value exceeds one million.

;; This is computationally expensive if we use the classic formula
;; nCr = n!/(r!(n-r)!). Instead, recognise that we can obtain
;; exactly these same values by constructing 100+1 rows of Pascal's
;; triangle (in a 101x101 matrix, say). In such a case we generate
;; the value in the (i,j) position from the following recurrence:
;;   Pascal(i,j) = Pascal(i-1, j-1) + Pascal(i-1, j),
;; with starting point Pascal(0, 0) = 1 at the top of the
;; triangle/matrix.
(defun pascal (m)
  "The M-row Pascal's triangle, where the nth row enumerates nCr."
  (let ((mat (make-array (list (1+ m) (1+ m)) :initial-element 0)))
    (loop for i from 0 upto m do
      (setf (aref mat i 0) 1))
    (loop for i from 1 upto m do
      (loop for j from 1 upto m do
	(setf (aref mat i j)
	      (+ (aref mat (1- i) (1- j)) (aref mat (1- i) j)))))
    mat))

(defun print-matrix (mat)
  "Pretty-print the given 2D array (matrix) MAT."
  (loop for i from 0 below (car (array-dimensions mat)) do
    (loop for j from 0 below (cadr (array-dimensions mat)) do
      (format t "~3a " (aref mat i j)))
    (format t "~%")))

(print-matrix (pascal 10))
;;=> 1   0   0   0   0   0   0   0   0   0   0
;;=> 1   1   0   0   0   0   0   0   0   0   0
;;=> 1   2   1   0   0   0   0   0   0   0   0
;;=> 1   3   3   1   0   0   0   0   0   0   0
;;=> 1   4   6   4   1   0   0   0   0   0   0
;;=> 1   5   10  10  5   1   0   0   0   0   0
;;=> 1   6   15  20  15  6   1   0   0   0   0
;;=> 1   7   21  35  35  21  7   1   0   0   0
;;=> 1   8   28  56  70  56  28  8   1   0   0
;;=> 1   9   36  84  126 126 84  36  9   1   0
;;=> 1   10  45  120 210 252 210 120 45  10  1
;;=> NIL

;; So we'll simply populate a 101x101 Pascal's triangle matrix and
;; then loop through the rows and columns, counting the values
;; that are larger than 1,000,000 (exploiting the given fact that
;; we can start our search from the 23th row).
(defvar triangle (pascal 100))

(aref triangle 23 10)
;;=> 1144066

(let ((count 0))
  (loop for i from 22 below (car (array-dimensions triangle)) do
    (loop for j from 0 below (cadr (array-dimensions triangle)) do
      (if (> (aref triangle i j) 1000000)
	  (incf count))))
  count)
;;=> 4075
