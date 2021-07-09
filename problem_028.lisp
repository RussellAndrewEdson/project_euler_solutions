;; Code for Project Euler Problem 28.
;;
;; Code author: Russell A. Edson
;; Date last modified: 09/07/2021

;; Observe the patterns in the 7x7 spiral matrix:
;;   43 44 45 46 47 48 49
;;   42 21 22 23 24 25 26
;;   41 20  7  8  9 10 27
;;   40 19  6  1  2 11 28
;;   39 18  5  4  3 12 29
;;   38 17 16 15 14 13 30
;;   37 36 35 34 33 32 31
;;
;; The top-right diagonal contains 1, 9, 25, 49: i.e. i^2, for i=1,7.
;; The bottom-left diagonal contains 5, 17, 37:
;;   i.e. (2^2+1), (4^2+1), (6^2+1).
;; Summing the numbers across these two diagonals (the anti-diagonal)
;; gives us 1^2 + 2^2 + ... + 7^2 + floor(n/2).
;;
;; Similarly, the top-left and bottom-right diagonal together give us
;; the numbers 1, 3, 7, 13, 21, 31, 43, which are i^2-i+1 for i=1,7.
;; So for general n the total sum is
;;   sum^n_{i=1} i^2 + floor(n/2) + sum^n_{i=1} (i^2-i+1) - 1.

;; A function to return the closed-form solution is:
(defun spiral-sum (n)
  "Return the sum across the diagonals for the NxN spiral matrix."
  (+ (loop for i from 1 to n summing (expt i 2))
     (floor (/ n 2))
     (loop for i from 1 to n summing (+ (expt i 2) (- i) 1))
     -1))

(spiral-sum 1001)
;;=> 669171001

;; Alternatively we just build the matrix and sum the diagonal and
;; anti-diagonal manually to arrive at the same answer.
(defun spiral-matrix (n)
  "Return the NxN spiral matrix."
  (let ((upper (1- n))
	(lower 0)
	(value (1+ (expt n 2)))
	(mat (make-array (list n n))))
    (loop while (> upper lower) do
      (loop for j from upper downto lower do
	(setf (aref mat lower j) (decf value)))
      (loop for i from (1+ lower) to upper do
	(setf (aref mat i lower) (decf value)))
      (loop for j from (1+ lower) to upper do
	(setf (aref mat upper j) (decf value)))
      (incf lower)
      (loop for i from (1- upper) downto lower do
	(setf (aref mat i upper) (decf value)))
      (decf upper))
    (setf (aref mat lower lower) 1)
    mat))

(defun print-matrix (mat)
  "Pretty-print the given 2D array (matrix) MAT."
  (loop for i from 0 below (car (array-dimensions mat)) do
    (loop for j from 0 below (cadr (array-dimensions mat)) do
      (format t "~3a " (aref mat i j)))
    (format t "~%")))

(print-matrix (spiral-matrix 9))
;;=> 73  74  75  76  77  78  79  80  81
;;=> 72  43  44  45  46  47  48  49  50
;;=> 71  42  21  22  23  24  25  26  51
;;=> 70  41  20  7   8   9   10  27  52
;;=> 69  40  19  6   1   2   11  28  53
;;=> 68  39  18  5   4   3   12  29  54
;;=> 67  38  17  16  15  14  13  30  55
;;=> 66  37  36  35  34  33  32  31  56
;;=> 65  64  63  62  61  60  59  58  57

;; So we sum up the diagonals of the explicitly-created 1001x1001 spiral
;; matrix, and then subtract 1 because we count the 'middle' element
;; twice in the diagonal summation.
(defun sum-diagonals (mat)
  "Sum the diagonal and anti-diagonal of the 2D square matrix MAT."
  (let ((n (car (array-dimensions mat))))
    (loop for i from 0 below n
	  summing (+ (aref mat i i)
		     (aref mat i (- (1- n) i))))))

(1- (sum-diagonals (spiral-matrix 1001)))
;;=> 669171001
