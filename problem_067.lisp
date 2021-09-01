;; Code for Project Euler Problem 67.
;;
;; Code author: Russell A. Edson
;; Date last modified: 01/09/2021

;; As in Problem 18, we are given a triangle of numbers and are
;; asked to find the maximum path sum. We first read in the triangle
;; by downloading triangle.txt from the Project Euler Problem page:
(ql:quickload '(:dexador :str))
(defparameter text-file
  "https://projecteuler.net/project/resources/p067_triangle.txt")
(defparameter triangle-rows
  (str:split #\newline
	     (str:trim (dex:get text-file))))

(defvar triangle
  (mapcar (lambda (row)
	    (mapcar #'parse-integer (str:split #\space row)))
	  triangle-rows))

;; We can get the last row of the triangle as follows:
(car (last triangle))
;;=> (23 33 44 81 80 92 93 75 94 88 23 61 39 76 22 3 28 94 32 6 49 65
;;=>  41 34 18 23 8 47 62 60 3 63 33 13 80 52 31 54 73 43 70 26 16 69
;;=>  57 87 83 31 3 93 70 81 47 95 77 44 29 68 39 51 56 59 63 7 25 70
;;=>  7 77 43 53 64 3 94 42 95 39 18 1 66 21 16 97 20 50 90 16 70 10
;;=>  95 69 29 6 25 61 41 26 15 59 63 35)

;; Now the same algorithm we used in Problem 18 will not work here,
;; since we cannot enumerate all 2^99 paths to maximise the sums.
;; However, observe the following interesting recursive relation: the
;; maximum path sum starting at row i, column j, will be the maximum
;; of the path sums of the two triangles immediately beneath that point,
;; i.e. the triangle with tip at row (i-1) column j, and the triangle
;; with tip at row (i-1) column (j+1). The base case for the recursion
;; are the 'unit triangles' that populate the bottom row of the original
;; triangle.
;;
;; Recursively this is still inefficient since we'd have a prohibitive
;; number of stack frames before the recursion unwinds. However, since
;; we are only concerned with the final sums, we can actually start at
;; the bottom row of the triangle. We'll work our way up the triangle
;; with a 'stagger then max' strategy.
;;
;; For example, consider the given 4-row triangle
;;   3
;;   7 4
;;   2 4 6
;;   8 5 9 3
;;
;; Starting from the bottom row, we first 'stagger' so that every
;; adjacent pair occurs:
;;   [8 5 9 3] -> [(8 5) (5 9) (9 3)].
;; (This staggering allows us to account for left-and-right paths.)
;; We then pick the maximum out of each of the pairs:
;;   [(8 5) (5 9) (9 3)] -> [8 9 9]
;; And then we add the next row:
;;   [8 9 9] + [2 4 6] -> [10 13 15].
;; We then repeat this process of 'staggering', maxing and adding
;; until we arrive at the top of the triangle. (If you like, we are
;; effectively doing the recursion in reverse.)
;;
;; So we'll define a utility function to stagger for the algorithm:
(defun stagger (list)
  "'Stagger' through LIST by duplicating elements in adjacent pairs."
  (if (<= (length list) 2)
      list
      (loop for (element . rest) on list
	    while (not (null (car rest)))
	    collect (list element (car rest)))))

(stagger '(1 2 3 4 5 6 7))
;;=> ((1 2) (2 3) (3 4) (4 5) (5 6) (6 7))

;; And then stagger, max and sum our way up the triangle according
;; to the procedure we outlined above. When we reach the top, we
;; will have the maximum path sum (albeit not the actual path itself).
(time
 (let ((sum (stagger (car (last triangle)))))
   (loop for row in (cdr (reverse triangle)) do
     (setf sum (mapcar (lambda (pair)
			 (if (listp pair)
			     (apply #'max pair)
			     pair))
		       sum))
     (setf sum (stagger (mapcar #'+ sum row))))
   (car sum)))
;;=> Evaluation took:
;;=>   0.001 seconds of real time
;;=>   0.000000 seconds of total run time (0.000000 user, 0.000000 system)
;;=>   0.00% CPU
;;=>   2,833,456 processor cycles
;;=>   424,656 bytes consed
;;=>
;;=> 7273
