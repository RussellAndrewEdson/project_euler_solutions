;; Code for Project Euler Problem 96.
;;
;; Code author: Russell A. Edson
;; Date last modified: 22/10/2021

;; In this problem we want to solve a given set of 50 Sudoku puzzles
;; and sum up the 3-digit numbers at the top of the top-left square
;; in each solution grid. (So effectively we want to code a Sudoku
;; solver.)

;; The given Sudoku problems are downloaded directly from the
;; Project Euler problem page and parsed into a list appropriately:
(ql:quickload '(:dexador :str :cl-ppcre))
(defparameter text-file
  "https://projecteuler.net/project/resources/p096_sudoku.txt")
(defparameter sudokus
  (mapcar (lambda (puzzle-text)
	    (mapcar (lambda (row)
		      (mapcar #'parse-integer
			      (cl-ppcre:split "" row)))
		    (str:split #\newline puzzle-text)))
	  (mapcar #'str:trim
		  (cdr (cl-ppcre:split "Grid \\d+\\s+" (dex:get text-file))))))

(defparameter example-sudoku (nth 0 sudokus))
example-sudoku
;;=> ((0 0 3 0 2 0 6 0 0)
;;=>  (9 0 0 3 0 5 0 0 1)
;;=>  (0 0 1 8 0 6 4 0 0)
;;=>  (0 0 8 1 0 2 9 0 0)
;;=>  (7 0 0 0 0 0 0 0 8)
;;=>  (0 0 6 7 0 8 2 0 0)
;;=>  (0 0 2 6 0 9 5 0 0)
;;=>  (8 0 0 2 0 3 0 0 9)
;;=>  (0 0 5 0 1 0 3 0 0))

;; Note that we need not be so clever here: we can simply throw
;; computation at the puzzle to solve it in a jiffy even if our
;; approach would be hilariously inefficient for solving by
;; pen-and-paper. Indeed, our strategy will be to code up some
;; functions for finding placeholders and determining feasible moves,
;; and then we simply (and recursively) solve the Sudoku puzzle by
;; testing every feasible move until we find the combination that works.

;; First, we want to find the indices of the first placeholder
;; (which are given to us as 0s here):
(defun first-placeholder (puzzle)
  "Return the (i,j) index of the first placeholder in PUZZLE."
  (let ((index nil))
    (block grid-scan
      (loop for i from 0 below 9 do
	(loop for j from 0 below 9 do
	  (if (= (nth j (nth i puzzle)) 0)
	      (progn
		(setf index (list i j))
		(return-from grid-scan))))))
    index))

(first-placeholder example-sudoku)
;;=> (0 0)

;; Note that if we cannot find a placeholder (i.e. FIRST-PLACEHOLDER
;; ever returns nil), then we've found the solution.

;; Next, we want a function that, given an index and a puzzle, returns
;; a list of all of the feasible numbers. (If there are ever none, then
;; we've made a mistake somewhere and we have to backtrack.) For this,
;; we want ancillary functions for finding the numbers in the same row,
;; same column, and same 3x3 square.
(defun row-numbers (puzzle index)
  "Return the numbers in the same row as INDEX in PUZZLE."
  (nth (car index) puzzle))

(row-numbers example-sudoku '(2 3))
;;=> (0 0 1 8 0 6 4 0 0)

(defun column-numbers (puzzle index)
  "Return the numbers in the same column as INDEX in PUZZLE."
  (mapcar (lambda (row) (nth (cadr index) row)) puzzle))

(column-numbers example-sudoku '(2 3))
;;=> (0 3 8 1 0 7 6 2 0)

(defun square-numbers (puzzle index)
  "Return the numbers in the same 3x3 square as INDEX in PUZZLE."
  (let ((i (car index))
	(j (cadr index)))
    (setf i (cond ((< i 3) 0) ((< i 6) 3) (t 6)))
    (setf j (cond ((< j 3) 0) ((< j 6) 3) (t 6)))
    (append
     (subseq (nth i puzzle) j (+ 3 j))
     (subseq (nth (+ 1 i) puzzle) j (+ 3 j))
     (subseq (nth (+ 2 i) puzzle) j (+ 3 j)))))

(square-numbers example-sudoku '(2 3))
;;=> (0 2 0 3 0 5 8 0 6)

(defun feasible-numbers (puzzle index)
  "Return a list of feasble numbers at INDEX in PUZZLE."
  (reduce #'set-difference
	  (list
	   '(1 2 3 4 5 6 7 8 9)
	   (row-numbers puzzle index)
	   (column-numbers puzzle index)
	   (square-numbers puzzle index))))

(feasible-numbers example-sudoku (first-placeholder example-sudoku))
;;=> (5 4)

;; And so our recursive solution function is as follows:
(defun solve (puzzle)
  "Solve the Sudoku PUZZLE."
  (let ((next-index (first-placeholder puzzle)))
    (if (null next-index)
	puzzle
	(let ((i (car next-index))
	      (j (cadr next-index))
	      (test-numbers (feasible-numbers puzzle next-index)))
	  (if (null test-numbers)
	      nil
	      (remove-if
	       #'null
	       (loop for number in test-numbers
		     append
		     (let ((new-puzzle (mapcar #'copy-list puzzle)))
		       (setf (nth j (nth i new-puzzle)) number)
		       (solve new-puzzle)))))))))

(time (solve example-sudoku))
;;=> Evaluation took:
;;=>   0.001 seconds of real time
;;=>   0.015625 seconds of total run time (0.015625 user, 0.000000 system)
;;=>   0.00% CPU
;;=>   3,488,998 processor cycles
;;=>   1,900,512 bytes consed
;;=>
;;=> ((4 8 3 9 2 1 6 5 7)
;;=>  (9 6 7 3 4 5 8 2 1)
;;=>  (2 5 1 8 7 6 4 9 3)
;;=>  (5 4 8 1 3 2 9 7 6)
;;=>  (7 2 9 5 6 4 1 3 8)
;;=>  (1 3 6 7 9 8 2 4 5)
;;=>  (3 7 2 6 8 9 5 1 4)
;;=>  (8 1 4 2 5 3 7 6 9)
;;=>  (6 9 5 4 1 7 3 8 2))

;; So we can solve all of the Sudoku puzzles all at once:
(time
 (defparameter solved-puzzles (mapcar #'solve sudokus)))
;;=>  Evaluation took:
;;=>    2.872 seconds of real time
;;=>    2.890625 seconds of total run time (2.562500 user, 0.328125 system)
;;=>    [ Run times consist of 0.360 seconds GC time, and 2.531 seconds non-GC time. ]
;;=>    100.66% CPU
;;=>    7,444,034,843 processor cycles
;;=>    3,965,446,848 bytes consed

;; And we can get at the sum of the first three digits of each solved
;; Sudoku as follows:
(reduce #'+
	(mapcar (lambda (puzzle)
		  (parse-integer
		   (format nil "~{~d~}" (subseq (nth 0 puzzle) 0 3))))
		solved-puzzles))
;;=> 24702
