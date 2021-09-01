;; Code for Project Euler Problem 58.
;;
;; Code author: Russell A. Edson
;; Date last modified: 01/09/2021

;; As in Problem 28, we look at patterns in spiral matrices
;; (this time the spiral is anticlockwise)
;;   37 36 35 34 33 32 31
;;   38 17 16 15 14 13 30
;;   39 18  5  4  3 12 29
;;   40 19  6  1  2 11 28
;;   41 20  7  8  9 10 27
;;   42 21 22 23 24 25 26
;;   43 44 45 46 47 48 49
;;
;; This time we want to look at the primes along the diagonal
;; and anti-diagonal. Note first that the bottom-right diagonal
;; contains the odd square numbers 1, 9, 25, 49, ... which will
;; never be prime by definition. On the other side, the top-left
;; diagonal contains numbers of the form (2k)^2+1 for k=1, 2, ...,
;; i.e. 2^2+1 = 5, 4^2+1 = 17, 6^2+1 = 37, and so on. We can
;; define a quick function to generate numbers for each of these
;; diagonals (the k = 0 case handles the 1 in the middle, which we
;; need to make sure we don't double-count):
(defun top-left (k)
  "Return the Kth number along the top-left diagonal, k >= 0."
  (1+ (expt (* 2 k) 2)))

(defun bottom-right (k)
  "Return the Kth number along the bottom-right diagonal, k >= 0."
  (expt (1+ (* 2 k)) 2))

(mapcar #'top-left '(0 1 2 3 4 5))
;;=> (1 5 17 37 65 101)

(mapcar #'bottom-right '(0 1 2 3 4 5))
;;=> (1 9 25 49 81 121)

;; Similarly for the anti-diagonal. The top-right diagonal contains
;; the numbers 1, 3, 13, 31, ..., i.e. the numbers (2k)(2k-1)+1 for
;; k=0, 1, 2, .... On the other side are the numbers 1, 7, 21, 43,
;; which are (2k)(2k+1)+1 for k=0, 1, 2, 3, and so on. We can likewise
;; code a pair of functions to compute those for given k:
(defun top-right (k)
  "Return the Kth number along the top-right diagonal, k >= 0."
  (1+ (* (* 2 k) (1- (* 2 k)))))

(defun bottom-left (k)
  "Return the Kth number along the bottom-left diagonal, k >= 0."
  (1+ (* (* 2 k) (1+ (* 2 k)))))

(mapcar #'top-right '(0 1 2 3 4 5))
;;=> (1 3 13 31 57 91)

(mapcar #'bottom-left '(0 1 2 3 4 5))
;;=> (1 7 21 43 73 111)

;; And we can relate k to the side length of the square spiral: for
;; side length m (where m must be odd), we have diagonal elements
;; up to k = (m-1)/2. So we can define a function that returns us
;; all the diagonal elements of the spiral square for a given side
;; length (and without double-counting the 1 in the middle):
(defun diagonal-elements (side-length)
  "Return the diagonal elements for the spiral square of SIDE-LENGTH."
  (unless (evenp side-length)
    (let ((k (loop for i from 1 upto (/ (1- side-length) 2) collecting i)))
      (append
       (mapcar #'top-left (cons 0 k))
       (mapcar #'top-right k)
       (mapcar #'bottom-left k)
       (mapcar #'bottom-right k)))))

(diagonal-elements 1)
;;=> (1)

(diagonal-elements 7)
;;=> (1 5 17 37 3 13 31 7 21 43 9 25 49)

;; (In the loop that we will write shortly, we will simply add in the
;; new values iteratively rather than repeatedly regenerating the list.)

;; Now we want to figure out how many of these numbers are primes and
;; monitor the ratio as we increase the side-length. We'll use a
;; straightforward trial division algorithm to count primes, noting
;; that we can memoize the process to make efficient use of the fact
;; that we'll often be computing the same primes over and over as we
;; loop for side lengths:
(defvar memoized-primes
  (let ((primes-list nil)
	(not-primes-list nil))
    (lambda (n)
      (cond ((member n primes-list) t)
	    ((member n not-primes-list) nil)
	    (t (if (<= n 1)
		   nil
		   (let ((primep t))
		     (loop for m from 2 upto (floor (sqrt n)) do
		       (if (zerop (mod n m))
			   (setf primep nil)))
		     (if primep
			 (cons n primes-list)
			 (cons n not-primes-list))
		     primep)))))))

(defun is-prime? (n)
  "True if N is a prime number."
  (funcall memoized-primes n))

;; And so, given a list of diagonal elements, we can determine the
;; ratio that are prime:
(defun prime-ratio (elements)
  "Return the ratio of primes/non-primes for the given ELEMENTS."
  (/ (loop for element in elements when (is-prime? element) count element)
     (length elements)))

(prime-ratio (diagonal-elements 7))
;;=> 8/13

;; So we want to know when the ratio of primes first falls below 1/10,
;; which we can do in a loop. Rather than using the diagonal-elements
;; or prime-ratio functions directly however, for efficiency we will
;; add each of the new diagonal elements iteratively and keep track of
;; the primes that we have.
(time
 (let* ((ratio 1)
	(side-length 1))
   (loop while (>= ratio 1/10)
	 for k = 1 then (1+ k)
	 for diagonal-elements = 5 then (+ 4 diagonal-elements)
	 with primes = 0
	 do
	    (setf side-length (+ 2 side-length))
	    (if (is-prime? (top-left k)) (incf primes))
	    (if (is-prime? (top-right k)) (incf primes))
	    (if (is-prime? (bottom-left k)) (incf primes))
	    (if (is-prime? (bottom-right k)) (incf primes))
	    (setf ratio (/ primes diagonal-elements)))
   (list side-length ratio)))
;;=> Evaluation took:
;;=>   12.634 seconds of real time
;;=>   12.625000 seconds of total run time (12.625000 user, 0.000000 system)
;;=>   99.93% CPU
;;=>   32,746,201,555 processor cycles
;;=>   425,984 bytes consed
;;=>
;;=> (26241 5248/52481)
