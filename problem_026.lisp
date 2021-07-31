;; Code for Project Euler Problem 26.
;;
;; Code author: Russell A. Edson
;; Date last modified: 31/07/2021

;; We look at reciprocal cycles in unit fractions, e.g.
;; 1/7 = 0.(142857), where the (142857) part repeats infinitely.
;; In lieu of 'arbitrary-precision floats', our first step is to
;; write a function that, given a unit fraction and some desired
;; length, returns a decimal representation of that fraction
;; up to that many decimal points in length.
;;
;; We do this as follows. Observe the following calculations:
;; 10/7 = 1.428571...
;; 10*(10/7 - 1*7/7) = 4.285714...
;; 10*(10*(10/7 - 1*7/7) - 4*7/7) = 2.857142...
;; 10*(10*(10*(10/7 - 1*7/7) - 4*7/7) -2*7/7) = 8.571428...
;;
;; Each time we subtract off the unitary part and multiply by 10,
;; then the new number in the units column is the next digit.
;; Because Common Lisp has arbitrary-length integers and rationals,
;; this works fine for our purposes here.
(defun decimal-representation (d &optional (decimal-length 20))
  "Return a string representation of 1/D with the given DECIMAL-LENGTH."
  (let* ((frac (/ 1 d))
	 (unit-part (truncate frac))
	 (digits (list unit-part ".")))
    (loop repeat decimal-length do
      (setf frac (* 10 (- frac unit-part)))
      (setf unit-part (truncate frac))
      (setf digits (append digits (list unit-part))))
    (format nil "~{~a~}" digits)))

(decimal-representation 7)
;;=> "0.14285714285714285714"

(decimal-representation 19 50)
;;=> "0.05263157894736842105263157894736842105263157894736"

;; With that task complete, our next job is to write a function that,
;; given one of these strings, works out how long the recurring cycle
;; is in the decimal expansion. We can do this in a jiffy with
;; regular expressions. The regular expression (.+?)\\1 matches the
;; smallest repeated substring. When we scan the string for this match
;; it will return both the substring and its duplicate, so we simply
;; take the first half to get the repeated part.
(ql:quickload '(:cl-ppcre))
(defun cycle-length (decimal-string)
  "Return the length of the longest recurring cycle in DECIMAL-STRING."
  (let ((match-indices (cl-ppcre:all-matches "(.+?)\\1" decimal-string)))
    (/ (apply #'max
	      (mapcar (lambda (pair) (reduce #'- pair))
		      (loop for (start end) on match-indices by #'cddr
			    collecting (list end start))))
       2)))

(cycle-length "0.172913413413413413")
;;=> 3

(cycle-length (decimal-representation 6))
;;=> 1

(cycle-length (decimal-representation 7))
;;=> 6

;; Finally, we simply gets the cycle lengths for the decimal expansions
;; of all of the fractions 1/d for 1 < d < 1000 and return the largest.
;; (We'll truncate the decimal expansions at 2000 decimal places, which
;; should suffice for us here.)
(defvar fraction-cycle-lengths
  (loop for d from 1 below 1000
	collecting
	(list d (cycle-length (decimal-representation d 2000)))))

(setf fraction-cycle-lengths
      (sort fraction-cycle-lengths #'< :key #'cadr))
(last fraction-cycle-lengths)
;;=> ((983 982))
