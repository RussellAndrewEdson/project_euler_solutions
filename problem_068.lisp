;; Code for Project Euler Problem 68.
;;
;; Code author: Russell A. Edson
;; Date last modified: 28/09/2021

;; For this problem we want to find the maximum 16-digit string
;; solution to the 'magic 5-gon ring'. Denote the nodal numbers
;; of the 5-gon ring by a1, a2, ..., a10, e.g. pictorially:
;;
;;                        a1
;;                          \
;;                           a2     a4
;;                          /  \   /
;;                       a9      a3
;;                      / \      /
;;                   a10   a7 - a5 --- a6
;;                          \
;;                           a8
;;
;; Now since we are after 16-digit strings (and not 17-digit strings),
;; this means that the two-digit 10 number must appear on one of the
;; 'spokes' a1, a4, a6, a8 or a10. WLOG, suppose a10 = 10 (since we
;; can rotate the 5-gon until this is true).
;;
;; Then our string-form for 5-gon solutions looks like
;;    a1,a2,a3; a4,a3,a5; a6,a5,a7; a8,a7,a9; 10,a9,a2
;; (where we cycle through the triples until the smallest of a1, a4,
;; a6 and a8 appears at the front), and we find all a1,a2,...,a9
;; that satisfy the following linear equations:
;;   a1 + a2 + a3 = N
;;   a3 + a4 + a5 = N
;;   a5 + a6 + a7 = N
;;   a7 + a8 + a9 = N
;;   a2 + a9 + 10 = N
;; for some given line total N.

;; Now we need some bounds for N. Since 10 must appear in one of the
;; lines, and the digits are unique, then the smallest possible N
;; is exactly 1 + 2 + 10 = 13. By a similar (albeit lazy) reasoning,
;; the largest possible N is 8 + 9 + 10 = 27, although this N has
;; exactly zero solutions so a tighter upper bound is readily possible.

;; The crux of our program is therefore as follows: we'll loop for
;; N between 13 and 27 and loop for all permutations of a1,...,a9
;; in the digits 1-9, collecting up the solutions that we find.
;; Once the solutions have been found we'll reorganize them into
;; the normal form (with the smallest 'spoke' number appearing first
;; in the string), and then grab the maximum.

;; We first code a function to test for a solution:
(defun solution? (a n)
  "True if the list A = (a1 a2 ... a9) is a solution for total = N."
  (and
   (= n (+ (first a) (second a) (third a)))
   (= n (+ (third a) (fourth a) (fifth a)))
   (= n (+ (fifth a) (sixth a) (seventh a)))
   (= n (+ (seventh a) (eighth a) (ninth a)))
   (= n (+ (second a) (ninth a) 10))))

;; We'll bring in the cl-permutation library to handle the looping
;; across solutions (i.e. permutations of the digits 1 through 9),
;; and then simply collect up the solutions that we find.
(ql:quickload :cl-permutation)

(defvar solutions nil)
(time
 (progn
   (loop for n from 13 upto 27 do
     (cl-permutation:doperms (permutation 9)
       (let ((candidate (cl-permutation:perm-to-list permutation)))
	 (if (solution? candidate n)
	     (push (list n candidate) solutions)))))
   solutions))
;;=> Evaluation took:
;;=>   4.472 seconds of real time
;;=>   4.468750 seconds of total run time (4.250000 user, 0.218750 system)
;;=>   [ Run times consist of 0.191 seconds GC time, and 4.278 seconds non-GC time. ]
;;=>   99.93% CPU
;;=>   11,592,901,893 processor cycles
;;=>   1,480,557,616 bytes consed
;;=>
;;=> ((16 (2 5 9 4 3 6 7 8 1)) (16 (8 1 7 6 3 4 9 2 5))
;;=>  (14 (6 3 5 7 2 8 4 9 1)) (14 (9 1 4 8 2 7 5 6 3)))

;; These are our solutions in the form a1,a2,...,a9, but we want
;; these in normal form, i.e. one of
;;   a1,a2,a3, a4,a3,a5, a6,a5,a7, a8,a7,a9, 10,a9,a2
;;   a4,a3,a5, a6,a5,a7, a8,a7,a9, 10,a9,a2, a1,a2,a3
;;   a6,a5,a7, a8,a7,a9, 10,a9,a2, a1,a2,a3, a4,a3,a5
;;   a8,a7,a9, 10,a9,a2, a1,a2,a3, a4,a3,a5, a6,a5,a7
;; depending on which of a1, a4, a6 or a8 is smaller.
(defun a1-a9->solution-form (a)
  "Return the solution normal form for A=(a1 a2 ... a9)."
  (multiple-value-bind (a1 a2 a3 a4 a5 a6 a7 a8 a9) (values-list a)
    (let* ((min-a (min a1 a4 a6 a8))
	   (min-index (1+ (position min-a a))))
      (values
       (parse-integer
	(format
	 nil
	 "~{~d~}"
	 (cond ((= min-index 1)
		(list a1 a2 a3 a4 a3 a5 a6 a5 a7 a8 a7 a9 10 a9 a2))
	       ((= min-index 4)
		(list a4 a3 a5 a6 a5 a7 a8 a7 a9 10 a9 a2 a1 a2 a3))
	       ((= min-index 6)
		(list a6 a5 a7 a8 a7 a9 10 a9 a2 a1 a2 a3 a4 a3 a5))
	       (t
		(list a8 a7 a9 10 a9 a2 a1 a2 a3 a4 a3 a5 a6 a5 a7)))))))))

(mapcar (lambda (solution) (a1-a9->solution-form (cadr solution)))
	solutions)
;;=> (2594936378711015 2951051817673439 6357528249411013
;;=>  6531031914842725)

;; By inspection, the maximum string is 6531031914842725, so we're done.
