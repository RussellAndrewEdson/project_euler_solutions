;; Code for Project Euler Problem 24.
;;
;; Code author: Russell A. Edson
;; Date last modified: 30/07/2021

;; We are interested in lexicographic permutations, so first we want
;; a function that takes a list (of digits) and returns all possible
;; permutations. If we're smart about the way we code this, we can
;; make it return the lexicographic order at the same time:
(defun permutations (list)
  "Return all permutations of the digits in LIST in lexicographic order."
  (defun permutations-recur (sublist)
    (cond ((null (cdr sublist)) (list sublist))
	  (t (loop for element in sublist
		   append
		   (mapcar (lambda (rest) (append (list element) rest))
			   (permutations-recur (remove element sublist)))))))
  (permutations-recur (sort list #'<)))

(permutations '(0 1 2))
;;=> ((0 1 2) (0 2 1) (1 0 2) (1 2 0) (2 0 1) (2 1 0))

;; We want the permutations of the digits 0 through 9. This operation
;; is reasonably quick but very expensive space-wise (2GB consed!):
(time
 (defvar 0-9-permutations (permutations '(0 1 2 3 4 5 6 7 8 9))))
;;=> Evaluation took:
;;=>   12.759 seconds of real time
;;=>   11.078125 seconds of total run time (5.843750 user, 5.234375 system)
;;=>   [ Run times consist of 8.251 seconds GC time, and 2.828 seconds non-GC time. ]
;;=>   86.82% CPU
;;=>   33,069,939,636 processor cycles
;;=>   2,347,779,408 bytes consed
;;=> 
;;=> 0-9-PERMUTATIONS

;; The millionth lexicographic permutation is:
(nth (1- 1000000) 0-9-permutations)
;;=> (2 7 8 3 9 1 5 4 6 0)
