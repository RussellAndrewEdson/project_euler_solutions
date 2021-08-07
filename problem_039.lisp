;; Code for Project Euler Problem 39.
;;
;; Code author: Russell A. Edson
;; Date last modified: 07/08/2021

;; We want to find the number of integral-length right triangles given
;; a perimeter p, for 2 <= p <= 1000. Suppose the sides of the triangle
;; are labelled a, b, c (WLOG a <= b, relabelling as necessary).
;; Then the right triangles are those that satisfy the Pythagorean
;; theorem:
(defun right-triangle? (a b c)
  (= (* c c) (+ (* a a) (* b b))))

;; A naive but reasonably efficient exhaustive search simply has us
;; loop over values of p, a, and b >= a, collecting up the right-angled
;; triangles as we go.
(time
 (defvar ps
   (loop for p from 2 upto 1000
	 collect
	 (list p
	       (loop for a from 1 upto p
		     append
		     (loop for b from a upto p
			   when (right-triangle? a b (- p a b))
			     collect (list a b (- p a b))))))))
;;=> Evaluation took:
;;=>   1.757 seconds of real time
;;=>   1.703125 seconds of total run time (1.703125 user, 0.000000 system)
;;=>   [ Run times consist of 0.016 seconds GC time, and 1.688 seconds non-GC time. ]
;;=>   96.93% CPU
;;=>   4,554,554,165 processor cycles
;;=>   8,093,680 bytes consed
;;=>
;;=> PS

;; All that's left is to find the value of p with the most solutions.
;; We can use Common Lisp's alexandria library and the extremum
;; function to handle this.
(ql:quickload '(:alexandria))
(alexandria:extremum ps #'> :key (lambda (pair) (length (cadr pair))))
;;=> (840
;;=>  ((40 399 401) (56 390 394) (105 360 375) (120 350 370) (140 336 364)
;;=>   (168 315 357) (210 280 350) (240 252 348)))
