;; Code for Project Euler Problem 85.
;;
;; Code author: Russell A. Edson
;; Date last modified: 19/10/2021

;; We want to find the area of the grid that contains close to 2 million
;; rectangles. Now we are given through an example that the 2x3 grid
;; contains 18 rectangles:
;;   6 1x1 rectangles,
;;   4 1x2 rectangles,
;;   2 1x3 rectangles,
;;   3 2x1 rectangles,
;;   2 2x2 rectangles,
;;   1 2x3 rectangle.
;;
;; Notice that we can count the rectangles by first counting all of the
;; 2-row rectangles (of which there are 1+2+3), and then counting all
;; of the 1-row rectangles (of which there are also 1+2+3), but since
;; we have two rows that can fit these 1-row rectangles, we multiply
;; this sum by 2 for this next step.
;;
;; This idea generalises. Consider the mxn rectangle. We count the m-row
;; rectangles (of which there are 1+2+...+n, or n(n+1)/2. Then there
;; are 2 lots of (m-1)-row rectangles, 3 lots of (m-2)-row rectangles,
;; and so on.
;; So, the number of rectangles in an mxn grid is exactly
;;   1*(n(n+1)/2) + 2*(n(n+1)/2) + ... + m*(n(n+1)/2),
;; or succinctly,
;;   m(m+1)/2*n(n+1)/2.
;;
;; We can define a function to count the rectangles as such:
(defun rectangles-count (m n)
  "Return the number of rectangles in the MxN grid."
  (/ (* m (1+ m) n (1+ n)) 4))

(rectangles-count 2 3)
;;=> 18

;; Our problem now becomes one of nonlinear integer programming: we seek
;; to minimise |2,000,000 - m(m+1)/2*n(n+1)/2| subject to integer m, n.
;; We need not be too sophsticated here, however. Observe that we can
;; find a useful bound for m and n by exploiting the symmetry of the
;; solution space and solving for (continuous) n given m=1:
;;        2,000,000 - 1(2)/2*n(n+1)/2 = 0
;;   ==>  2,000,000 - n(n+1)/2 = 0
;;   ==>  n = sqrt(16000001)/2 - 1/2
;;   ==>  n ~= 2000.
(rectangles-count 1 2000)
;;=> 2001000

;; So m = 1, n = 2000 and m = 2000, n = 1 are our bounds and the solution
;; space is inherently symmetric, we can naively search for all
;; 1 <= m <= 2000, m <= n <= 2000 to find our desired minimum in a
;; reasonably efficient manner.
(time
 (let ((minimum-m-n (list 1 1))
       (minimum (abs (- 2000000 (rectangles-count 1 1)))))
   (loop for m from 1 upto 2000 do
     (loop for n from m upto 2000 do
       (let ((current-m-n (list m n))
	     (current (abs (- 2000000 (rectangles-count m n)))))
	 (if (<= current minimum)
	     (progn
	       (setf minimum-m-n current-m-n)
	       (setf minimum current))))))
   (list minimum-m-n minimum)))
;;=> Evaluation took:
;;=>   0.151 seconds of real time
;;=>   0.156250 seconds of total run time (0.062500 user, 0.093750 system)
;;=>   [ Run times consist of 0.094 seconds GC time, and 0.063 seconds non-GC time. ]
;;=>   103.31% CPU
;;=>   392,499,987 processor cycles
;;=>   64,012,112 bytes consed
;;=>
;;=> ((36 77) 2)

;; So the 36x77 grid (or symmetrically, the 77x36 grid) contains closest
;; to 2,000,000 rectangles.
(rectangles-count 36 77)
;;=> 1999998

;; The area of the grid is:
(* 36 77)
;;=> 2772
