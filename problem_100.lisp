;; Code for Project Euler Problem 100.
;;
;; Code author: Russell A. Edson
;; Date last modified: 21/10/2021

;; Given a box of n > 10^12 coloured discs with m blue discs, we want
;; to find the number m such that the probability of drawing two
;; blue discs (without replacement) from the box is exactly 1/2, where
;;   P(BB) = (m/n)*((m-1)/(n-1)).
;;
;; First, we can prove some useful mathematical results that help us
;; narrow down the search space. First, note that the inequality
;;   (m-1)/(n-1) <= m/n
;; holds for all m <= n, n > 2:
;;         (m-1)/(n-1) <= m/n
;;   <==>          m-1 <= m*(n-1)/n
;;   <==>          m-1 <= m*(n-1)/m  (since m <= n, so 1/m >= 1/n)
;;   <==>          m-1 <= n-1
;;   <==>            m <= n,
;; which is true.
;;
;; So using this result, we can find a lower bound for m such that
;; P(BB) >= 1/2:
;;                       P(BB) >= 1/2
;;   <==>  (m/n)*((m-1)/(n-1)) >= 1/2
;;   <==>          (m/n)*(m/n) >= 1/2  (since (m-1)/(n-1) <= m/n)
;;   <==>              (m/n)^2 >= 1/2
;;   <==>                  m/n >= 1/sqrt(2)
;;   <==>                    m >= n/sqrt(2).
;;
;; So for P(BB) >= 1/2 we require m >= n/sqrt(2), which we can use
;; as a lower bound for m in our looping across the search space.
;;
;; Then for the upper bound for m, we can simply note that as soon as
;; P(BB) > 1/2, then P(BB) will never decrease if we keep
;; incrementing m, so we stop.
;;
;; That's the first part for m sorted. However, we are still stuck
;; with a huge search space for n if we resign to incrementing n
;; one-by-one. For now though, we'll code it up and have a play.
;; So the algorithm is as follows:
;;   1. Start at n = ?.
;;   2. Then loop for m from ceil(n/sqrt(2)):
;;   3.   If ever P(BB) = 1/2 exactly, we've found the solution.
;;        Else, if P(BB) > 1/2, break out of the loop for m.
;;        Else, increment m.
;;   4. If we didn't find the solution, increment n.
;;
;; Luckily Common Lisp's arbitrary-precision integer arithmetic
;; handles most of the hassle of this for us. To avoid floating-point
;; errors, we need not use sqrt(2) directly and instead we'll use
;; 707106781186/1000000000000 as a sufficient, 12-decimal approximation
;; to sqrt(2) to keep the computations in integer-arithmetic (with the
;; downside that we might end up performing some redundant computations
;; for P(BB) < 1/2). We code this up as follows:
(defun p-bb (m n)
  "Probability of drawing BB from M blue discs in N total discs."
  (/ (* m (1- m)) (* n (1- n))))

(defparameter sqrt2-approx (/ 707106781186 1000000000000))

;; And then our (inefficient) search is coded as follows:
(defun find-next-solution (n)
  "Finds the next m/N ratio such that P(BB) = 1/2, starting from N."
  (let ((m (ceiling (* n sqrt2-approx)))
	(solution nil))
    (loop until solution do
      (loop named inner-loop while (<= (p-bb m n) 1/2) do
	(if (= (p-bb m n) 1/2)
	    (progn
	      (setf solution (list m n))
	      (return-from inner-loop)))
	(incf m))
      (incf n)
      (setf m (ceiling (* n sqrt2-approx))))
    solution))

;; So, for example,
(defvar solutions (list (find-next-solution 2)))
(car solutions)
;;=> (3 4)

(push (find-next-solution 5) solutions)
(car solutions)
;;=> (15 21)

;; And in particular,
(time
 (loop with solution = (list 15 21)
       with n
       repeat 7 do
	 (setf n (1+ (cadr solution)))
	 (push (setf solution (find-next-solution n)) solutions)))
;;=> Evaluation took:
;;=>   5.054 seconds of real time
;;=>   5.062500 seconds of total run time (4.890625 user, 0.171875 system)
;;=>   [ Run times consist of 0.141 seconds GC time, and 4.922 seconds non-GC time. ]
;;=>   100.16% CPU
;;=>   13,100,500,985 processor cycles
;;=>   493,643,920 bytes consed
;;=>
;;=> NIL

(setf solutions (reverse solutions))
solutions
;;=> ((3 4) (15 21) (85 120) (493 697) (2871 4060) (16731 23661)
;;=>  (97513 137904) (568345 803761) (3312555 4684660))

;; We can notice some patterns about the sequence of ratios here.
;; First, the sequence does appear to be converging to 1/sqrt(2),
;; which was the lower bound we deduced earlier for m in the proof:
(mapcar (lambda (pair) (float (/ (car pair) (cadr pair)))) solutions)
;;=> (0.75 0.71428573 0.7083333 0.70731705 0.70714283 0.70711297
;;=>  0.70710784 0.70710695 0.7071068)

(/ 1 (sqrt 2))
;;=> 0.70710677

;; And second (and what allows us to cut down on the search space
;; for n!), we can notice that the denominators also seem to
;; increase at a steady (in the limit) multiplicative rate,
;; e.g. 4 -> 21 -> 120 -> 697 -> 4060, ...
(let ((denominators (mapcar #'cadr solutions)))
  (mapcar (lambda (a b) (float (/ b a)))
	  (butlast denominators)
	  (cdr denominators)))
;;=> (5.25 5.714286 5.8083334 5.824964 5.8278327 5.8283253 5.8284097
;;=>  5.828424)

;; That is, for a given n that satisfies P(BB)=1/2, the next n
;; occurs roughly 5.8284*n later (which we can compute exactly if
;; we keep track of previous denominators to compute the new ratio
;; at each step). So we can take giant leaps in our searching without
;; having to increment n one-by-one each time.

;; (Aside: this 5.8284... number -seems- to be converging to
;; (sqrt(2)+1)/(sqrt(2)-1) after a bit of a play, which is probably
;; proveable from the equations and relates nicely to the required
;; m being approximately 1/sqrt(2). We don't need to prove
;; such a thing here though, and we can take the sequence of
;; denominators as is and use them for the computational search.)
(/ (1+ (sqrt 2)) (1- (sqrt 2)))
;;=> 5.828428

;; So our search for the first m/n with n>10^12 that gives P(BB)=1/2,
;; jumping by approx. 5.8284 across n-space is coded as follows:
(time
 (let* ((prev-denominator (cadar (last (butlast solutions))))
	(new-denominator (cadar (last solutions)))
	(solution (car (last solutions))))
   (flet ((next-n (n) (floor (* n (/ new-denominator prev-denominator)))))
     (loop while (< (cadr solution) 1000000000000)
	   with n do
	     (setf n (next-n new-denominator))
	     (setf prev-denominator new-denominator)
	     (setf solution (find-next-solution n))
	     (setf new-denominator (cadr solution))))
   solution))
;;=> Evaluation took:
;;=>   0.000 seconds of real time
;;=>   0.000000 seconds of total run time (0.000000 user, 0.000000 system)
;;=>   100.00% CPU
;;=>   1,494,678 processor cycles
;;=>   65,520 bytes consed
;;=>
;;=> (756872327473 1070379110497)
