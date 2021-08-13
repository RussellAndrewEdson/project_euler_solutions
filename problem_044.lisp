;; Code for Project Euler Problem 44.
;;
;; Code author: Russell A. Edson
;; Date last modified: 13/08/2021

;; We are given the formula for the pentagonal numbers,
;;   P_n = n(3n-1)/2,
;; and we want to determine the pair of pentagonal numbers
;; P_j and P_k such that the sum (P_j+P_k) is also pentagonal,
;; their difference |P_j-P_k| is also pentagonal, and the
;; difference is the minimum possible among the pairs that
;; satisfy these properties.

;; Our strategy here will be to accumulate a lazy list of
;; pentagonal numbers. Note in particular that we are interested
;; in those numbers P_j and P_k (assume WLOG P_k > P_j, relabelling
;; where necessary) where P_k-P_j is also a pentagonal number.
;; Since P_k-P_j < P_k, we only ever have to search backward
;; through the list that we've already generated to check for
;; differences. If we find one, only then will we bother computing
;; their sum.
;;
;; So with that in mind, we'll construct a 'backwards' lazy list
;; using a closure so that this backward search for the difference
;; is as efficient as possible.
(defvar lazy-list-pentagonal-numbers
  (let ((pentagonal-numbers (list 1)))
    (list
     ;; Memoize returning the Nth pentagonal numnber
     (lambda (n)
       (if (<= n (length pentagonal-numbers))
	   (nth (- (length pentagonal-numbers) n) pentagonal-numbers)
	   (progn
	     (loop while (> n (length pentagonal-numbers)) do
	       (let ((m (1+ (length pentagonal-numbers))))
		 (setf pentagonal-numbers
		       (cons (/ (* m (1- (* 3 m))) 2)
			     pentagonal-numbers))))
	     (car pentagonal-numbers))))
     ;; Memoize checking if the given number is a pentagonal number
     (lambda (p)
       (flet ((backwards-search (num)
		(loop for pentagonal-num in pentagonal-numbers do
		  (cond ((= num pentagonal-num) (return t))
			((> num pentagonal-num) (return nil))
			(t nil)))))
	 (if (<= p (car pentagonal-numbers))
	     (backwards-search p)
	     (progn
	       (loop while (> p (car pentagonal-numbers)) do
		 (let ((m (1+ (length pentagonal-numbers))))
		   (setf pentagonal-numbers
			 (cons (/ (* m (1- (* 3 m))) 2)
			       pentagonal-numbers))))
	       (backwards-search p))))))))

(defun pentagonal-number (n)
  "Return the Nth pentagonal number."
  (funcall (car lazy-list-pentagonal-numbers) n))

(defun pentagonal-number? (p)
  "True if the number P is a pentagonal number."
  (funcall (cadr lazy-list-pentagonal-numbers) p))

(mapcar #'pentagonal-number (loop for n from 1 upto 10 collecting n))
;;=> (1 5 12 22 35 51 70 92 117 145)

(pentagonal-number? 146)
;;=> NIL

;; With our lazy list implemented for our pentagonal-number lookup,
;; we'll proceed as follows:
;;   1. Generate the next pentagonal number,
;;   2. Search backward to compute differences and check if these
;;      differences are also pentagonal numbers,
;;   3. Only when the difference is a pentagonal number do we bother
;;      checking if the sum is a pentagonal number.
;;
;; We need a stopping point for our looping. However we don't have much
;; of an idea for this just yet, so tentatively choose 2 < k < 5000 to
;; start with so we can see what sort of numbers we are getting first.
(time
 (defvar candidates
   (let ((pairs nil))
     (loop for k from 2 upto 5000 do
       (loop for j from (1- k) downto 1 do
	 (let ((p_k (pentagonal-number k))
	       (p_j (pentagonal-number j)))
	   (if (and (pentagonal-number? (- p_k p_j))
		    (pentagonal-number? (+ p_k p_j)))
	       (setf pairs (cons (list p_j p_k) pairs))))))
     pairs)))
;;=> Evaluation took:
;;=>   639.655 seconds of real time
;;=>   565.531250 seconds of total run time (565.046875 user, 0.484375 system)
;;=>   88.41% CPU
;;=>   1,657,987,226,724 processor cycles
;;=>   98,304 bytes consed
;;=>
;;=> CANDIDATES

candidates
;;=> ((1560090 7042750))

;; The search took a while and we only found one candidate pair,
;; which has a difference of:
(- (cadar candidates) (caar candidates))
;;=> 5482660

;; Are we convinced that this is the minimum difference? Recall that
;; we searched up to k=5000:
(pentagonal-number 5000)
;;=> 37497500

;; The next pentagonal number would be P_5001:
(pentagonal-number 5001)
;;=> 37512501

;; With the difference between them only 15001. In fact, some simple
;; mathematics shows that P_(n+1) - p_n = 3*n + 1, so we'd need to
;; search up to n = (5482660 - 1)/3 = 1827553 before we started seeing
;; consecutive differences strictly as large as this 'minimum'. Until
;; then, we'd never really be sure that we couldn't come across a
;; smaller difference if we kept computing for a little longer.
;;
;; (Luckily in our case 5482660 is the answer that Project Euler
;; was looking for, so we've saved ourselves a hefty amount of work!)
