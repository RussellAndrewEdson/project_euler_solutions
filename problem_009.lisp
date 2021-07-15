;; Code for Project Euler Problem 9.
;;
;; Code author: Russell A. Edson
;; Date last modified: 15/07/2021

;; We aim to find the unique Pythagorean triplet a < b < c such that:
;;   a^2 + b^2 = c^2, and
;;   a + b + c = 1000
;; (and then the product a*b*c at the end).

;; As always, this otherwise tricky number theory problem can be
;; brute-forced with modern computation. Observe that the constraint
;; a + b + c = 1000 guarantees that we only need to check the numbers
;; 1 through 499 for each of a and b (since we can use the constraint
;; to determine c = 1000 - a - b, and c > b implies that b can be at
;; most 499).

;; So we generate a list of triples (a b (1000 - a - b)) for
;; a, b in (1, 499):
(defvar triples
  (loop for a from 1 to 499
	append
	(loop for b from 1 to 499
	      collect
	      (list a b (- 1000 a b)))))

(length triples)
;;=> 249001

;; And then it is simply a matter of filtering the triples using our
;; other constraints to wittle down our candidates. First, we require
;; a < b so we can filter to only those triples that satisfy this
;; constraint:
(setf triples (remove-if-not (lambda (triple) (< (car triple)
						 (cadr triple)))
			     triples))

(length triples)
;;=> 124251

;; Similarly, we remove the triples that don't satisfy b < c:
(setf triples (remove-if-not (lambda (triple) (< (cadr triple)
						 (caddr triple)))
			     triples))

(length triples)
;;=> 82834

;; And this is a relatively small set that we can simply check each
;; triple to see whether it satisfies a^2 + b^2 = c^2:
(defun pythagorean-triplet? (a b c)
  "True if a^2 + b^2 = c^2, False if not."
  (= (+ (* a a) (* b b)) (* c c)))

(setf triples (remove-if-not (lambda (triple)
			       (apply #'pythagorean-triplet? triple))
			     triples))

;; Voila.
(length triples)
;;=> 1

triples
;;=> ((200 375 425))

;; So finally, their product is:
(reduce #'* (car triples))
;;=> 31875000
