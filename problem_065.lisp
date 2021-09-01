;; Code for Project Euler Problem 65.
;;
;; Code author: Russell A. Edson
;; Date last modified: 01/09/2021

;; As in Problem 57, here we are interested in continued
;; fractions and convergents, this time for the irrational
;; number e. We'll build on the continued fraction
;; scaffolding we set up in that problem.

;; Now we are given that the continued fraction for e is
;;   [2; 1, 2, 1, 1, 4, 1, ..., 1, 2k, 1, ...],
;; so we can use this to form a generator function using a
;; closure that will generate successive values in this
;; sequence:
(defvar next-digit-for-e
  (let ((n 0))
    (lambda ()
      (incf n)
      (cond ((= n 1) 2)
	    ((zerop (mod n 3)) (* 2 (/ n 3)))
	    (t 1)))))

(defun next-digit-for-e ()
  "Return the next digit in the continued fraction for e."
  (funcall next-digit-for-e))

;; We can then embed that in a generalised form of the
;; continued-fraction pattern that we developed in Problem 57.
(defun make-continued-fraction (generator)
  "Return the continued fraction whose digits are made by GENERATOR."
  (let ((terms nil))
    (lambda (n)
      (if (>= n (length terms))
	  (loop while (>= n (length terms)) do
	    (setf terms (append terms (list (funcall generator))))))
      (subseq terms 0 (1+ n)))))

(defvar continued-fraction-for-e (make-continued-fraction #'next-digit-for-e))

(defun continued-fraction-for-e (n)
  "Return [a0; a1, a2, ..., aN] for the continued fraction for e."
  (funcall continued-fraction-for-e n))

(continued-fraction-for-e 5)
;;=> (2 1 2 1 1 4)

(continued-fraction-for-e 20)
;;=> (2 1 2 1 1 4 1 1 6 1 1 8 1 1 10 1 1 12 1 1 14)

;; We can then use the (truncated) continued fraction terms to
;; generate convergents for e:
(defun convergent (continued-fraction)
  "Return the convergent formed by evaluating CONTINUED-FRACTION."
  (let* ((denominators (reverse continued-fraction))
	 (result (car denominators)))
    (loop for number in (cdr denominators) do
      (setf result (+ (/ 1 result) number)))
    result))

(mapcar (lambda (n) (convergent (continued-fraction-for-e n)))
	(loop for n from 0 below 10 collecting n))
;;=> (2 3 8/3 11/4 19/7 87/32 106/39 193/71 1264/465 1457/536)

;; So the 100th convergent (found by taking a0, a1, ..., a99 in
;; the continued fraction terms) is:
(defvar 100th-convergent (convergent (continued-fraction-for-e 99)))

100th-convergent
;;=> 6963524437876961749120273824619538346438023188214475670667/
;;=>2561737478789858711161539537921323010415623148113041714756

;; We can then easily extract and sum the digits in the numerator.
(defun digits (number)
  "Return the set of digits in the given NUMBER."
  (mapcar #'digit-char-p (coerce (write-to-string number) 'list)))

(reduce #'+ (digits (numerator 100th-convergent)))
;;=> 272
