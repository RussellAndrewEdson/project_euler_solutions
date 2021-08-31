;; Code for Project Euler Problem 57.
;;
;; Code author: Russell A. Edson
;; Date last modified: 31/08/2021

;; Here we are interested in continued fractions and convergents.
;; (Peeking ahead, we see some later problems that probably also
;; benefit from hitting them with computed convergents, e.g.
;; Problem 65 and Problem 66, so it's worth taking some effort to
;; get the scaffolding planned out now.)

;; A continued fraction has the form
;;   a0 + 1/(a1 + 1/(a2 + 1/(a3 + ...)))
;; and we'll generally represent them in the standard form
;;   [a0, a1, a2, a3, ...]
;; as an infinitely long list, say. Lazy lists are a very nice
;; computational representation of such infinitely-long lists,
;; which we can implement with generator functions and closures.
(defvar sqrt2-continued-fraction
  (let* ((a (list 1))
	 (next-digit (lambda () 2))
	 (expand-list-to-an
	   ;; Expand the list up to a_n (indexed from n = 0)
	   (lambda (n)
	     (loop while (>= n (length a)) do
	       (setf a (append a (list (funcall next-digit))))))))
    (list
     ;; Return the nth digit in the fraction (indexed from 0)
     (lambda (n)
       (if (>= n (length a))
	   (funcall expand-list-to-an n)
	   (nth n a)))
     ;; Return all of the digits up to and including n (indexed from 0)
     (lambda (n)
       (if (>= n (length a))
	   (funcall expand-list-to-an n))
       (subseq a 0 (1+ n))))))

(defun sqrt2-continued-fraction (n)
  "Return [a0, a1, a2, ..., a(N)] for the sqrt(2) continued fraction."
  (funcall (cadr sqrt2-continued-fraction) n))

(sqrt2-continued-fraction 0)
;;=> (1)

(sqrt2-continued-fraction 1)
;;=> (1 2)

(sqrt2-continued-fraction 10)
;;=> (1 2 2 2 2 2 2 2 2 2 2)

;; We can evaluate a continued fraction (convergent) most easily
;; by starting at the end and working our way back up iteratively.
(defun eval-convergent (continued-fraction)
  "Evaluate the given CONTINUED-FRACTION and return its rational form."
  (let* ((denominators (reverse continued-fraction))
	 (convergent (car denominators)))
    (loop for number in (cdr denominators) do
      (setf convergent (+ (/ 1 convergent) number)))
    convergent))

(eval-convergent (sqrt2-continued-fraction 0))
;;=> 1

(eval-convergent (sqrt2-continued-fraction 1))
;;=> 3/2

(mapcar (lambda (n) (eval-convergent (sqrt2-continued-fraction n)))
	(loop for n from 0 to 10 collecting n))
;;=> (1 3/2 7/5 17/12 41/29 99/70 239/169 577/408 1393/985
;;=>  3363/2378 8119/5741)

;; For this problem we want to know when the numerator has more digits
;; than the denominator in the first 1000 expansions (i.e. convergents),
;; so we can first generate a list of those convergents:
(time
 (defvar sqrt2-convergents
   (mapcar (lambda (n) (eval-convergent (sqrt2-continued-fraction n)))
	   (loop for n from 1 to 1000 collecting n))))
;;=> Evaluation took:
;;=>   0.124 seconds of real time
;;=>   0.109375 seconds of total run time (0.109375 user, 0.000000 system)
;;=>   [ Run times consist of 0.031 seconds GC time, and 0.079 seconds non-GC time. ]
;;=>   87.90% CPU
;;=>   321,677,922 processor cycles
;;=>   216,191,568 bytes consed
;;=>
;;=> SQRT2-CONVERGENTS

;; And then we simply loop over these, counting the number of digits
;; in the numerator and denominators and keeping track of those with
;; more digits in the numerator, as desired.
(defun digits (number)
  "Return the set of digits in the given NUMBER."
  (mapcar #'digit-char-p (coerce (write-to-string number) 'list)))

(loop for convergent in sqrt2-convergents
      when (> (length (digits (numerator convergent)))
	      (length (digits (denominator convergent))))
	count convergent)
;;=> 153
