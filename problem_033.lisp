;; Code for Project Euler Problem 33.
;;
;; Code author: Russell A. Edson
;; Date last modified: 09/08/2021

;; We seek 'digit cancelling fractions', e.g. 49/98, where we
;; can 'cancel' the 9s on the numerator and denominator to get 4/8,
;; which is equal to the original fraction. So a first step would
;; be to code a function that 'cancels' digits in a fraction in
;; this way. (As with Problem 32, we can exploit Lisp's homoiconicity
;; here and treat our fractions as lists (/ a b).)
(defun digits (number)
  "Return the set of digits in the given NUMBER."
  (mapcar #'digit-char-p (coerce (write-to-string number) 'list)))

(defun combine-digits (digits)
  "Return the number made by combining DIGITS together."
  (values (parse-integer (format nil "~{~d~}" digits))))

(digits 1234)
;;=> (1 2 3 4)

(combine-digits '(9 8 7 6))
;;=> 9876

(defun cancel-digits (fraction)
  "Return (multiple) fractions by cancelling digits in FRACTION."
  (let ((numerator-digits (digits (cadr fraction)))
	(denominator-digits (digits (caddr fraction)))
	(cancelled nil))
    (loop for digit in numerator-digits do
      (if (not (null (member digit denominator-digits)))
	  (setf
	   cancelled
	   (cons
	    (list '/
		  (combine-digits (remove digit numerator-digits :count 1))
		  (combine-digits (remove digit denominator-digits :count 1)))
	    cancelled))))
    cancelled))

(cancel-digits '(/ 49 98))
;;=> ((/ 4 8))
(cancel-digits '(/ 12 21))
;;=> ((/ 1 1) (/ 2 2))
(cancel-digits '(/ 12 34))
;;=> NIL

;; Now we define a function to check for a digit-cancelling fraction by
;; testing whether evaluating the fraction makes it equal to any of the
;; fractions returned by cancel-digits.
(defun digit-cancelling-fraction? (fraction)
  "True if FRACTION is a 'digit cancelling fraction, False if not."
  (let ((digit-cancelling-p nil))
    (loop for cancelled-fraction in (cancel-digits fraction) do
      ;; Check for divide by zero: return false if so
      (unless (or (zerop (caddr fraction))
		  (zerop (caddr cancelled-fraction)))
	(if (= (eval fraction) (eval cancelled-fraction))
	    (setf digit-cancelling-p t))))
    digit-cancelling-p))

(digit-cancelling-fraction? '(/ 49 98))
;;=> T

(digit-cancelling-fraction? '(/ 12 21))
;;=> NIL

(digit-cancelling-fraction? '(/ 12 34))
;;=> NIL

;; With these functions coded, we simply loop over all of the fractions
;; less than 1, with 2-digit numerators and denominators.
(time
 (defvar digit-cancelling-fractions
   (loop for m from 10 upto 99
	 append
	 (loop for n from (1+ m) upto 99
	       when (digit-cancelling-fraction? (list '/ m n))
		 collect (list '/ m n)))))
;;=> Evaluation took:
;;=>   0.005 seconds of real time
;;=>   0.000000 seconds of total run time (0.000000 user, 0.000000 system)
;;=>   0.00% CPU
;;=>   8,694 forms interpreted
;;=>   12,738,646 processor cycles
;;=>   3,335,088 bytes consed
;;=>
;;=> DIGIT-CANCELLING-FRACTIONS

digit-cancelling-fractions
;;=> ((/ 10 20) (/ 10 30) (/ 10 40) (/ 10 50) (/ 10 60) (/ 10 70)
;;=>  (/ 10 80) (/ 10 90) (/ 16 64) (/ 19 95) (/ 20 30) (/ 20 40)
;;=>  (/ 20 50) (/ 20 60) (/ 20 70) (/ 20 80) (/ 20 90) (/ 26 65)
;;=>  (/ 30 40) (/ 30 50) (/ 30 60) (/ 30 70) (/ 30 80) (/ 30 90)
;;=>  (/ 40 50) (/ 40 60) (/ 40 70) (/ 40 80) (/ 40 90) (/ 49 98)
;;=>  (/ 50 60) (/ 50 70) (/ 50 80) (/ 50 90) (/ 60 70) (/ 60 80)
;;=>  (/ 60 90) (/ 70 80) (/ 70 90) (/ 80 90))

;; We are given that there are exactly four non-trivial digit cancelling
;; fractions (i.e. we are not interested in the 10/20, 10/30 etc ones).
;; We can see from the output list that these non-trivial ones are:
;;   16/64, 19/95, 26/65, 49/98.
;; The product of these four fractions in lowest terms is
(* (/ 16 64) (/ 19 95) (/ 26 65) (/ 49 98))
;;=> 1/100
