;; Code for Project Euler Problem 32.
;;
;; Code author: Russell A. Edson
;; Date last modified: 09/08/2021

;; We consider '9-pandigital' expressions: that is, expressions where
;; each of the digits in the operands and RHS result contain the
;; digits 1 through 0 exact once each.

;; Here is where we can make full use of Common Lisp's homoiconicity.
;; First, we'll code a function to return a list of the digits in
;; a given number:
(defun digits (number)
  "Return the set of digits in the given NUMBER."
  (mapcar #'digit-char-p (coerce (write-to-string number) 'list)))

(digits 15324)
;;=> (1 5 3 2 4)

;; Next, we'll code a function that returns a list of the digits
;; in the given (quoted) Lisp expression, filtering out the operators
;; and syntax. Since Lisp code is just a Lisp list, this is very
;; straightforward.
(ql:quickload '(:alexandria))

(defun extract-digits (expression)
  "Return the set of digits in the given EXPRESSION (ignoring syntax)."
  (mapcan #'digits
	  (remove-if-not #'numberp (alexandria:flatten expression))))

(extract-digits '(* 39 186))
;;=> (3 9 1 8 6)

;; Now we can code a function to check whether we are given
;; a 9-pandigital (Lisp) expression. Note that we take in the
;; product expression as 'data', and then simply evaluate it to
;; compute the product.
(defun 9-pandigital? (expression)
  "True if the given EXPRESSION and its result contain the digits 1-9."
  (equalp
   (list 1 2 3 4 5 6 7 8 9)
   (sort (append (digits (eval expression))
		 (extract-digits expression))
	 #'<)))

(9-pandigital? '(* 39 186))
;;=> T

;; Finally, we'll code a relatively naive loop, but we want to at
;; least deduce a sensible stopping point so that we're not waiting
;; around too long. Note that we need exactly 9 digits to appear in
;; the expression, so our operands can be at most 4 digits long
;; each: otherwise there are too many digits on the LHS of the
;; expression. So in this naive, overkill case we 'only' have to loop
;; for i in [1, 9999] and j in [i, 9999].
(time
 (defvar pandigital-products
   (loop for i from 1 upto 9999
	 append
	 (loop for j from i upto 9999
	       when (9-pandigital? (list '* i j))
		 collect (list (list '* i j) (* i j))))))
;;=> Evaluation took:
;;=>   101.877 seconds of real time
;;=>   101.875000 seconds of total run time (100.0000000 user, 1.875000 system)
;;=>   [ Run times consist of 3.330 seconds GC time, and 98.545 seconds non-GC time. ]
;;=>   100.00% CPU
;;=>   149,985,000 forms interpreted
;;=>   264,064,961,354 processor cycles
;;=>   59,491,891,840 bytes consed
;;=>
;;=> PANDIGITAL-PRODUCTS

pandigital-products
;;=> (((* 4 1738) 6952) ((* 4 1963) 7852) ((* 12 483) 5796)
;;=>  ((* 18 297) 5346) ((* 27 198) 5346) ((* 28 157) 4396)
;;=>  ((* 39 186) 7254) ((* 42 138) 5796) ((* 48 159) 7632))

;; As per the warning, we remove the duplicate products and then sum
;; them all up for the answer.
(reduce #'+
	(remove-duplicates pandigital-products :key #'cadr)
	:key #'cadr)
;;=> 45228
