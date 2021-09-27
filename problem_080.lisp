;; Code for Project Euler Problem 80.
;;
;; Code author: Russell A. Edson
;; Date last modified: 27/09/2021

;; In this problem we want to generate arbitrary-precision square root
;; expansions so that we can sum the digits of all of the irrational
;; square roots.
;;
;; A straightforward way to generate arbitrary decimal expansions of
;; square roots is just to map the old 'pencil-and-paper' method for
;; finding square roots to a computation.
;;
;; Example: suppose we want to find the square root of 31:
;;    5.5...
;;   ----------------
;;   31.0000          1. We start with remainder c (here c = 31)
;;                       and partial root p (here p=0)
;;   25               2. We solve y = x(20p+x) <= c for the largest
;;                       x (here x = 5, for y = 5(0+5) = 25). We write
;;                       x at the top (it is the next digit in the
;;                       square root expansion).
;;    6 00            3. We then set the new remainder
;;                       c = 100(31-25) = 600 and update the partial
;;                       root p = 5, and repeat the process.
;;    5 25           2b. We solve y = x(20p+x) <= c for the largest x
;;                       (here x = 5 again, for y = 5(100+5) = 525).
;;                       So 5 is the next digit in our root expansion,
;;                       which we write at the top.
;;      75 00        3b. We then set the new remainder
;;                       c = 100(75) = 7500, and update the partial
;;                       root p = 55 (ignoring the decimal point).
;;                       And we loop again. If ever the remainder is
;;                       0, we're finished.

;; In our case we don't need to worry too much about where the
;; decimal point goes, since we can just take the integer square root
;; to find the integer part, set a decimal point manually, and then
;; proceed with the above algorithm. Taking the integer square root
;; also lets us terminate early in the case of non-irrational roots
;; (since we are given that if the square root of a natural number is
;; not irrational, then it must be wholly integer).

;; So, we'll code up this algorithm as follows. We'll return a
;; string representation for now so that we can check that our
;; procedure works.
(defun square-root-expansion (n &optional (decimal-places 20))
  "Return a string representation of sqrt(N) to given DECIMAL-PLACES."
  (let* ((p (isqrt n))
	 (c (- n (* p p)))
	 (square-root (list "." p)))
    (if (zerop c)
	(loop repeat decimal-places do (push 0 square-root))
	(loop repeat decimal-places do
	  (setf c (* 100 c))
	  (let ((x 0))
	    (loop while (<= (* x (+ (* 20 p) x)) c)
		  do (incf x)
		  finally (setf x (1- x)))
	    (push x square-root)
	    (setf c (- c (* x (+ (* 20 p) x))))
	    (setf p (+ (* 10 p) x)))))
    (format nil "~{~a~}" (reverse square-root))))

(square-root-expansion 2)
;;=> "1.41421356237309504880"

(square-root-expansion 2 100)
;;=> "1.4142135623730950488016887242096980785696718753769480731766797379
;;=>  907324784621070388503875343276415727"

(sqrt 2)
;;=> 1.4142135

(square-root-expansion 4)
;;=> "2.00000000000000000000"

;; Looks good so far. Now we simply loop over the natural numbers
;; between 1 and 100 and collect up all of the 100-decimal-place
;; square root expansions for the irrational roots.
(time
 (defvar square-roots
   (loop for n from 2 upto 99
	 when (/= n (* (isqrt n) (isqrt n)))
	   collect (square-root-expansion n 100))))
;;=> Evaluation took:
;;=>   0.018 seconds of real time
;;=>   0.015625 seconds of total run time (0.015625 user, 0.000000 system)
;;=>   88.89% CPU
;;=>   46,447,060 processor cycles
;;=>   11,070,144 bytes consed
;;=>
;;=> SQUARE-ROOTS

;; Finally, we loop over these expansions, extracting the digits
;; (i.e. ignoring the decimal point) and summing up the first 100
;; decimal digits.
(time
 (loop for expansion in square-roots
       summing (reduce #'+
		       (subseq (mapcar #'digit-char-p
				       (remove-if
					(lambda (char) (eq char #\.))
					(coerce expansion 'list)))
			       0
			       100))))
;;=> Evaluation took:
;;=>   0.001 seconds of real time
;;=>   0.000000 seconds of total run time (0.000000 user, 0.000000 system)
;;=>   0.00% CPU
;;=>   1,743,948 processor cycles
;;=>   589,824 bytes consed
;;=>
;;=> 40886
