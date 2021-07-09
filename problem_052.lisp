;; Code for Project Euler Problem 52.
;;
;; Code author: Russell A. Edson
;; Date last modified: 09/07/2021

;; We can simply hit this problem with modern computation in a brute
;; force solution. Define functions to do the digit-wise comparison,
;; and then loop over some sufficiently large set of numbers.
(defun digits (number)
  "Return the set of digits in the given NUMBER."
  (coerce (write-to-string number) 'list))

(defun contain-same-digits? (number1 number2)
  "True if NUMBER1 and NUMBER2 contain the same digits, False if not."
  (null (set-exclusive-or (digits number1) (digits number2))))

(loop for i from 1 to 9999999 do
  (if (and (contain-same-digits? i (* 2 i))
	   (contain-same-digits? i (* 3 i))
	   (contain-same-digits? i (* 4 i))
	   (contain-same-digits? i (* 5 i))
	   (contain-same-digits? i (* 6 i)))
      (format t "~a~%" i)))
;;=> 142857
;;=> 1428570
;;=> 1429857
