;; Code for Project Euler Problem 38.
;;
;; Code author: Russell A. Edson
;; Date last modified: 13/08/2021

;; We are interested in 9-pandigital numbers (i.e. numbers containing
;; the digits 1 through 9 in any order). We can reuse some of the
;; functions coded during the related Problem 32 here:
(defun digits (number)
  "Return the set of digits in the given NUMBER."
  (mapcar #'digit-char-p (coerce (write-to-string number) 'list)))

(defun 9-pandigital? (number)
  "True if the given NUMBER contains the digits 1-9 in any order."
  (equalp
   (list 1 2 3 4 5 6 7 8 9)
   (sort (digits number) #'<)))

(9-pandigital? 192384576)
;;=> T

;; We'll also make a helper function that computes the 'concatenated
;; product' of a given number and list:
(defun concatenated-product (n list)
  "Return the concatenated product of N with the numbers in LIST."
  (values
   (parse-integer
    (format nil "~{~d~}" (mapcar (lambda (num) (* n num)) list)))))

(concatenated-product 192 '(1 2 3))
;;=> 192384576

(concatenated-product 9 '(1 2 3 4 5))
;;=> 918273645

;; Now we want the largest 9-pandigital number that can be formed
;; as the concatenated product of some integer with (1 2 ... n)
;; where n > 1. First note that we strictly require a 9-digit number,
;; so we have some upper bounds on the search space already: we know
;; that the upper bound for our candidate integers are the 4-digit
;; ones (since a 5-digit number concatenated with its double will
;; always be too big to fit into a 9-digit number). So an exhaustive
;; brute force search simply has us enumerating all numbers from
;; 1 to 9999 and multiplying by 1, 2, ..., n until we've got exactly
;; 9 digits in each case.
;;
;; We are also helpfully given a lower bound in 918273645. We know
;; that this is the worst we can do, so we already know that whatever
;; our 9-pandigital concatenation is, it has to start wth a 9. And so
;; we need not check any 2-digit candidates less than 91, or any
;; 3-digit candidates less than 918, etc. So we'll assume this is the
;; 'maximum' and loop to determine otherwise.
(defvar maximum-number 918273645)

;; Let's check the cases for m x (1 2 ... n) in turn.

;; m = 2-digit number from 91 to 98:
;; Happily we don't even need to loop for this case. For any number
;; between 91 and 98, we have that:
;;   multiplying by 1 gives a 2-digit number,
;;   multiplying by 2 gives a 3-digit number,
;;   multiplying by 3 gives a 3-digit number,
;;   multiplying by 4 gives a 3-digit number.
;; There is no way to generate a 9-digit number; concatenating with
;; (1 2 3) gives an 8-digit number, and concatenating with (1 2 3 4)
;; gives an 11-digit number. So we need not check the 2-digit cases
;; at all.

;; m = 3-digit number from 918 to 987:
;; In a similar way, we need not loop for any of these 3-digit
;; cases either. For all of the numbers between 918 and 987:
;;   multiplying by 1 gives a 3-digit number,
;;   multiplying by 2 gives a 4-digit number,
;;   multiplying by 3 gives a 4-digit number.
;; Again, there is no way to generate a strictly 9-digit number;
;; we'll always either cut it short at 7-digits, or go too long
;; to 11-digits. So none of the 3-digit cases need to be checked
;; either.

;; m = 4-digit number from 9182 to 9876:
;; Here we arrive at the one set of cases we need to check. Observe
;; that for all numbers between 9182 and 9876:
;;   multiplying by 1 gives a 4-digit number,
;;   multiplying by 2 gives a 5-digit number.
;; This gives us our 9-digit numbers. So we'll loop over these
;; checking each concatenated product to see if we have a 9-pandigital
;; number or not.
(time
 (loop for m from 9182 to 9876 do
   (let ((candidate (concatenated-product m '(1 2))))
     (if (and (9-pandigital? candidate)
	      (> candidate maximum-number))
	 (setf maximum-number candidate)))))
;;=> Evaluation took:
;;=>   0.004 seconds of real time
;;=>   0.000000 seconds of total run time (0.000000 user, 0.000000 system)
;;=>   0.00% CPU
;;=>   11,763,270 processor cycles
;;=>   720,496 bytes consed
;;=>
;;=> NIL

;; Our new maximum 9-pandigital concatenated product is:
maximum-number
;;=> 932718654
