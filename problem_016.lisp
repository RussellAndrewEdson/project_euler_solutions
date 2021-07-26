;; Code for Project Euler Problem 16.
;;
;; Code author: Russell A. Edson
;; Date last modified: 26/07/2021

;; We want to sum over the digits of a given power of 2. As always,
;; this task is radically simplified by arbitrary-length integers.
;; (Often using the right tools is most of the battle.)
(defun digits (number)
  "Return the set of digits in the given NUMBER."
  (mapcar #'digit-char-p (coerce (write-to-string number) 'list)))

(reduce #'+ (digits (expt 2 15)))
;;=> 26

;; So the sum of the digits in 2^1000 is simply
(reduce #'+ (digits (expt 2 1000)))
;;=> 1366
