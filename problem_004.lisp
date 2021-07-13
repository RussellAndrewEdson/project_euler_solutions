;; Code for Project Euler Problem 4.
;;
;; Code author: Russell A. Edson
;; Date last modified: 13/07/2021

;; A function to determine if a given number is a palindrome:
(defun palindrome? (number)
  "True if the given NUMBER is a palindrome, False if not."
  (equalp (digits number) (reverse (digits number))))

(defun digits (number)
  "Return the set of digits in the given NUMBER."
  (coerce (write-to-string number) 'list))

(palindrome? 123)
;;=> NIL

(palindrome? 1234321)
;;=> T


;; Then to find the largest palindrome from 3-digit number products
;; we can simply loop down from 999*999 (noting that we only need to
;; loop for half of these, since checking e.g. 516*788 is the same
;; as checking 788*516).
(defvar products (loop for n from 999 downto 100
		       append
		       (loop for m from n downto 100 collect (* n m))))

(apply #'max (remove-if-not #'palindrome? products))
;;=> 906609
