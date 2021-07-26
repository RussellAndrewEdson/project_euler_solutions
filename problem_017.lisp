;; Code for Project Euler Problem 17.
;;
;; Code author: Russell A. Edson
;; Date last modified: 26/07/2021

;; In this problem we want to write out the number between 1 and 1000
;; in words and count the letters. Amazingly enough, Common Lisp's
;; format macro almost has a ready-made solution for us:
(format nil "~r" 342)
;;=> "three hundred forty-two"

;; The only issue is that it isn't including the "and" after the
;; hundred. But we know the rule for this: we say "hundred and"
;; unless "hundred" is the last word, and that suffices for the
;; numbers from 1 to 1000. So it is straightforward to use a string
;; replacement here to adjust the string after the fact.
(ql:quickload :cl-ppcre)

(defun number->words (number)
  "Returns the NUMBER written out in words."
  (cl-ppcre:regex-replace "hundred "
			  (format nil "~r" number)
			  "hundred and "))

(number->words 342)
;;=> "three hundred and forty-two"

(number->words 115)
;;=> "one hundred and fifteen"

;; Now we simply count the letters (not including hyphens).
(defun strip-spaces-and-hyphens (string)
  "Return STRING but with spaces and hyphens removed."
  (cl-ppcre:regex-replace-all "[ -]" string ""))

(length (strip-spaces-and-hyphens (number->words 342)))
;;=> 23

(reduce #'+
	(mapcar (lambda (number)
		  (length (strip-spaces-and-hyphens (number->words number))))
		(loop for i from 1 upto 1000 collecting i)))
;;=> 21124
