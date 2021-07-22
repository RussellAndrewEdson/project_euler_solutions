;; Code for Project Euler Problem 13.
;;
;; Code author: Russell A. Edson
;; Date last modified: 22/07/2021

;; We can scrape the numbers to be summed straight from the
;; Project Euler problem archive page using lquery:
(ql:quickload '(:dexador :plump :lquery :str))
(defparameter url "https://projecteuler.net/problem=13")
(defvar parsed-url (lquery:$ (initialize (dex:get url))))

(defvar numbers
  (str:split #\newline
	     (str:trim (elt (lquery:$ parsed-url
			      ".problem_content .monospace"
			      (text))
			    0))))

;; And then adding these long integers together is trivial for
;; Common Lisp's default arbitrary-length Integer class.
(reduce #'+
	(mapcar #'parse-integer numbers))
;;=> 5537376230390876637302048746832985971773659831892672
