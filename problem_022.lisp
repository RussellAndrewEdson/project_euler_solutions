;; Code for Project Euler Problem 22.
;;
;; Code author: Russell A. Edson
;; Date last modified: 27/07/2021

;; We are given a list of names in a text file that we first download
;; from the Project Euler problem page and sort into alphabetical order:
(ql:quickload '(:dexador :str))
(defparameter text-file
  "https://projecteuler.net/project/resources/p022_names.txt")
(defparameter names
  (sort (mapcar (lambda (string) (string-trim '(#\") string))
		(str:split #\, (dex:get text-file)))
	#'string-lessp))

;; Now we define some functions to compute the 'name score' for
;; each of the names in the list:
(defun name-score (name index)
  "Return the 'name score' for the given NAME at INDEX in the name list."
  (* index (alphabetical-value name)))

(defun alphabetical-value (name)
  "Return the alphabetical value of NAME."
  (reduce #'+ (mapcar #'number-in-alphabet (coerce name 'list))))

(defun number-in-alphabet (char)
  "Return the alphabetical number for CHAR (e.g. A=1, B=2,...)"
  (1+ (- (char-code char) (char-code #\A))))

(alphabetical-value "COLIN")
;;=> 53

;; Finally, we simply loop over the list (keeping in mind that
;; Common Lisp indexes from zero, so we need to account for this).
(loop for index from 1 upto (length names)
      summing
      (name-score (elt names (1- index)) index))
;;=> 871198282
