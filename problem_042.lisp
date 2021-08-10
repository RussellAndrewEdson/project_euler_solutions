;; Code for Project Euler Problem 42.
;;
;; Code author: Russell A. Edson
;; Date last modified: 10/08/2021

;; This problem looks at triangle numbers and 'triangle words'.
;; We are given a text file of words that we can download from
;; the Project Euler problem page:
(ql:quickload '(:dexador :str))
(defparameter text-file
  "https://projecteuler.net/project/resources/p042_words.txt")
(defparameter words
  (mapcar (lambda (string) (string-trim '(#\") string))
	  (str:split #\, (dex:get text-file))))

;; We can re-use some functions from Problem 22 for computing the
;; sum (or "alphabetical value") of a given word:
(defun alphabetical-value (word)
  "Return the alphabetical value of WORD."
  (reduce #'+ (mapcar #'number-in-alphabet (coerce word 'list))))

(defun number-in-alphabet (char)
  "Return the alphabetical number for CHAR (e.g. A=1, B=2,...)"
  (1+ (- (char-code char) (char-code #\A))))

(alphabetical-value "SKY")
;;=> 55

;; We want to compare which of these alphabetical values are
;; triangle numbers, and so we want to implement a lazy list
;; of the triangle numbers that only grows to be as big as it
;; needs to be (there are infinitely many triangle numbers!)
;; We can do this in a straightforward manner using a closure.
(defvar lazy-list-triangle-numbers
  (let ((triangle-numbers (list 1)))
    (lambda (n)
      (if (<= n (length triangle-numbers))
	  (nth (1- n) triangle-numbers)
	  (progn
	    (loop while (> n (length triangle-numbers)) do
	      (let ((m (1+ (length triangle-numbers))))
		(setf triangle-numbers
		      (append triangle-numbers
			      (list (/ (* m (1+ m)) 2))))))
	    (car (last triangle-numbers)))))))

(defun triangle-number (n)
  "Return the Nth triangle number."
  (funcall lazy-list-triangle-numbers n))

(triangle-number 10)
;;=> 55

;; We want to check whether a given number is a triangle number,
;; which entails extending the list if it is not long enough.
;;
;; Some simple rearrangement of t_n = n(n+1)/2 >= C (for some
;; number C that we want a triangle number t_n larger than) gives
;;   n = ceil((sqrt(8*c+1)-1)/2) for a sufficiently large n.
(defun next-triangle-number (c)
  "Return the next triangle number greater than or equal to C."
  (triangle-number (ceiling (/ (1- (sqrt (1+ (* 8 c)))) 2))))

(next-triangle-number 100)
;;=> 105

(next-triangle-number 105)
;;=> 105

;; Finally, we simply loop over the list of words, collecting up
;; the ones that are triangle words.
(defvar triangle-words
  (remove-if-not
   (lambda (alpha-val) (= alpha-val (next-triangle-number alpha-val)))
   words
   :key #'alphabetical-value))

triangle-words
;;=> ("A" "ABILITY" "ABOVE" "ACCOMPANY" "ACHIEVEMENT" "AGENCY"
;;=>  "AGREE" "AIR" "ALREADY" "AN" "ANCIENT" "APPARENT" "APPOINT"
;;=>  "APPROACH" "ASSUME" "AT" "ATMOSPHERE" "BAG" "BAND" "BANK" "BAR"
;;=>  "BEAT" "BELONG" "BENEATH" "BONE" "BOTH" "BRIDGE" "BUILDING"
;;=>  "BURN" "CALL" "CAPACITY" "CAREFUL" "CASE" "CHILD" "CIVIL"
;;=>  "CLOSELY" "COME" "CONFIDENCE" "CONFIRM" "CONSERVATIVE"
;;=>  "CONSTRUCTION" "CONTENT" "COULD" "CURRENTLY" "DECISION"
;;=>  "DEFINITION" "DEMOCRATIC" "DEPUTY" "DESPITE" "DISTINCTION"
;;=>  "EAST" "EDGE" "EDUCATIONAL" "EFFECT" "EQUIPMENT" "EVENT" "FACE"
;;=>  "FAIL" "FAMILY" "FEEL" "FIELD" "FIGURE" "FLOOR" "FREEDOM" "FUND"
;;=>  "FUTURE" "GENTLEMAN" "GREY" "GROWTH" "HAIR" "HAPPY" "HAVE"
;;=>  "HERE" "HIS" "IF" "INCIDENT" "INCREASED" "INCREASINGLY"
;;=>  "INDIVIDUAL" "INSTRUMENT" "INTEND" "INTENTION" "IS" "LAW"
;;=>  "LEADER" "LEAVE" "LENGTH" "LESS" "LITTLE" "LOVELY" "MAN" "MATCH"
;;=>  "MERELY" "MILK" "MISTAKE" "MOVE" "MUCH" "NEED" "NOTICE" "OBJECT"
;;=>  "OBJECTIVE" "OF" "OIL" "ONLY" "OTHER" "OURSELVES" "PART" "PASS"
;;=>  "PATH" "PERFORM" "PRISON" "PRIVATE" "PROBABLY" "PROCEDURE"
;;=>  "QUALITY" "QUESTION" "RANGE" "READ" "REAL" "RELIEF" "REMOVE"
;;=>  "REPRESENT" "REQUEST" "RESPOND" "RIDE" "SAMPLE" "SAY" "SEAT"
;;=>  "SECURITY" "SINGLE" "SKY" "SOIL" "SOLICITOR" "SONG" "SOUTHERN"
;;=>  "SPIRIT" "START" "SUGGESTION" "TALL" "TAX" "THEORY" "THREATEN"
;;=>  "THROUGHOUT" "TITLE" "TOOTH" "TOTALLY" "TRAVEL" "TYPE" "UNABLE"
;;=>  "UNDERSTAND" "UPON" "USE" "VARIOUS" "VARY" "VIDEO" "WAGE" "WARM"
;;=>  "WATCH" "WE" "WHILST" "WIDELY" "WOMAN")

(length triangle-words)
;;=> 162
