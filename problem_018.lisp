;; Code for Project Euler Problem 18.
;;
;; Code author: Russell A. Edson
;; Date last modified: 26/07/2021

;; We are given a triangle of numbers and asked to find the maximum
;; path sum. So our first step is to read in the triangle by
;; scraping it from the Project Euler archive page:
(ql:quickload '(:dexador :plump :lquery :str))
(defparameter url "https://projecteuler.net/problem=18")
(defvar parsed-url (lquery:$ (initialize (dex:get url))))
(defvar triangle-rows
  (str:split #\newline
	     (str:trim (elt (lquery:$ parsed-url
			      ".problem_content .monospace"
			      (text))
			    1))))

(let ((n (length triangle-rows)))
  (defvar triangle (make-array (list n n) :element-type :integer))
  (loop for i from 0 below n do
    (let ((row (str:split #\space (elt triangle-rows i))))
      (loop for j from 0 below (length row) do
	(setf (aref triangle i j) (parse-integer (elt row j)))))))
triangle
;;=> #2A((75 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;;=>     (95 64 0 0 0 0 0 0 0 0 0 0 0 0 0)
;;=>     (17 47 82 0 0 0 0 0 0 0 0 0 0 0 0)
;;=>     (18 35 87 10 0 0 0 0 0 0 0 0 0 0 0)
;;=>     (20 4 82 47 65 0 0 0 0 0 0 0 0 0 0)
;;=>     (19 1 23 75 3 34 0 0 0 0 0 0 0 0 0)
;;=>     (88 2 77 73 7 63 67 0 0 0 0 0 0 0 0)
;;=>     (99 65 4 28 6 16 70 92 0 0 0 0 0 0 0)
;;=>     (41 41 26 56 83 40 80 70 33 0 0 0 0 0 0)
;;=>     (41 48 72 33 47 32 37 16 94 29 0 0 0 0 0)
;;=>     (53 71 44 65 25 43 91 52 97 51 14 0 0 0 0)
;;=>     (70 11 33 28 77 73 17 78 39 68 17 57 0 0 0)
;;=>     (91 71 52 38 17 14 91 43 58 50 27 29 48 0 0)
;;=>     (63 66 4 68 89 53 67 30 73 16 69 87 40 31 0)
;;=>     (4 62 98 27 23 9 70 98 73 93 38 53 60 4 23))

;; Now as the problem mentions, there are only 16384 different
;; routes through the triangle, and so it is no problem to
;; simply enumerate them all and check each one in turn. This
;; exhaustive search will guarantee us the maximum.
;;
;; First for convenience, let us represent a path through the
;; triangle by a binary string (where 0=left and 1=right as
;; we make our way down the triangle).
(defun binary (n &optional (bits 14))
  "Return the (14)-bit binary representation of the given number N."
  (coerce
   (mapcar (lambda (i) (if (logbitp i n)
			   1
			   0))
	   (reverse (loop for i from 0 below bits collect i)))
   'bit-vector))

(defun path-through-triangle (binary-string)
  "Return the path through the triangle represented by BINARY-STRING."
  (let ((j 0)
	(left-right (coerce binary-string 'list))
	(path (list (aref triangle 0 0))))
    (loop for i from 1 below (car (array-dimensions triangle)) do
      (setf j (+ j (car left-right)))
      (setf path (cons (aref triangle i j) path))
      (setf left-right (cdr left-right)))
    (reverse path)))

;; e.g. the left-most path through the triangle:
(path-through-triangle #*00000000000000)
;;=> (75 95 17 18 20 19 88 99 41 41 53 70 91 63 4)

;; e.g. the right-most path:
(path-through-triangle #*11111111111111)
;;=> (75 64 82 10 65 34 67 92 33 29 14 57 48 31 23)

;; With this representation, we simply loop over all of the 14-bit
;; binary strings (paths), computing the maximum path sum for each
;; to find the maximum in an exhaustive search.
(defun path-sum (binary-string)
  "Return the path sum for the path represented by BINARY-STRING."
  (reduce #'+ (path-through-triangle binary-string)))

(time
 (let ((max-path #*00000000000000))
   (loop for n from 1 below (expt 2 14) do
     (if (>= (path-sum (binary n)) (path-sum max-path))
	 (setf max-path (binary n))))
   (list max-path (path-sum max-path))))
;;=> Evaluation took:
;;=>   0.059 seconds of real time
;;=>   0.062500 seconds of total run time (0.062500 user, 0.000000 system)
;;=>   105.08% CPU
;;=>   152,777,136 processor cycles
;;=>   35,946,192 bytes consed
;;=>
;;=> (#*11001001111101 1074)

;; However, as the problem description also notes, this exhaustive
;; searching becomes infeasible for e.g. Problem 67 which has 100 rows.
;; In such a case we seek a faster optimisation algorithm. (Our binary
;; representation here is especially suggestive that a genetic
;; algorithm may be appropriate, perhaps.)
