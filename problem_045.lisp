;; Code for Project Euler Problem 45.
;;
;; Code author: Russell A. Edson
;; Date last modified: 12/08/2021

;; We are given formulae for the triangle, pentagonal and hexagonal
;; numbers, and that T_285 = P_165 = H_143 = 40755 is a number
;; that is trigonal, pentagonal and hexagonal. We seek the very next
;; such number.
;;
;; First, we can define the Nth triangle/pentagonal/hexagonal number
;; using the given equations:
(defun triangle-number (n)
  "Return the Nth triangle number."
  (/ (* n (1+ n)) 2))

(defun pentagonal-number (n)
  "Return the Nth pentagonal number."
  (/ (* n (1- (* 3 n))) 2))

(defun hexagonal-number (n)
  "Return the Nth hexagonal number."
  (* n (1- (* 2 n))))

(triangle-number 285)
;;=> 40755

(pentagonal-number 165)
;;=> 40755

(hexagonal-number 143)
;;=> 40755

;; Now we'll loop until we find the next number that is a triangle,
;; pentagonal and hexagonal number simultaneously. Our strategy here
;; will be to increment along the hexagonal numbers, and at each step
;; increment the triangle and pentagonal numbers until they are no
;; longer less than the hexagonal number. If they are then equal, we
;; have our desired number. If not, we keep incrementing along the
;; hexagonal numbers.
(time
 (let ((triangle-index 285)
       (pentagonal-index 165)
       (hexagonal-index 143)
       (number-found nil))
   (loop while (not number-found) do
     (incf hexagonal-index)
     (let ((current-hexagonal-number (hexagonal-number hexagonal-index))
	   (current-pentagonal-number (pentagonal-number pentagonal-index))
	   (current-triangle-number (triangle-number triangle-index)))
       (loop while (< current-pentagonal-number current-hexagonal-number) do
	 (incf pentagonal-index)
	 (setf current-pentagonal-number (pentagonal-number pentagonal-index)))
       (loop while (< current-triangle-number current-hexagonal-number) do
	 (incf triangle-index)
	 (setf current-triangle-number (triangle-number triangle-index)))
       (if (and (= current-hexagonal-number current-pentagonal-number)
		(= current-hexagonal-number current-triangle-number))
	   (setf number-found current-hexagonal-number))))
  number-found))
;;=> Evaluation took:
;;=>   0.007 seconds of real time
;;=>   0.000000 seconds of total run time (0.000000 user, 0.000000 system)
;;=>   0.00% CPU
;;=>   18,560,762 processor cycles
;;=>   0 bytes consed
;;=>
;;=> 1533776805
