;; Code for Project Euler Problem 56.
;;
;; Code author: Russell A. Edson
;; Date last modified: 30/08/2021

;; We want to find the maximal digit sum of numbers of the form a^b
;; for 1 < a, b < 100. Even though these numbers are relatively
;; large, this problem still buckles under modern computation and
;; Common Lisp's arbitrary length integers.
(defun digits (number)
  "Return the set of digits in the given NUMBER."
  (mapcar #'digit-char-p (coerce (write-to-string number) 'list)))

(time
 (let ((max-a-b '(1 99))
       (max-sum 1))
   (loop for a from 2 upto 99 do
     (loop for b from 1 upto 99 do
       (let ((current-sum (reduce #'+ (digits (expt a b)))))
	 (if (> current-sum max-sum)
	     (progn
	       (setf max-sum current-sum)
	       (setf max-a-b (list a b)))))))
   (list max-a-b max-sum)))
;;=> Evaluation took:
;;=>   0.048 seconds of real time
;;=>   0.046875 seconds of total run time (0.046875 user, 0.000000 system)
;;=>   97.92% CPU
;;=>   126,512,082 processor cycles
;;=>   35,089,808 bytes consed
;;=>
;;=> ((99 95) 972)

(expt 99 95)
;;=> 3848960788934848611927795802824596789608451156087366034658627953530
;;=>14812600853425803226738376862748709461096855428669269737472672585319
;;=>5657679460590239636893953692985541958490801973870359499
