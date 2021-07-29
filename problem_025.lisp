;; Code for Project Euler Problem 25.
;;
;; Code author: Russell A. Edson
;; Date last modified: 29/07/2021

;; We first code a function to compute the Fibonacci numbers. For
;; extra whimsy, we'll build it as a memoized generator using
;; a closure:
(defvar fibonacci
  (let ((fibonacci-numbers (list 1 1)))
    (lambda (n)
      (if (<= n (length fibonacci-numbers))
	  (nth (1- n) fibonacci-numbers)
	  (progn
	    (loop while (> n (length fibonacci-numbers)) do
	      (let ((fib-1 (nth (- (length fibonacci-numbers) 1)
				fibonacci-numbers))
		    (fib-2 (nth (- (length fibonacci-numbers) 2)
				fibonacci-numbers)))
		(setf fibonacci-numbers
		      (append fibonacci-numbers
			      (list (+ fib-1 fib-2))))))
	    (car (last fibonacci-numbers)))))))
(defun fibonacci (n)
  "Return the Nth Fibonacci number."
  (funcall fibonacci n))

(fibonacci 12)
;;=> 144

;; Now we want to find the index of the first Fibonacci number to
;; contain 1000 digits. So we can write some functions to count the
;; digits in a given number:
(defun digits (number)
  "Return the set of digits in the given NUMBER."
  (mapcar #'digit-char-p (coerce (write-to-string number) 'list)))

(defun num-digits (number)
  "Return the number of digits in the given NUMBER."
  (length (digits number)))

;; Finally, we loop until we find the first Fibonacci number with
;; 1000 digits.
(time
 (let ((n 1))
   (loop while (< (num-digits (fibonacci n)) 1000) do (incf n))
   n))
;;=> Evaluation took:
;;=>   0.429 seconds of real time
;;=>   0.437500 seconds of total run time (0.343750 user, 0.093750 system)
;;=>   [ Run times consist of 0.094 seconds GC time, and 0.344 seconds non-GC time. ]
;;=>   102.10% CPU
;;=>   1,112,596,243 processor cycles
;;=>   286,705,392 bytes consed
;;=>
;;=> 4782

(time (fibonacci 4782))
;;=> Evaluation took:
;;=>   0.000 seconds of real time
;;=>   0.000000 seconds of total run time (0.000000 user, 0.000000 system)
;;=>   100.00% CPU
;;=>   61,050 processor cycles
;;=>   0 bytes consed
;;=>   
;;=> 1070066266382758936764980584457396885083683896632151665013235203375
;;=> 3145206046940406218891475824897926578046948881775919574843364666725
;;=> 6995951299603046126274809248218614406943305123477444275027378175308
;;=> 7579391666192149259186759553966422837148943113074699503439547001985
;;=> 4326097230672901928705264472437261177158218255484911205250132014786
;;=> 1296593138179223555965745203950613755146783754322911960212993404826
;;=> 0706175397706847068202895486902666185435124521900369480641357447470
;;=> 9117076197669456910700980243934396174741037369125032313655321647736
;;=> 9702316775505159517351846057995491941096777837322966579658164651390
;;=> 3488154256310184224190259846088000110186255550245493937113651657039
;;=> 4476295847145485234259504285824253060835444354282126110089928637950
;;=> 4800689433030977321783486454311320576565986845628861680871869383529
;;=> 7350643986297640660000723562917905207051164077614812491885830945940
;;=> 5666883391093509444565763576661516193177537928916615813271596168774
;;=> 87983821820492520348473874384736771934512787029218636250627816
