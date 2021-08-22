;; Code for Project Euler Problem 49.
;;
;; Code author: Russell A. Edson
;; Date last modified: 22/08/2021

;; We seek the (other) increasing arithmetic sequence of 4-digit prime
;; numbers such that each term in the sequence is just a permutation of
;; the digits of the other numbers in the sequence. A brute-force
;; approach should suffice here, so we'll define functions to check for
;; primality and manipulate digits:
(defun is-prime? (n)
  "True if the given number N is prime, False if not."
  (if (= n 1)
      nil
      (let ((primep t))
	(loop for m from 2 upto (floor (sqrt n)) do
	  (if (zerop (mod n m))
	      (setf primep nil)))
	primep)))

(defun digits (number)
  "Return the set of digits in the given NUMBER."
  (mapcar #'digit-char-p (coerce (write-to-string number) 'list)))

;; Now we're only interested in the 4-digit primes, so we'll
;; first construct exactly such a list:
(time
 (defvar 4-digit-primes
   (loop for n from 1000 upto 9999
	 when (is-prime? n)
	   collect n)))
;;=> Evaluation took:
;;=>   0.013 seconds of real time
;;=>   0.000000 seconds of total run time (0.000000 user, 0.000000 system)
;;=>   0.00% CPU
;;=>   33,119,188 processor cycles
;;=>   32,768 bytes consed
;;=>
;;=> 4-DIGIT-PRIMES

(length 4-digit-primes)
;;=> 1061

;; There are not too many 4-digit primes, so a naive strategy of
;; "check every number against every other number and collect up the
;; permutations" suffices for our purposes.
(defvar prime-permutations nil)

(defun permutation? (m n)
  "True if M and N are permutations of each other with the same digits."
  (equalp (sort (digits m) #'<)
	  (sort (digits n) #'<)))

(time
 (let ((primes-left-to-check (copy-list 4-digit-primes)))
   (loop while (not (null primes-left-to-check)) do
     (let* ((m (nth 0 primes-left-to-check))
	    (m-permutations (list m)))
       (loop for i from 1 below (length primes-left-to-check) do
	 (let ((n (nth i primes-left-to-check)))
	   (if (permutation? m n)
	       (setf m-permutations (cons n m-permutations)))))
       (if (> (length m-permutations) 2)
	   (setf prime-permutations
		 (cons (sort (copy-list m-permutations) #'<)
		       prime-permutations)))
       (loop for prime in m-permutations do
	 (setf primes-left-to-check (remove prime primes-left-to-check)))))))
;;=> Evaluation took:
;;=>   0.169 seconds of real time
;;=>   0.171875 seconds of total run time (0.171875 user, 0.000000 system)
;;=>   [ Run times consist of 0.015 seconds GC time, and 0.157 seconds non-GC time. ]
;;=>   101.78% CPU
;;=>   439,392,974 processor cycles
;;=>   61,874,832 bytes consed
;;=>
;;=> NIL

(length prime-permutations)
;;=> 174

;; There are reasonable number of these permutations to sift through.
;; However, if we assume that we are strictly after the 3330 arithmetic
;; sequence primes, then we can further filter the list somewhat
;; by checking the pairwise differences between the numbers in the
;; permutations.
(defun pairwise-differences (list)
  "Return the pairwise differences between the numbers in LIST."
  (loop for i from 0 below (length list)
	append
	(loop for j from (1+ i) below (length list)
	      collect (abs (- (nth i list) (nth j list))))))

(remove-if-not
 (lambda (list) (member 3330 list))
 (mapcar
  (lambda (index permutations)
    (list index (pairwise-differences permutations)))
  (loop for index from 1 upto (length prime-permutations) collecting index)
  prime-permutations)
 :key #'cadr)
;;=> ((68 (630 828 3960 4230 198 3330 3600 3132 3402 270))
;;=>  (79 (270 3600 6930 3330 6660 3330))
;;=>  (86 (378 2700 2718 3348 4950 6678 2322 2340 2970 4572 6300 18
;;=>       648 2250 3978 630 2232 3960 1602 3330 1728))
;;=>  (88 (54 414 720 990 3384 360 666 936 3330 306 576 2970 270
;;=>       2664 2394))
;;=>  (135 (360 3330 3384 5994 6354 6660 7254 2970 3024 5634 5994
;;=>        6300 6894 54 2664 3024 3330 3924 2610 2970 3276 3870
;;=>        360 666 1260 306 900 594))
;;=>  (136 (3330 3348 6948 18 3618 3600)))

;; By inspection we can see that the prime permutations at indices
;; 79 (or 78, indexed from 0) and 135 are of interest in that they
;; contain at least two primes that differ by 3330.

(nth (1- 79) prime-permutations)
;;=> (2699 2969 6299 9629)

(nth (1- 135) prime-permutations)
;;=> (1487 1847 4817 4871 7481 7841 8147 8741)

;; We were given the 1487- permutation, so the other permutation
;; is the 2969- permutation that we're after.
(pairwise-differences '(2969 6299 9629))
;;=> (3330 6660 3330)

;; So our concatenated 12-digit answer is 296962999629.
