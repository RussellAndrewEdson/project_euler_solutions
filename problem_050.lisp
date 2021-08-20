;; Code for Project Euler Problem 50.
;;
;; Code author: Russell A. Edson
;; Date last modified: 20/08/2021

;; We want to find the largest prime number under 1000000 which can
;; be written as the sum of consecutive prime numbers. A
;; straightforward (if inefficient) exhaustive search of the prime
;; numbers below 1000000 should suffice for us here, even though
;; we may be waiting for a little bit to get the answer.
(defun is-prime? (n)
  "True if the given number N is prime, False if not."
  (if (= n 1)
      nil
      (let ((primep t))
	(loop for m from 2 upto (floor (sqrt n)) do
	  (if (zerop (mod n m))
	      (setf primep nil)))
	primep)))

(time
 (defvar primes
   (loop for n from 2 upto 1000000
	 when (is-prime? n)
	   collect n)))
;;=> Evaluation took:
;;=>   13.738 seconds of real time
;;=>   13.718750 seconds of total run time (13.718750 user, 0.000000 system)
;;=>   99.86% CPU
;;=>   35,607,964,024 processor cycles
;;=>   1,277,952 bytes consed
;;=>
;;=> PRIMES

;; Consider the smaller case of the primes less than 100 (for which
;; we are given that 41 = 2 + 3 + 5 + 7 + 11 + 13 is the longest
;; consecutive prime sum that is itself prime):
(defvar small-primes (remove-if-not (lambda (prime) (< prime 100)) primes))

;; Now if we generate running sums as we loop across the primes, we
;; can see visually how we might find our maximum length consecutive
;; sum of primes:
(defvar running-sums-below-100
  (maplist
   (lambda (sublist)
     (loop for n in sublist
	   sum n into running-sum
	   while (< running-sum 100)
	   collect running-sum))
   small-primes))

running-sums-below-100
;;=> ((2 5 10 17 28 41 58 77) (3 8 15 26 39 56 75 98)
;;=>  (5 12 23 36 53 72 95) (7 18 31 48 67 90) (11 24 41 60 83)
;;=>  (13 30 49 72) (17 36 59 88) (19 42 71) (23 52 83) (29 60 97)
;;=>  (31 68) (37 78) (41 84) (43 90) (47) (53) (59) (61) (67) (71)
;;=>  (73) (79) (83) (89) (97))

;; From here, we simply find the largest prime starting from the
;; end of the list for each of these lists. It's position in the
;; list (indexed from 1) will give the length of the consecutive
;; sequence.
(setf running-sums-below-100
      (mapcar
       (lambda (sums-list)
	 (let ((index (position-if #'is-prime? sums-list :from-end T)))
	   (list (nth index sums-list) (1+ index))))
       running-sums-below-100))
;;=> ((41 6) (3 1) (53 5) (67 5) (83 5) (13 1) (59 3) (71 3) (83 3)
;;=>  (97 3) (31 1) (37 1) (41 1) (43 1) (47 1) (53 1) (59 1) (61 1)
;;=>  (67 1) (71 1) (73 1) (79 1) (83 1) (89 1) (97 1))

;; And finally, we find the maximum length from these (which we can
;; do using the extremum function from the alexandria library):
(ql:quickload '(:alexandria))
(alexandria:extremum running-sums-below-100 #'> :key #'cadr)
;;=> (41 6)

;; So we'll wrap all of this in a convenient function and then find
;; the maximum length consecutive prime for the entire primes list,
;; which will give us our desired answer.
(defun longest-consecutive-prime-sum ()
  "Return the longest consecutive prime sum below 1000000."
  (let ((running-sums
	  (maplist
	   (lambda (sublist)
	     (loop for n in sublist
		   sum n into running-sum
		   while (< running-sum 1000000)
		   collect running-sum))
	   primes)))
    (alexandria:extremum
     (mapcar (lambda (sublist)
	       (let ((index (position-if #'is-prime? sublist :from-end T)))
		 (list (nth index sublist) (1+ index))))
	     running-sums)
     #'>
     :key #'cadr)))

(time (longest-consecutive-prime-sum))
;;=> Evaluation took:
;;=>   8.971 seconds of real time
;;=>   8.984375 seconds of total run time (8.984375 user, 0.000000 system)
;;=>   100.14% CPU
;;=>   23,253,547,695 processor cycles
;;=>   16,710,768 bytes consed
;;=>
;;=> (997651 543)
