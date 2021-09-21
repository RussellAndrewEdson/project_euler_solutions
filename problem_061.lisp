;; Code for Project Euler Problem 61.
;;
;; Code author: Russell A. Edson
;; Date last modified: 21/09/2021

;; We want to find the only ordered set of six 'cyclic' 4-digit
;; numbers where each number in the set is a different figurate
;; number (e.g. triangle number, square number, pentagonal number...)
;;
;; There are actually not many 4-digit triangle numbers (and even
;; fewer square/pentagonal/hexagonal/heptagonal/octagonal 4-digit
;; numbers too), so our straightforward approach could simply involve
;; pre-computing them all, and then looping over one of the sets,
;; testing all of the viable candidates from the other number sets
;; for a cyclic pair. WLOG assume that the first number in the cycle
;; is the triangle number (since it's a cycle we can always 'cycle'
;; through the numbers in the set until this is the case, of course).

;; Now we're interested in digits, so we'll bring in our usual function
;; for extracting the digits of a number:
(defun digits (number)
  "Return the set of digits in the given NUMBER."
  (mapcar #'digit-char-p (coerce (write-to-string number) 'list)))

(digits 12345)
;;=> (1 2 3 4 5)

;; We can compute the triangle numbers as follows:
(defun triangle (n)
  "Return the Nth triangle number."
  (/ (* n (1+ n)) 2))

(triangle 150)
;;=> 11325

;; And we're only interested in the 4-digit ones, so we'll collect all
;; of the 4-digit triangle numbers up right now. We're also only
;; interested in those 4-digit numbers that can be part of a cyclic set,
;; so we can further shrink our problem space by noticing that it is
;; impossible to have a cycle if one of the numbers has a 0 as the third
;; digit (since this would mean that the next number in the cycle is a
;; 3-digit number as it would have a leading 0). So we can filter our
;; sets accordingly:
(defvar triangle-numbers
  (loop for n from 1 upto 150
	for t-n = (triangle n)
	when (and (= (length (digits t-n)) 4)
		  (/= (third (digits t-n)) 0))
	  collect t-n))

triangle-numbers
;;=> (1035 1081 1128 1176 1225 1275 1326 1378 1431 1485 1540 1596 1653
;;=>  1711 1770 1830 1891 1953 2016 2080 2145 2211 2278 2346 2415 2485
;;=>  2556 2628 2775 2850 2926 3081 3160 3240 3321 3486 3570 3655 3741
;;=>  3828 3916 4095 4186 4278 4371 4465 4560 4656 4753 4851 4950 5050
;;=>  5151 5253 5356 5460 5565 5671 5778 5886 5995 6216 6328 6441 6555
;;=>  6670 6786 7021 7140 7260 7381 7626 7750 7875 8128 8256 8385 8515
;;=>  8646 8778 8911 9045 9180 9316 9453 9591 9730 9870)

;; Similarly, we'll construct a list of square numbers:
(defun square (n)
  "Return the Nth square number."
  (* n n))

(square 100)
;;=> 10000

(defvar square-numbers
  (loop for n from 1 upto 100
	for s-n = (square n)
	when (and (= (length (digits s-n)) 4)
		  (/= (third (digits s-n)) 0))
	  collect s-n))

square-numbers
;;=> (1024 1089 1156 1225 1296 1369 1444 1521 1681 1764 1849 1936 2025
;;=>  2116 2916 3025 3136 3249 3364 3481 3721 3844 3969 4096 4225 4356
;;=>  4489 4624 4761 5041 5184 5329 5476 5625 5776 5929 6084 6241 6561
;;=>  6724 6889 7056 7225 7396 7569 7744 7921 8281 8464 8649 8836 9025
;;=>  9216)

;; And a list of pentagonal numbers (note that the total numbers of each
;; of these figurate categories gets smaller as we go):
(defun pentagonal (n)
  "Return the Nth pentagonal number."
  (/ (* n (1- (* 3 n))) 2))

(pentagonal 90)
;;=> 12105

(defvar pentagonal-numbers
  (loop for n from 1 upto 90
	for p-n = (pentagonal n)
	when (and (= (length (digits p-n)) 4)
		  (/= (third (digits p-n)) 0))
	  collect p-n))

pentagonal-numbers
;;=> (1080 1162 1247 1335 1426 1520 1617 1717 1820 1926 2035 2147 2262
;;=>  2380 2625 2752 2882 3015 3151 3290 3432 3577 3725 3876 4030 4187
;;=>  4347 4510 4676 4845 5017 5192 5370 5551 5735 5922 6112 7315 7526
;;=>  7740 7957 8177 8626 8855 9087 9322 9560)

;; And the same for the hexagonal numbers:
(defun hexagonal (n)
  "Return the Nth hexagonal number."
  (* n (1- (* 2 n))))

(hexagonal 80)
;;=> 12720

(defvar hexagonal-numbers
  (loop for n from 1 upto 80
	for h-n = (hexagonal n)
	when (and (= (length (digits h-n)) 4)
		  (/= (third (digits h-n)) 0))
	  collect h-n))

hexagonal-numbers
;;=> (1035 1128 1225 1326 1431 1540 1653 1770 1891 2016 2145 2278 2415
;;=>  2556 2850 3160 3321 3486 3655 3828 4186 4371 4560 4753 4950 5151
;;=>  5356 5565 5778 5995 6216 6441 6670 7140 7381 7626 7875 8128 8385
;;=>  8646 8911 9180 9453 9730)

;; And the heptagonal numbers:
(defun heptagonal (n)
  "Return the Nth heptagonal number."
  (/ (* n (- (* 5 n) 3)) 2))

(heptagonal 70)
;;=> 12145

(defvar heptagonal-numbers
  (loop for n from 1 upto 70
	for h-n = (heptagonal n)
	when (and (= (length (digits h-n)) 4)
		  (/= (third (digits h-n)) 0))
	  collect h-n))

heptagonal-numbers
;;=> (1071 1177 1288 1525 1651 1782 1918 2059 2356 2512 2673 2839 3010
;;=>  3186 3367 3553 3744 3940 4141 4347 4558 4774 4995 5221 5452 5688
;;=>  5929 6175 6426 6682 6943 7480 7756 8037 8323 8614 8910 9211 9517
;;=>  9828)

;; And lastly, the octagonal numbers:
(defun octagonal (n)
  "Return the Nth octagonal number."
  (* n (- (* 3 n) 2)))

(octagonal 60)
;;=> 10680

(defvar octagonal-numbers
  (loop for n from 1 upto 60
	for o-n = (octagonal n)
	when (and (= (length (digits o-n)) 4)
		  (/= (third (digits o-n)) 0))
	  collect o-n))

octagonal-numbers
;;=> (1045 1160 1281 1541 1680 1825 1976 2133 2296 2465 2640 2821 3816
;;=>  4033 4256 4485 4720 4961 5461 5720 5985 6256 6533 6816 8321 8640
;;=>  8965 9296 9633 9976)

;; Now we want to be keeping track of which set a number belongs
;; to as we loop through to find the cycle. This will allow us to
;; cut down on the search space by removing the numbers from sets that
;; we've already linked earlier in the cycle. A straightforward way
;; to do this is to prepend each number/list of numbers with a key
;; describing that number, and then take this key into account
;; for the looping and cycle-checking.
(defvar figurate-numbers
  (list (list :triangle triangle-numbers)
	(list :square square-numbers)
	(list :pentagonal pentagonal-numbers)
	(list :hexagonal hexagonal-numbers)
	(list :heptagonal heptagonal-numbers)
	(list :octagonal octagonal-numbers)))

(defun figurate-except (figurate-types)
  "Return the figurate numbers except for those in FIGURATE-TYPES."
  (remove-if
   (lambda (type) (member type figurate-types :test #'eq))
   figurate-numbers
   :key #'car))

(figurate-except '(:triangle :pentagonal :hexagonal))
;;=> ((:SQUARE
;;=>   (1024 1089 1156 1225 1296 1369 1444 1521 1681 1764 1849 1936
;;=>    2025 2116 2916 3025 3136 3249 3364 3481 3721 3844 3969 4096
;;=>    4225 4356 4489 4624 4761 5041 5184 5329 5476 5625 5776 5929
;;=>    6084 6241 6561 6724 6889 7056 7225 7396 7569 7744 7921 8281
;;=>    8464 8649 8836 9025 9216))
;;=>  (:HEPTAGONAL
;;=>   (1071 1177 1288 1525 1651 1782 1918 2059 2356 2512 2673 2839
;;=>    3010 3186 3367 3553 3744 3940 4141 4347 4558 4774 4995 5221
;;=>    5452 5688 5929 6175 6426 6682 6943 7480 7756 8037 8323 8614
;;=>    8910 9211 9517 9828))
;;=>  (:OCTAGONAL
;;=>   (1045 1160 1281 1541 1680 1825 1976 2133 2296 2465 2640 2821
;;=>    3816 4033 4256 4485 4720 4961 5461 5720 5985 6256 6533 6816
;;=>    8321 8640 8965 9296 9633 9976)))

;; Next, we can code some helper functions for returning the 'prefix'
;; (i.e. the first two digits) or the 'suffix' (last two digits) of
;; a given number, which we will use for determining the cycle and
;; also for filtering candidate numbers for the cycle.
(defun prefix (number)
  "Return the first two digits of NUMBER."
  (let ((number-digits (digits number)))
    (values
     (parse-integer
      (format nil "~d~d" (first number-digits) (second number-digits))))))

(defun suffix (number)
  "Return the last two digits of NUMBER."
  (let ((number-digits (digits number)))
    (values
     (parse-integer
      (format nil "~d~d" (third number-digits) (fourth number-digits))))))

(prefix 7751)
;;=> 77

(suffix 7751)
;;=> 51

;; As part of our test for a cycle, we want to enumerate all of the
;; candidates for the next link in the chain for a given number and
;; candidate pool (which will include their figurate number labels).
;; We can do that as follows:
(defun next-candidates (current pool)
  "Return the candidates from POOL that can come after CURRENT."
  (let ((candidates-set (remove-if
			 #'null
			 (loop for (type numbers) in pool
			       collect
			       (list type
				     (remove-if-not
				      (lambda (candidate)
					(= (suffix current) (prefix candidate)))
				      numbers)))
			 :key #'cadr)))
    (loop for number-set in candidates-set
	  append
	  (mapcar (lambda (number) (list (car number-set) number))
		  (cadr number-set)))))

(next-candidates 1123 figurate-numbers)
;;=> ((:TRIANGLE 2346) (:PENTAGONAL 2380) (:HEPTAGONAL 2356))

;; With that, we can finally write a recursive loop function that
;; extends a given chain of numbers out as far as it can go,
;; collecting up all of the different possible number alternatives
;; for the cycles.
(defun extend-chains (chains)
  "Recursively extend the given number CHAINS as far as possible."
  (loop for chain in chains
	append
	(let* ((types (mapcar #'car chain))
	       (current (cadar (last chain)))
	       (next (next-candidates current (figurate-except types))))
	  (if next
	      (extend-chains (mapcar (lambda (candidate)
				       (append chain (list candidate)))
				     next))
	      (list chain)))))

(extend-chains '(((:triangle 1035))))
;;=> (((:TRIANGLE 1035) (:PENTAGONAL 3577) (:SQUARE 7744)
;;=>   (:OCTAGONAL 4485))
;;=>  ((:TRIANGLE 1035) (:PENTAGONAL 3577) (:HEPTAGONAL 7756)
;;=>   (:SQUARE 5625) (:HEXAGONAL 2556))
;;=>  ((:TRIANGLE 1035) (:HEPTAGONAL 3553) (:SQUARE 5329))
;;=>  ((:TRIANGLE 1035) (:HEPTAGONAL 3553) (:PENTAGONAL 5370)
;;=>   (:SQUARE 7056))
;;=>  ((:TRIANGLE 1035) (:HEPTAGONAL 3553) (:HEXAGONAL 5356)
;;=>   (:SQUARE 5625)))

;; Finally, we simply map this function over the entire list of triangle
;; numbers (as starting numbers) to get all possible chains in a jiffy.
(time
 (defvar all-chains
   (extend-chains 
    (mapcar (lambda (number)
	      (list (list :triangle number)))
	    triangle-numbers))))
;;=> Evaluation took:
;;=>   0.354 seconds of real time
;;=>   0.359375 seconds of total run time (0.250000 user, 0.109375 system)
;;=>   [ Run times consist of 0.125 seconds GC time, and 0.235 seconds non-GC time. ]
;;=>   101.41% CPU
;;=>   915,848,912 processor cycles
;;=>   290,043,168 bytes consed
;;=>
;;=> ALL-CHAINS

;; Of these, we want the chain that has length 6 (so that we have
;; a representative from each group of figurate numbers), and which
;; has its last elements suffix match up with its first element's
;; prefix. This gives us our desired cycle (and we can find the sum).
(defvar cycle
  (car (remove-if-not
	(lambda (chain)
	  (and (= (length chain) 6)
	       (= (suffix (cadar (last chain))) (prefix (cadr (first chain))))))
	all-chains)))

cycle
;;=> ((:TRIANGLE 8256) (:SQUARE 5625) (:HEPTAGONAL 2512)
;;=>  (:OCTAGONAL 1281) (:HEXAGONAL 8128) (:PENTAGONAL 2882))

(reduce #'+ cycle :key #'cadr)
;;=> 28684
