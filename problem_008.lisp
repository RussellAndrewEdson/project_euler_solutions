;; Code for Project Euler Problem 8.
;;
;; Code author: Russell A. Edson
;; Date last modified: 15/07/2021

;; We want to grab an 1000-digit number and search through it to
;; find the 13 adjacent digits that yield the largest product.
;; As is typical for problems at this scale, we simply hit it
;; with the brute-force of modern computation: a 1000-digit
;; number is relatively small.

;; To be extra lazy and to make sure that we don't make a copy error,
;; we'll actually scrape the (large) number to be searched directly
;; from the Project Euler Archive page using lquery:
(ql:quickload '(:dexador :plump :lquery))
(defparameter url "https://projecteuler.net/problem=8")
(defvar parsed-url (lquery:$ (initialize (dex:get url))))

(defvar 1000-digit-num
  (parse-integer
   (remove #\newline
	   (elt (lquery:$ parsed-url ".problem_content .monospace" (text)) 0))))
1000-digit-num
;;=> 7316717653133062491922511967442657474235534919493496983520312774506
;;=> 3262395783180169848018694788518438586156078911294949545950173795833
;;=> 1952853208805511125406987471585238630507156932909632952274430435576
;;=> 6896648950445244523161731856403098711121722383113622298934233803081
;;=> 3533627661428280644448664523874930358907296290491560440772390713810
;;=> 5158593079608667017242712188399879790879227492190169972088809377665
;;=> 7273330010533678812202354218097512545405947522435258490771167055601
;;=> 3604839586446706324415722155397536978179778461740649551492908625693
;;=> 2197846862248283972241375657056057490261407972968652414535100474821
;;=> 6637048440319989000889524345065854122758866688116427171479924442928
;;=> 2308634656748139191231628245861786645835912456652947654568284891288
;;=> 3142607690042242190226710556263211111093705442175069416589604080719
;;=> 8403850962455444362981230987879927244284909188845801561660979191338
;;=> 7549920052406368991256071760605886116467109405077541002256983155200
;;=> 05593572972571636269561882670428252483600823257530420752963450

;; We have the number, so now we want to split it into individual
;; digits so we can choose 13 digits at a time:
(defun digits (number)
  "Return the set of digits in the given NUMBER."
  (mapcar #'digit-char-p (coerce (write-to-string number) 'list)))

;; And we can grab all the products at once by taking 13 digits at
;; a time and multiplying them together.
(defun product (nums)
  "Return the product of all of the numbers in NUMS."
  (reduce #'* nums))

(defvar products
  (let ((digits-list (digits 1000-digit-num))
	(sublist-length 13))
    (mapcar #'product
	    (loop for i from 0 upto (- (length digits-list) sublist-length)
		  collect
		  (subseq digits-list i (+ i sublist-length))))))

;; The largest product in the list is...
(apply #'max products)
;;=> 23514624000
