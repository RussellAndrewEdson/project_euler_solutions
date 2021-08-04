;; Code for Project Euler Problem 31.
;;
;; Code author: Russell A. Edson
;; Date last modified: 04/08/2021

;; We want to enumerate all of the different ways that 2 pounds
;; can be made using different coins:
;;   (1p, 2p, 5p, 10p, 20p, 50p, 1pound, 2pound).
;;
;; For us it is much more convenient to represent the 1 and 2 pound
;; coins as 100p and 200p respectively:
(defvar coins (list 200 100 50 20 10 5 2 1))

;; Our strategy here will be to start from 200p and successively
;; subtract coins, tallying up the ways as we go in a series of
;; nested loops. The code is somewhat ugly but it gets the job done
;; in a jiffy:
(defvar coin-combinations (list (list 1 0 0 0 0 0 0 0)))
(defvar different-ways 1)
(let ((total 200))
  (loop for 100p from 0 to (/ total (elt coins 1)) do
    (loop for 50p from 0 to (/ (- total (* 100 100p)) (elt coins 2)) do
      (loop for 20p from 0 to (/ (- total
				    (* 100 100p)
				    (* 50 50p))
				 (elt coins 3))
	    do
	       (loop for 10p from 0 to (/ (- total
					     (* 100 100p)
					     (* 50 50p)
					     (* 20 20p))
					  (elt coins 4))
		     do
			(loop for 5p from 0 to (/ (- total
						     (* 100 100p)
						     (* 50 50p)
						     (* 20 20p)
						     (* 10 10p))
						  (elt coins 5))
			      do
				 (loop for 2p from 0 to (/ (- total
							      (* 100 100p)
							      (* 50 50p)
							      (* 20 20p)
							      (* 10 10p)
							      (* 5 5p))
							   (elt coins 6))
				       do
					  (incf different-ways)
					  (setf
					   coin-combinations
					   (cons
					    (list 0 100p 50p 20p 10p 5p 2p
						  (- total
						     (* 100 100p)
						     (* 50 50p)
						     (* 20 20p)
						     (* 10 10p)
						     (* 5 5p)
						     (* 2 2p)))
					    coin-combinations)))))))))

different-ways
;;=> 73682

(mapcar #'list coins (elt coin-combinations 12345))
;;=> ((200 0) (100 0) (50 1) (20 2) (10 6) (5 0) (2 5) (1 40))
