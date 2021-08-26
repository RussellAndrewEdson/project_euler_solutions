;; Code for Project Euler Problem 54.
;;
;; Code author: Russell A. Edson
;; Date last modified: 26/08/2021

;; We are given a list of two-player poker hands that we first download
;; and parse from the Project Euler problem page. We are given that
;; the hands comprise 10 cards separated by spaces, and the first 5
;; cards belong to Player 1, and the last 5 cards belong to Player 2.
(ql:quickload '(:dexador :str))
(defparameter text-file
  "https://projecteuler.net/project/resources/p054_poker.txt")
(defparameter poker-hands
  (str:split #\newline (str:trim (dex:get text-file))))

;; The poker hands look like the following example:
(defvar example-hand "5H 5C 6S 7S KD 2C 3S 8S 8D TD")

;; Each card is represented as a string, but actually we want to
;; be able to talk about attributes of the cards like their
;; value and suit and compare cards in a clean way. So we'll define
;; some pesudo-OOP infrastructure here to allow us to deal with
;; the values and suits of these cards explicitly as a data abstraction:
(defun value (card)
  "Return the value of CARD."
  (str:substring 0 1 card))

(defun suit (card)
  "Return the suit of CARD."
  (str:substring 1 2 card))

(value "5H")
;;=> "5"

(suit "5H")
;;=> "H"

;; This is a 'good' start, but notice that we cannot easily
;; compare (numerically) the values of different cards, since
;; the value can be a string like "T" or "K" for example. So we'll
;; define a mapping to the natural numbers that lets us compare
;; card values in a straightforward way.
(defun comparative-value (card)
  "Returns a comparable numeric value for CARD, so 2<...<9<T<J<Q<K<A."
  (let ((values (list "2" "3" "4" "5" "6" "7" "8" "9" "T" "J" "Q" "K" "A")))
    (+ 2 (position (value card) values :test #'equalp))))

(comparative-value "TH")
;;=> 10

(comparative-value "KD")
;;=> 13

;; These functions suffice for us to be able to compare cards and
;; compute differences/duplicates, etc, by leveraging existing
;; functions for string and integer operations. We'll consider a
;; hand to (naturally) just be a list of these card strings, and so
;; we can write a quick function to parse a given 10-card hand and
;; split it into halves for each player's cards:
(defun deal-hand (hand)
  "Return lists for Player 1 and Player 2's cards from the given HAND."
  (let ((player-1 nil)
	(player-2 (str:split #\space hand)))
    (loop repeat 5 do (push (pop player-2) player-1))
    (values (reverse player-1) player-2)))

(deal-hand example-hand)
;;=> ("5H" "5C" "6S" "7S" "KD")
;;=> ("2C" "3S" "8S" "8D" "TD")

;; We can use the comparative card values to define a simple
;; sort function to get the cards in increasing order:
(defun sort-cards (cards)
  "Return the given CARDS sorted in increasing order of value."
  (sort (copy-list cards) #'< :key #'comparative-value))

(sort-cards '("5H" "TS" "QS" "2D" "7D" "7C"))
;;=> ("2D" "5H" "7D" "7C" "TS" "QS")

;; We can then easily define a function to pick out the largest value
;; from a given set of cards:
(defun largest-value-card (cards)
  "Return the comparative value for the largest value card in CARDS."
  (comparative-value (car (last (sort-cards cards)))))

(largest-value-card '("5H" "2D" "7C" "QH" "5H"))
;;=> 12

;; And in particular for the poker rankings, it is useful to know
;; how many of each value/suit are in a given set of cards, so
;; we can code up utility functions for those too. We'll return
;; convenient plist data structures for the counts:
(defun card-value-counts (cards)
  "Return the card value counts for each value in the given CARDS."
  (let ((unique-values
	  (remove-duplicates (mapcar #'value cards) :test #'equalp)))
    (loop for value in unique-values
	  append (list value
		       (count value cards :key #'value :test #'equalp)))))

(card-value-counts '("2D" "2S" "AS" "7C" "6C" "7H" "KD"))
;;=> ("2" 2 "A" 1 "6" 1 "7" 2 "K" 1)

(defun card-suit-counts (cards)
  "Return the card suit counts for each suit in the given CARDS."
  (let ((unique-suits
	  (remove-duplicates (mapcar #'suit cards) :test #'equalp)))
    (loop for suit in unique-suits
	  append (list suit
		       (count suit cards :key #'suit :test #'equalp)))))

(card-suit-counts '("2D" "2S" "AS" "7C" "6C" "7H" "KD"))
;;=> ("S" 2 "C" 2 "H" 1 "D" 2)

;; And we in turn use those utility functions to build up further
;; utility functions that will let us distinguish between flushes,
;; straights, pairs, etc.
(defun all-same-suit? (cards)
  "True if all of the CARDS have the same suit."
  (= 1 (length (loop for (key val) on (card-suit-counts cards)
		     by #'cddr
		     collect key))))

(all-same-suit? '("2D" "3D" "5D"))
;;=> T

(all-same-suit? '("2D" "3D" "6S" "5D"))
;;=> NIL

(defun all-consecutive-values? (cards)
  "True if all of the CARDS have consecutive values."
  (loop for (card . rest-of-cards) on (sort-cards cards)
	while rest-of-cards
	always (= 1 (- (comparative-value (car rest-of-cards))
		       (comparative-value card)))))

(all-consecutive-values? '("2D" "3D" "5D"))
;;=> NIL

(all-consecutive-values? '("2D" "3D" "5D" "4S"))
;;=> T

(defun n-values-match (cards &optional (n 2))
  "Return the matching cards when there are N value matches in CARDS."
  (let ((matches nil))
    (loop for (key val) on (card-value-counts cards)
	  by #'cddr
	  when (= val n)
	    do (push (remove-if-not
		      (lambda (card) (equalp (value card) key))
		      cards)
		     matches))
    matches))

(n-values-match '("2D" "2S" "AS" "7C" "6C" "7H" "KD") 2)
;;=> (("7C" "7H") ("2D" "2S"))

;; Finally, we combine everything together in a 'hand ranking'
;; function that returns the type of rank and the cards that make
;; up the rank.
(defun hand-ranking (hand)
  "Return the highest rank for the given HAND and the ranking cards."
  (setf hand (sort-cards hand))
  (let ((quadruples nil)
	(triples nil)
	(pairs nil)
	(leftover-cards hand))
    ;; First check for quadruples, triples and pairs in that order
    (setf quadruples (n-values-match leftover-cards 4))
    (setf leftover-cards (set-difference leftover-cards (car quadruples)))
    (setf triples (n-values-match leftover-cards 3))
    (setf leftover-cards (set-difference leftover-cards (car triples)))
    (setf pairs (n-values-match leftover-cards 2))

    (cond ((and (all-same-suit? hand)
		(equalp (list "T" "J" "Q" "K" "A") (mapcar #'value hand)))
	   (list :royal-flush (largest-value-card hand) hand))
	  ((and (all-same-suit? hand)
		(all-consecutive-values? hand))
	   (list :straight-flush (largest-value-card hand) hand))
	  ((not (null quadruples))
	   (list :four-of-a-kind
		 (comparative-value (caar quadruples))
		 (car quadruples)))
	  ((and (not (null triples))
		(not (null pairs)))
	   ;; Note: For a Full House, we consider only the value of
	   ;; the triple, and not the pair.
	   (list :full-house
		 (comparative-value (caar triples))
		 (append triples pairs)))
	  ((all-same-suit? hand)
	   (list :flush (largest-value-card hand) hand))
	  ((all-consecutive-values? hand)
	   (list :straight (largest-value-card hand) hand))
	  ((not (null triples))
	   (list :three-of-a-kind
		 (comparative-value (caar triples))
		 (car triples)))
	  ;; For a two-pair, we consider the value of the highest pair.
	  ((= (length pairs) 2)
	   (list :two-pairs
		 (largest-value-card (apply #'append pairs))
		 pairs))
	  ((not (null pairs))
	   (list :one-pair
		 (comparative-value (caar pairs))
		 (car pairs)))
	  (t
	   (list :high-card
		 (comparative-value (car (last hand)))
		 (last hand))))))

;; So e.g. we can use this ranking function as follows:
(defvar player-1 (nth-value 0 (deal-hand example-hand)))
(defvar player-2 (nth-value 1 (deal-hand example-hand)))

(hand-ranking player-1)
;;=> (:ONE-PAIR 5 ("5H" "5C"))

(hand-ranking player-2)
;;=> (:ONE-PAIR 8 ("8S" "8D"))

(hand-ranking '("2H" "2D" "4C" "4D" "4S"))
;;=> (:FULL-HOUSE 4 (("4C" "4D" "4S") ("2D" "2H")))

(hand-ranking '("QC" "4C" "QH" "2S" "4S"))
;;=> (:TWO-PAIRS 12 (("QC" "QH") ("4C" "4S")))

(hand-ranking '("2H" "7H" "QH" "3C" "AS"))
;;=> (:HIGH-CARD 14 ("AS"))

;; We use this ranking function to code a function to determine
;; a winner for a given 10-card hand, recalling that we break ties
;; by checking the values of the ranking cards first. In the event
;; of a tie we compare values for each card individually.
(defun ranking-order (ranking)
  "Return an integer between 0-9 quantifying how good the given RANK is."
  (let ((ranking-order (list :high-card :one-pair :two-pairs :three-of-a-kind
			     :straight :flush :full-house :four-of-a-kind
			     :straight-flush :royal-flush)))
    (position ranking ranking-order)))

(defun winner (hand)
  "Return the winner (Player 1 or 2) for the 10-card dealt poker HAND."
  (let ((winner nil))
    (multiple-value-bind (player-1 player-2) (deal-hand hand)
      ;; Compare hand rankings for each player
      (let* ((ranking-1 (hand-ranking player-1))
	     (ranking-order-1 (ranking-order (car ranking-1)))
	     (ranking-value-1 (cadr ranking-1))
	     (ranking-2 (hand-ranking player-2))
	     (ranking-order-2 (ranking-order (car ranking-2)))
	     (ranking-value-2 (cadr ranking-2)))

	;; When rankings differ, the highest rank wins
	(if (/= ranking-order-1 ranking-order-2)
	    (setf winner (if (< ranking-order-1 ranking-order-2) 2 1))

	    ;; Tie-breaker. In this case we look first at the ranking
	    ;; cards' values and pick the highest.
	    (if (/= ranking-value-1 ranking-value-2)
		(setf winner (if (< ranking-value-1 ranking-value-2) 2 1))

		;; Tie-breaker: now we check the values of each of the
		;; cards from highest to lowest to determine the winner.
		(let ((card-values-1 (mapcar #'comparative-value
					     (reverse (sort-cards player-1))))
		      (card-values-2 (mapcar #'comparative-value
					     (reverse (sort-cards player-2)))))
		  (block value-compare
		    (loop for i from 0 below (length card-values-1) do
		      (if (/= (nth i card-values-1) (nth i card-values-2))
			  (if (< (nth i card-values-1) (nth i card-values-2))
			      (progn
				(setf winner 2)
				(return-from value-compare))
			      (progn
				(setf winner 1)
				(return-from value-compare)))))))))))
    winner))

(winner example-hand)
;;=> 2

(winner "5D 8C 9S JS AC 2C 5C 7D 8S QH")
;;=> 1

(winner "2D 9C AS AH AC 3D 6D 7D TD QD")
;;=> 2

(winner "4D 6S 9H QH QC 3D 6D 7H QD QS")
;;=> 1

(winner "2H 2D 4C 4D 4S 3C 3D 3S 9S 9D")
;;=> 1

;; We loop through the given poker hands and count how many
;; times Player 1 wins.
(loop for hand in poker-hands when (= 1 (winner hand)) count hand)
;;=> 376
