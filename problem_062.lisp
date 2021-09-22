;; Code for Project Euler Problem 62.
;;
;; Code author: Russell A. Edson
;; Date last modified: 22/09/2021

;; In this problem we want to find the smallest cube for which
;; exactly five of its digit permutations are also cube. Rather
;; than doing the digit permutations, what we really want for this
;; task is a clever choice of data structure that will let us
;; efficiently keep track of the cubes that we've already
;; computed and the numbers of each digit in them. We'll do this
;; using a hash table lookup as follows.

;; First, note that we're interested in the numbers of each digit
;; that appear in the cubes. We'll write a series of functions to
;; count these up:
(defun digits (number)
  "Return the set of digits in the given NUMBER."
  (mapcar #'digit-char-p (coerce (write-to-string number) 'list)))

(defun digit-counts (number)
  "Return a list containing the digit counts (0-9) in NUMBER."
  (let ((counts (list 0 0 0 0 0 0 0 0 0 0)))
    (loop for digit in (digits number) do
      (incf (nth digit counts)))
    counts))

(digit-counts 41063625)
;;=> (1 1 1 1 1 1 2 0 0 0)

;; And since Common Lisp allows arbitrary objects to be used for keys
;; in a hash table, we simply use this list as a key for a value pair
;; consisting of a count (for rapid checking of the number of
;; permutations found) and a list of numbers with those digits that
;; we append to as we loop over the cubes.
(defparameter *cube-digits* (make-hash-table :test #'equalp))

(defun tally (cube)
  "Keep track of the digits seen in CUBE."
  (let* ((counts (digit-counts cube))
	 (current-tally (gethash counts *cube-digits*)))
    (if current-tally
	(progn
	  (incf (car current-tally))
	  (push cube (cadr current-tally))
	  (setf (gethash counts *cube-digits*) current-tally))
	(setf (gethash counts *cube-digits*)
	      (list 1 (list cube))))))

(tally (* 1 1 1))
;;=> (1 (1))

(tally (* 2 2 2))
;;=> (1 (8))

(loop for key being the hash-keys of *cube-digits*
	using (hash-value value)
      collect (list key value))
;;=> (((0 1 0 0 0 0 0 0 0 0) (1 (1))) ((0 0 0 0 0 0 0 0 1 0) (1 (8))))

;; In this representation, we can easily check for cube permutations.
;; For instance, if we loop up to past 405^3 we can confirm the given
;; example for 41063625 being the smallest cube with exactly
;; 3 cube permutations:
(time
 (loop for n from 3 upto 420 do (tally (* n n n))))
;;=> Evaluation took:
;;=>   0.000 seconds of real time
;;=>   0.000000 seconds of total run time (0.000000 user, 0.000000 system)
;;=>   100.00% CPU
;;=>   1,048,094 processor cycles
;;=>   261,216 bytes consed
;;=>
;;=> NIL

(loop for key being the hash-keys of *cube-digits*
	using (hash-value value)
      when (>= (car value) 2)
	collect (list key value))
;;=> (((0 1 1 0 0 1 0 0 0 0) (2 (512 125)))
;;=>  ((0 0 1 0 1 1 0 1 1 0) (2 (54872 42875)))
;;=>  ((3 1 1 0 0 1 0 0 0 0) (2 (512000 125000)))
;;=>  ((3 2 0 2 0 0 0 0 0 0) (2 (1331000 1030301)))
;;=>  ((2 2 1 0 0 0 1 0 1 0) (2 (8120601 1061208)))
;;=>  ((0 0 1 1 0 2 1 1 0 1) (2 (7529536 5639752)))
;;=>  ((1 2 0 1 0 0 1 1 1 1) (2 (11697083 10793861)))
;;=>  ((0 3 1 1 0 1 0 2 0 0) (2 (72511713 17173512)))
;;=>  ((0 2 1 1 0 0 1 2 0 1) (2 (67917312 21717639)))
;;=>  ((0 1 1 1 1 1 1 1 0 1) (2 (32461759 24137569)))
;;=>  ((0 2 1 1 0 1 1 0 1 1) (2 (39651821 35611289)))
;;=>  ((0 1 1 1 1 0 3 0 0 1) (2 (66923416 36264691)))
;;=>  ((1 1 1 1 1 1 2 0 0 0) (3 (66430125 56623104 41063625)))
;;=>  ((3 0 1 0 1 1 0 1 1 0) (2 (54872000 42875000)))
;;=>  ((1 3 0 0 1 1 1 0 1 0) (2 (51064811 45118016))))

;; Since this operation is reasonably efficient, we can simply pick
;; an appropriately large upper-bound for our continued tally loop
;; and then check the cubes afterward. (Note that we want a bit of
;; a buffer here: we're looking for the smallest cube that has
;; exactly five permutations, so we can't necessarily stop as soon
;; as we've found the fifth permutation, since that particular
;; set of cubes might have a 6th or a 7th permutation as well in
;; which case it isn't the solution. So we compute for a little
;; while longer so that we're more confident that we've found the
;; 5-set.)
(time
 (loop for n from 421 upto 10000 do (tally (* n n n))))
;;=> Evaluation took:
;;=>   0.032 seconds of real time
;;=>   0.031250 seconds of total run time (0.031250 user, 0.000000 system)
;;=>   96.88% CPU
;;=>   82,538,418 processor cycles
;;=>   6,873,936 bytes consed
;;=>
;;=> NIL

(loop for key being the hash-keys of *cube-digits*
	using (hash-value value)
      when (= (car value) 5)
	collect (list key value))
;;=> (((1 1 1 2 1 2 1 1 1 1)
;;=>   (5 (589323567104 569310543872 373559126408 352045367981
;;=>       127035954683)))
;;=>  ((1 1 1 2 1 1 2 1 1 1)
;;=>   (5 (936302451687 913237656408 613258407936 536178930624
;;=>       140283769536))))

;; So our smallest cube is 127035954683.
