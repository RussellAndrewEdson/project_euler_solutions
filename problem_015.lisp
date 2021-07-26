;; Code for Project Euler Problem 15.
;;
;; Code author: Russell A. Edson
;; Date last modified: 26/07/2021

;; We want the number of paths from the top-left corner to the
;; bottom-right corner of a 20x20 lattice where we can only move
;; rightward or downward.
;;
;; The trick with this problem is to not be daunted by its framing
;; in terms of 'lattices and paths'. Consider the following: for
;; the 1x1 lattice we have exactly two paths:
;;    --          |
;;      |   and   |__ ,
;;
;; right-and-down (or: RD) and down-and-right (or: DR) respectively.
;; Similarly and using this D/R shorthand, the given example of 6
;; paths for the 2x2 lattice has paths:
;;   RRDD  RDRD  RDDR  DRRD  DRDR  DDRR
;;
;; A pattern becomes obvious: we're really interested in the number
;; of symbol sequences RDDR, RDRD, etc, where we require that there
;; are an equal number of Rs and Ds (which corresponds to ending in
;; the bottom-right on the lattice). In this form the problem is
;; clearer as one of combinatorics. In fact, we have a closed-form
;; solution already: for the sequence of length m, where m/2 of the
;; symbols are identical (and the other m/2 are also identical), the
;; number of distinct sequences is
;;   C(m) = m! / [(m/2)!(m/2)!] .
;; And relating it back to the lattice by noticing that the nxn lattice
;; will have path sequences of length n+n, then our solution is C(n+n).

;; So for the 20x20 lattice, our solution is 40!/(20!*20!) and our
;; problem now becomes computing this number when the numerator and
;; denominator are both so large. Again, this is no issue for Common
;; Lisp's arbitrary-length integers:
(defvar number-of-paths
  (/ (reduce #'* (loop for n from 40 above 20 collecting n))
     (reduce #'* (loop for m from 20 downto 1 collecting m))))
;;=> 137846528820
