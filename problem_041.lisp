;; Code for Project Euler Problem 41.
;;
;; Code author: Russell A. Edson
;; Date last modified: 18/08/2021

;; We are interested in pandigital prime numbers, so we can reuse and
;; expand on various functions that have been coded in previous problems
;; (e.g. Problem 38 and Problem 37) for manipulating pandigital
;; numbers and prime numbers:
(defun digits (number)
  "Return the set of digits in the given NUMBER."
  (mapcar #'digit-char-p (coerce (write-to-string number) 'list)))

(defun combine-digits (digits)
  "Return the number we get by concatenating the given DIGITS together."
  (values (parse-integer (format nil "~{~d~}" digits))))

(defun n-pandigital? (number n)
  "True if the given NUMBER contains the digits 1 through N."
  (equalp
   (loop for i from 1 upto n collecting i)
   (sort (digits number) #'<)))

(defun is-prime? (n)
  "True if the given number N is prime, False if not."
  (if (= n 1)
      nil
      (let ((primep t))
	(loop for m from 2 upto (floor (sqrt n)) do
	  (if (zerop (mod n m))
	      (setf primep nil)))
	primep)))

(n-pandigital? 2143 4)
;;=> T

(is-prime? 2143)
;;=> T

;; We want to determine the largest n-digit pandigital prime.
;; Safely assuming that n must be between 1 and 9 (because '10' would
;; count as two digits), then our strategy should be to check the
;; 9-digit pandigital numbers for primality first, and then (failing
;; those) the 8-digit pandigital numbers, and so on.
;;
;; To speed things up, we'll make use of the awesome cl-permutation
;; library and simply permute known pandigital numbers. This is much
;; more efficient than looping over all n-digit integers.
(ql:quickload '(:cl-permutation))

(defun n-digit-pandigital-primes (n)
  "Loop through the N-digit pandigitals, collecting primes (if any)."
  (let ((primes nil))
    (cl-permutation:doperms (permutation n)
      (let ((number (combine-digits (cl-permutation:perm-to-list permutation))))
	(if (is-prime? number)
	    (setf primes (cons number primes)))))
    primes))
(defvar primes nil)

;; We'll check the 9-digit pandigital primes first:
(time
 (setf primes (n-digit-pandigital-primes 9)))
;;=> Evaluation took:
;;=>   173.926 seconds of real time
;;=>   173.750000 seconds of total run time (173.750000 user, 0.000000 system)
;;=>   [ Run times consist of 0.016 seconds GC time, and 173.734 seconds non-GC time. ]
;;=>   99.90% CPU
;;=>   450,817,185,767 processor cycles
;;=>   290,291,440 bytes consed
;;=>
;;=> NIL

;; Surprisingly(?) there were no 9-digit pandigital prime numbers,
;; so we try the 8-digit ones next:
(time
 (setf primes (n-digit-pandigital-primes 8)))
;;=> Evaluation took:
;;=>   6.019 seconds of real time
;;=>   6.015625 seconds of total run time (6.015625 user, 0.000000 system)
;;=>   99.95% CPU
;;=>   15,601,554,598 processor cycles
;;=>   31,586,352 bytes consed
;;=>
;;=> NIL

;; Still no pandigital primes. We check the 7-digit ones similarly:
(time
 (setf primes (n-digit-pandigital-primes 7)))
;;=> Evaluation took:
;;=>   0.218 seconds of real time
;;=>   0.218750 seconds of total run time (0.218750 user, 0.000000 system)
;;=>   100.46% CPU
;;=>   565,780,002 processor cycles
;;=>   3,794,416 bytes consed
;;=>
;;=> (2136457 2613547 2163547 2153647 6215347 6251347 6572143 5762143
;;=>  5627143 5621437 5261743 5726143 7526143 5216473 5214637 5214763
;;=>  5271463 5214367 2516473 2561743 2651437 2651743 2657143 2765143
;;=>  7625143 6725143 6257143 6251743 6217543 7621543 2761543 2176543
;;=>  2716543 7216543 7215643 2156437 2714563 2145763 7216453 2716453
;;=>  2761453 6214753 6214573 6214357 2146357 2143567 2413657 2641357
;;=>  6241537 2741653 2451367 2475163 2451637 2457613 7246513 2674513
;;=>  7264513 2547613 5724163 5274163 5247163 5241673 5246173 5246713
;;=>  5724613 5267413 5264173 5264137 5624137 5624713 5672413 5762413
;;=>  7562413 7652413 6572413 6527413 6524137 6542713 6547213 6754213
;;=>  5462137 5462173 5476213 5472613 5426713 5426173 5421673 5421763
;;=>  7542163 4521367 7452163 4572163 4521637 4652173 4765213 6452137
;;=>  6472513 7642513 7462513 4625713 4265137 4265713 4276513 4725613
;;=>  4257613 4257163 7421563 4271563 4216573 4216753 4721653 4621537
;;=>  6421573 4261357 4213567 4231567 4263157 6423517 6423751 6472351
;;=>  7426351 4235761 4723561 4253167 7425361 4253671 4253617 4725631
;;=>  7425631 4267531 4265731 4672531 6745231 4652317 4562317 4562731
;;=>  4567231 7456231 4526371 4523671 4527361 4752361 5423167 5742361
;;=>  5423617 5426371 5472631 5742631 5746231 5647231 5674231 7564231
;;=>  6574231 6572431 7562431 5624317 5276431 7524631 5274631 5243761
;;=>  5243167 2754361 2547361 2543617 2546317 2547631 2654317 2654371
;;=>  6257431 6245731 6274531 7624531 7264531 2674531 2647531 2645371
;;=>  2456731 2456371 2457361 2745361 7245361 2743561 2436517 2436571
;;=>  2476351 2467351 7264351 6724351 6243157 2431657 2341567 6234517
;;=>  2637451 2634517 2364517 2736451 7234651 2345617 2347561 2734561
;;=>  2354167 2375641 2735641 2376541 2365471 2637541 6235741 6235417
;;=>  6275341 7625341 2657341 2653741 2563417 2576341 7256341 7253641
;;=>  2534671 2537461 7253461 5234167 5723461 5236741 5237641 5726341
;;=>  5267341 5263417 5672341 7562341 5231647 2536147 2563147 6235147
;;=>  2315647 2631457 3214567 3214657 3216457 6321457 3261547 3215467
;;=>  3251467 6352147 3526147 5321467 5321647 5632741 7536241 5327461
;;=>  7352461 3524617 3526741 3752641 3756241 3562417 3675241 3765241
;;=>  6352741 6325471 6732541 7362541 3672541 3265741 3257641 3256471
;;=>  3256417 3254761 3725461 7324561 3245761 3246751 7324651 3264571
;;=>  3627451 3672451 3624157 3246157 3241657 3421567 3421657 3642157
;;=>  6342157 6342517 3642571 3462517 3462751 3467251 3427561 3742561
;;=>  3475261 3457261 3452671 3456217 3456721 3746521 3467521 3465271
;;=>  3674521 6734521 6347521 6345721 6345271 6354217 3576421 3756421
;;=>  7356421 3574621 3546721 3546271 3542761 7354261 3542167 5342167
;;=>  5342761 5347621 5734621 7536421 5376421 5367421 5634217 5634721
;;=>  5763421 5647321 5643217 5463217 5463721 7546321 5436721 5436271
;;=>  5436217 5473261 4537261 7456321 4563271 4563217 4657321 6475321
;;=>  6453721 6435721 6437521 4365271 7435621 4375621 4356721 4356217
;;=>  4732561 4325617 4326571 4372651 7432651 4362751 4637251 6473251
;;=>  4321657 4312657 4361257 4631527 6435127 4563127 5431627 5436127
;;=>  6543127 5364127 5346127 5341627 6345127 3456127 3451627 3415627
;;=>  3461257 3412657 3412567 3145627 3154267 3156427 3165427 3516427
;;=>  3514267 5631427 5361247 5312467 3512647 3561247 3152467 3126547
;;=>  3612547 6312547 3612457 3126457 3124657 3124567 1324567 1362457
;;=>  1632457 1356247 1532647 6513427 5136427 1563427 1653427 1356427
;;=>  1354267 1345627 6134257 1634257 1342657 1342567 1436257 1463257
;;=>  1436527 1435627 1453267 1645327 6145327 1546327 5143267 5614327
;;=>  6514327 5641327 4513627 4516327 4561327 4165327 4135627 7641253
;;=>  4176253 4716253 7412653 4172653 4127653 4126537 4125637 4125673
;;=>  4712563 7412563 4175263 4152763 4157623 7415623 4167523 6415237
;;=>  6451723 6457123 4675123 4657123 4561237 4561723 7451623 4571263
;;=>  7451263 7541623 5746123 5461723 5461273 6541723 7561423 5761423
;;=>  5164273 5164723 7514623 5146237 5142637 1546273 1574623 1564237
;;=>  7165423 6175423 6154723 6154273 6145273 6145723 6714523 1674523
;;=>  1647523 1465273 1476523 7145623 1452637 1425367 7142563 1742563
;;=>  1427563 1426753 1427653 1476253 1647253 1764253 7164253 6174253
;;=>  6142573 6124753 1672453 1624573 1264537 1264573 1726453 7126453
;;=>  7124653 1246573 1246537 1245763 1247563 1254367 7125463 1725463
;;=>  1257463 1254637 1275643 7126543 1276543 6152743 6175243 1576243
;;=>  7152643 1752643 1572643 1524637 1524763 5172463 5126437 5176243
;;=>  5162743 5162473 6751243 6512437 6512347 5126347 5123467 1562347
;;=>  1652347 1625347 1265347 1256347 1263547 1234657)

;; Now we have finally found some (a lot!) of the pandigital prime
;; numbers. We simply take the largest of these.
(apply #'max primes)
;;=> 7652413