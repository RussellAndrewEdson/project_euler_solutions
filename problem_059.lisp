;; Code for Project Euler Problem 59.
;;
;; Code author: Russell A. Edson
;; Date last modified: 23/08/2021

;; We are given a ciphertext of encrypted ASCII codes in a text file
;; that we first download from the Project Euler problem page and
;; parse into a list appropriately:
(ql:quickload '(:dexador :str))
(defparameter text-file
  "https://projecteuler.net/project/resources/p059_cipher.txt")
(defparameter ciphertext (str:split #\, (dex:get text-file)))

;; Now we are given that the encryption key in this case is 3 lowercase
;; ASCII characters, and that the encryption scheme is simple
;; byte-wise XOR encryption. Fortunately Common Lisp already has
;; some functions that we can use here. code-char and char-code
;; convert back and forth between ASCII characters and number codes:
(char-code #\A)
;;=> 65

(code-char 42)
;;=> #\*

;; We can also use the inbuilt XOR functions if we define a conversion
;; between an ASCII integer and it's 8-bit representation (really 7-bit,
;; but we redundantly include the parity bit for the full byte).
(defun byte-representation (n)
  "Return the 8-bit binary representation of the given number N."
  (coerce
   (mapcar (lambda (i) (if (logbitp i n) 1 0))
	   (reverse (loop for i from 0 below 8 collecting i)))
   'bit-vector))

(byte-representation 65)
;;=> #*01000001

(defun integer-representation (bit-string)
  "Return the (decimal) integer representation of the 8-bit BIT-STRING."
  (loop for bit in (coerce (reverse bit-string) 'list)
	for two-power = 1 then (* 2 two-power)
	sum (* bit two-power)))

(integer-representation #*01000001)
;;=> 65

;; Then bit-wise XOR works as we'd like, so we can wrap everything
;; into a convenient integer (decimal) XOR function.
(bit-xor #*01000001 #*00101010)
;;=> #*01101011

(defun xor (m n)
  "Return the integer representation of M XOR N."
  (integer-representation (bit-xor (byte-representation m)
				   (byte-representation n))))

(xor 65 42)
;;=> 107

;; With these, we can define a function to perform the decryption
;; given the ciphertext and a key. We'll make clever use of a
;; circular list for the key characters, in which case the
;; decryption becomes a straightforward XOR mapping:
(defun circular-list (list)
  "Convert the given LIST into a circular list."
  (let ((list-copy (copy-list list)))
    (setf (cdr (last list-copy)) list-copy)
    list-copy))

(defun xor-decrypt (ciphertext key)
  "Return the XOR-decrypted plaintext codes for CIPHERTEXT using KEY."
  (mapcar #'xor ciphertext (circular-list key)))

(defvar test-text (mapcar #'char-code (coerce "Testing" 'list)))
test-text
;;=> (84 101 115 116 105 110 103)

(defvar test-key (mapcar #'char-code (coerce "mykey" 'list)))
test-key
;;=> (109 121 107 101 121)

(mapcar #'code-char (xor-decrypt test-text test-key))
;;=> (#\9 #\Fs #\Can #\Dc1 #\Dle #\Etx #\Rs)

(mapcar #'code-char (xor-decrypt (xor-decrypt test-text test-key) test-key))
;;=> (#\T #\e #\s #\t #\i #\n #\g)

;; Now we can begin actually decrypting the ciphertext by trying to
;; find the key. We are given that the key is three-characters long
;; and contains only lowercase characters (i.e. so the codes between 97
;; and 122), and that the plaintext contains common English words
;; (for instance, "that", "this", "from", etc).
(defun plaintext (key)
  "Return the plaintext string from decrypting *ciphertext* with KEY."
  (coerce
   (mapcar #'code-char
	   (xor-decrypt (mapcar #'parse-integer ciphertext) key))
   'string))

;;e.g.
(plaintext '(99 112 113))
;;=> "Gf!cputibr(ugcdh(gtgl&|ic(hh|silte|hif!in!ifd&gg&Mtjms!{!kgrr(bcdd
;;=>  dz`rme&x`vmsu$!$Ld&{tkehu(rczhcztk(sckhvzneisse#&SNh(unm!u}lu(n`(r
;;=>  czhc{!in!tmboxsik`j{\\<(H&``pm!tmbcfujq!`gthl-&yto|d&}ocpqckuclm
;;=>  $!gf!cddaior(d~xsc{rogo&nnt(unm!cfuozd&{tk(n`(unar&{dtadu(0&#!7'5&
;;=>  #!7'8&#!7'00(*&mue&-&ioki&ldvmob{!if!r`d&ytglsg|ttm!in!r`d&khtkm
;;=>  c$!ug!r``r(h`(unm!rztc(rse!in!r`hu(rczhc{!o{!ijugaocl-&nsie!o|!g|!
;;=>  ifbc(unm!w}`bz`r}sc(n`(unm!easedd&nnjdnq{/&F`kmm$!O(ig~d&nnsfe&|
;;=>  ig|!r`d&{tk(n`(unar&{dtadu(hu(`&{h~|i&x`t|!in!r`d&{psisc(n`(unm!vm
;;=>  soedrms&gg&|ic(bozbjm!q`num!ba`kmucz!o{!73!iz!dq!v}uraoa(unm!u}l&g
;;=>  g&|io{!umsomr&mpsim&|n&{-&au&``u(unm!tiuog!uysr 7/(lsduoxmome&jx&{
;;=>  !rg!7(n`(unm!vmsoedrms&|n&|ic(eoilc|dt&!O(vodm&{nif!u`nq(uniu&|ic(
;;=>  rse!in!r`hu(rczhc{!rg!dm!gxqtgyoe`rmm(0(>52122870054:72;723!gfe&
;;=>  nsie!k}mraqjqhho!r`hu(oseccz!dq!uay*(`hl!r`dh(ugchho!r`d&{psisc(si
;;=>  gu*(unm!h}ldms&;/7<03130=23081124;9&ar&aobmdb(qtgeskdb$!q`he`!cpqt
;;=>  mrumr&|ic(qczhkmucz!in!g(bozbjm!q`num!ba`kmucz!o{!7&!@gmjgvoff&ifg
;;=>  ao&|ic(rged&{ucxr&jx&ioki&A!nie&istawcl!g|!r`hu(rse-&A!niwc(eo{b
;;=>  i~dtme&|ig|!r`d&{tk(n`(unm!umsomr&9!-(0)97&#!7'97(*&9.4=7&#!7'74=!
;;=>  -(drk/&imug!bmqcfeu(nh(unm!w}`bz`r}sc(n`(unm!easedd((Ogedjq-&|ic(r
;;=>  se!in!r`hu(lsduoxmome&jx&11&ohpmr&|ic(coytglsg|d& gi}sr`!vgvcz(&gg
;;=>  &|ic(bozbsegczdhkd&gg&|ic(qczhkmucz!in!g(bozbjm!q`num!ba`kmucz!o{!
;;=>  7&!Gfe&jx&{hkamgz!tm`ugooff&A!niwc(mocdqarc(ccmo&icjm!rg!bmuczlofd
;;=>  &|ic(rser&gg&|ic(rsjrcytcfu&{dtadu(hh(vnabn(unm!cpqifdh|r&isc(dpmo
;;=>  &ftkjdt{/"

;; So we'll loop over every possible key combination until we find a
;; plaintext that 'makes sense'.
(defvar common-words
  (list "that" "this" "from" "when" "there" "where"))
(defvar keys-of-interest nil)
(time
 (loop for a from 97 upto 122 do
   (loop for b from 97 upto 122 do
     (loop for c from 97 upto 122 do
       (let ((has-words nil))
	 (loop for word in common-words do
	   (if (str:contains? word (plaintext (list a b c)))
	       (setf has-words t)))
	 (if has-words
	     (setf keys-of-interest (cons (list a b c) keys-of-interest))))))))
;;=> Evaluation took:
;;=>   227.226 seconds of real time
;;=>   226.546875 seconds of total run time (217.468750 user, 9.078125 system)
;;=>   [ Run times consist of 14.206 seconds GC time, and 212.341 seconds non-GC time. ]
;;=>   99.70% CPU
;;=>   588,965,639,992 processor cycles
;;=>   170,022,047,936 bytes consed
;;=>
;;=> NIL

(length keys-of-interest)
;;=> 41

;; There are still many candidate keys to check, so we can now look
;; at ordering them according to how many common words each contain.
(setf common-words (append common-words
			   (list "the" "of" "an" "you" "is" "it")))
(let ((word-counts nil))
  (loop for key in keys-of-interest do
    (let ((count 0))
      (loop for word in common-words
	    when (str:contains? word (plaintext key))
	      do (incf count))
      (setf word-counts (cons (list count key) word-counts))))
  word-counts)
;;=> ((3 (97 100 119)) (3 (97 114 115)) (2 (97 116 113))
;;=>  (2 (97 117 107)) (4 (98 97 107)) (2 (98 117 103))
;;=>  (3 (98 117 118)) (3 (98 121 121)) (2 (99 122 97))
;;=>  (2 (100 107 103)) (3 (100 113 119)) (3 (100 122 108))
;;=>  (8 (101 120 112)) (4 (102 120 122)) (3 (103 100 113))
;;=>  (3 (103 114 115)) (2 (104 97 112)) (1 (104 99 116))
;;=>  (3 (104 111 115)) (3 (104 111 119)) (3 (104 121 121))
;;=>  (1 (105 107 101)) (3 (105 111 108)) (4 (108 101 118))
;;=>  (4 (108 117 113)) (2 (109 105 106)) (1 (112 116 99))
;;=>  (3 (112 118 115)) (3 (114 119 108)) (2 (114 121 99))
;;=>  (3 (115 114 120)) (1 (116 97 100)) (3 (116 98 120))
;;=>  (2 (116 104 97)) (2 (116 115 101)) (1 (117 117 116))
;;=>  (2 (118 111 113)) (3 (119 105 107)) (3 (120 113 106))
;;=>  (5 (120 116 119)) (2 (121 121 114)))

;; By inspection, key (101 120 112) seems like a clear winner.
(mapcar #'code-char '(101 120 112))
;;=> (#\e #\x #\p)

(plaintext '(101 120 112))
;;=> "An extract taken from the introduction of one of Euler's most
;;=>  celebrated papers, \"De summis serierum reciprocarum\" [On the
;;=>  sums of series of reciprocals]: I have recently found, quite
;;=>  unexpectedly, an elegant expression for the entire sum of this
;;=>  series 1 + 1/4 + 1/9 + 1/16 + etc., which depends on the
;;=>  quadrature of the circle, so that if the true sum of this series
;;=>  is obtained, from it at once the quadrature of the circle
;;=>  follows. Namely, I have found that the sum of this series is a
;;=>  sixth part of the square of the perimeter of the circle whose
;;=>  diameter is 1; or by putting the sum of this series equal to s,
;;=>  it has the ratio sqrt(6) multiplied by s to 1 of the perimeter
;;=>  to the diameter. I will soon show that the sum of this series to
;;=>  be approximately 1.644934066842264364; and from multiplying this
;;=>  number by six, and then taking the square root, the number
;;=>  3.141592653589793238 is indeed produced, which expresses the
;;=>  perimeter of a circle whose diameter is 1. Following again the
;;=>  same steps by which I had arrived at this sum, I have discovered
;;=>  that the sum of the series 1 + 1/16 + 1/81 + 1/256 + 1/625 + etc.
;;=>  also depends on the quadrature of the circle. Namely, the sum of
;;=>  this multiplied by 90 gives the biquadrate (fourth power) of the
;;=>  circumference of the perimeter of a circle whose diameter is 1.
;;=>  And by similar reasoning I have likewise been able to determine
;;=>  the sums of the subsequent series in which the exponents are
;;=>  even numbers."

;; As our final answer, we return the sum of the ASCII values of the
;; plaintext.
(reduce #'+ (xor-decrypt (mapcar #'parse-integer ciphertext) '(101 120 112)))
;;=> 129448
