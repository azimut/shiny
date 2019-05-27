(in-package #:shiny)

;; Ideas took from:
;; https://github.com/kunstmusik/csound-live-code/

;; choose   - cm:odds
;; cycle    - cm:cycle
;; contains - cl-user:position
;; remove   - cl-user:remove
;; rand     - cm:pickl

;; /** Wrapper opcode that calls schedule only if iamp > 0. */
;; opcode cause, 0, Siiii
;;   Sinstr, istart, idur, ifreq, iamp xin
;;   if(iamp > 0) then
;;     schedule(Sinstr, istart, idur, ifreq, iamp)
;;   endif
;; endop

;; https://rosettacode.org/wiki/Bitwise_operations#Common_Lisp
(defun rotl (x width bits)
  "Compute bitwise left rotation of x by 'bits' bits, represented on 'width' bits"
  (logior (logand (ash x (mod bits width))
                  (1- (ash 1 width)))
          (logand (ash x (- (- width (mod bits width))))
                  (1- (ash 1 width)))))

;; http://lisptips.com/post/44509805155/formatting-integers-in-different-radixes
;; decimal to ...
(defun :hex (number &optional (size 8))
  "(:hex 666) => 029A"
  (format NIL "~v,'0X" size number))
(defun :bits (number &optional (size 16))
  "(:bits 42) => 00101010"
  (format NIL "~v,'0B" size number))
;; ... to decimal
(defun :bits2dec (value)
  "(:bits2dec \"1010\") => 10"
  (parse-integer value :radix 2))

;;--------------------------------------------------

(defun %hexbeat (hex)
  "(%hexbeat \"A\") => \"1010\"
   (%hexbeat #\\a)  => \"1010\""
  (declare (type (or character string) hex))
  (let* ((hex-string  (if (characterp hex) (string hex) hex))
         (hex-string  (cl-ppcre:regex-replace-all " " hex-string ""))
         (zero-length (* 4 (length hex-string)))
         (decimal     (parse-integer hex-string :radix 16))
         (bin-string  (format NIL "~v,'0B" zero-length decimal)))
    bin-string))

;;--------------------------------------------------

(defun hexpat (s)
  "returns a list of T or NIL, of pattern S, with variable nr of beats"
  (declare (type string s))
  (make-cycle (parse1 (%hexbeat s))))

;;--------------------------------------------------

;; TODO: formatting
(defun hexbeat (hex &optional (offset 0))
  "returns \"binary\" string of the HEX beat provided, padded to 16 beats"
  (declare (type string hex)
           (type fixnum offset))
  (let* ((decimal (parse-integer hex :radix 16))
         (decimal-offset
           (cond ((plusp offset)  (rotl decimal 16 offset))
                 ((minusp offset) (rotl decimal 16 offset))
                 ((zerop offset)  decimal)))
         (binary (format NIL "~v,'0B" 16 decimal-offset)))
    binary))

(defun %hexbeat-nths (hex)
  "helper, returns a list of beats of when the hexbeat HEX happens
   (%hexbeat-nths \"a09\") => (4 6 12 15)"
  (declare (type string hex))
  (let ((bbeats (hexbeat hex)))
    (declare (type string bbeats))
    (loop :for i :from 0
          :for b :across bbeats
          :when (eq #\1 b)
          :collect i)))

(defun hbeat (time hex)
  "utility to check if current beat is HEX, returns T if it is"
  (declare (type double-float time)
           (type string hex))
  (let ((nhex (%hexbeat-nths hex)))
    (declare (type list nhex))
    (loop :for n :in nhex
          :thereis (= n (mod (/ time *1beat*) 16)))))

(defun nth-beat (beat l)
  "something kind of:
   xosc(phsb(1.7), array(1,2,3,4))
   It changes cycles overtime, producing some kind stutter. Might be math wrong..."
  (declare (type list l))
  (let ((length (length l)))
    (nth
     (floor (* length
               (/ (mod (/ (node-uptime 0) *1beat*) (* beat 4)) (* beat 4))))
     l)))

(defun choose (prob)
  (declare (type (single-float 0f0 1f0) prob))
  (if (cm:odds prob) 1 0))
