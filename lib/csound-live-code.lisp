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

(defun rotr (x width bits)
  "Compute bitwise right rotation of x by 'bits' bits, represented on 'width' bits"
  (logior (logand (ash x (- (mod bits width)))
                  (1- (ash 1 width)))
          (logand (ash x (- width (mod bits width)))
                  (1- (ash 1 width)))))

;; http://lisptips.com/post/44509805155/formatting-integers-in-different-radixes
;; decimal to ...
(defun :hex (value &optional (size 8))
  "(:hex 666) => 029A"
  (format NIL "~v,'0X" size value))
(defun :bits (value &optional (size 16))
  "(:bits 42) => 00101010"
  (format NIL "~v,'0B" size value))
;; ... to decimal
(defun :bits2dec (value)
  "(:bits2dec \"1010\") => 10"
  (parse-integer value :radix 2))

;; hexbeat
(defun %hexbeat (hex)
  "(%hexbeat \"A\") => \"1010\"
   (%hexbeat #\\a)  => \"1010\""
  (declare (type (or character string) hex))
  (let* ((hexs       (if (characterp hex) (string hex) hex))
         (decimal    (parse-integer hexs :radix 16))
         (bin-string (format NIL "~v,'0B" 4 decimal)))
    bin-string))

;; TODO: formatting
(defun hexbeat (hex &optional (offset 0))
  "returns binary string of the HEX beat provided"
  (declare (type string hex)
           (type fixnum offset))
  (let* ((decimal (parse-integer hex :radix 16))
         (decimal-offset
          (cond ((plusp offset)
                 (rotl decimal 16 (abs offset)))
                ((minusp offset)
                 (rotr decimal 16 (abs offset)))
                ((zerop offset) decimal)))
         (binary  (format NIL "~v,'0B" 16 decimal-offset)))
    binary))

(defun hexbeat-nths (hex)
  "returns a list of beats of when the hexbeat HEX happens
   ( \"a09\") => (4 6 12 15)"
  (declare (type string hex))
  (let ((bbeats (hexbeat hex)))
    (declare (type string bbeats))
    (loop
       :for i :from 0
       :for b :across bbeats
       :when (eq #\1 b)
       :collect i)))

(defun hbeat (time hex)
  "utility to check if current beat is HEX, returns T if it is"
  (declare (type double-float time)
           (type string hex))
  (let ((nhex (hexbeat-nths hex)))
    (declare (type list nhex))
    (loop
       :for n :in nhex
       :thereis (= n (mod (/ time (calc-beats 1)) 16)))))



