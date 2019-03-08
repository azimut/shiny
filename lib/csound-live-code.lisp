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

;; http://lisptips.com/post/44509805155/formatting-integers-in-different-radixes
;; decimal to ...
(defun :hex (value &optional (size 4))
  "(:hex 666) => 029A"
  (format NIL "~v,'0X" size value))
(defun :bits (value &optional (size 8))
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
(defun hexbeat (hex &key offset)
  "returns binary string of the HEX beat provided"
  (declare (type string hex))
  (let* ((decimal (parse-integer hex :radix 16))
         (binary  (format NIL "~v,'0B" 16 decimal)))
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
       :for n-beat := (calc-beats n)
       :thereis (zerop (if (zerop n-beat) 0d0 (mod time n-beat))))))
