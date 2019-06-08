(in-package #:shiny)

;; TODO: keep the force 16beat padding (?
;; TODO: function to check if we are in beat N of the hexbeat
;;
;; Ideas took from:
;; https://github.com/kunstmusik/csound-live-code/
;; http://lisptips.com/post/44509805155/formatting-integers-in-different-radixes

;; choose   ~ cm:odds
;; cycle    - cm:cycle
;; contains - cl-user:position
;; remove   - cl-user:remove
;; rand     - cm:pickl
;; caose    - > velocity 0

(defvar *hexpitch-cache* (make-hash-table :test #'equal))

(defun :bits (number &optional (size 16))
  "(:bits 42) => 00101010"
  (format NIL "~v,'0B" size number))

(defun %hexbeat (hex)
  "takes a string with possible spaces or a single char, returns a binary string
   (%hexbeat \"A\") => \"1010\"
   (%hexbeat #\\a)  => \"1010\""
  (declare (type (or character string) hex))
  (let* ((hex-string  (if (characterp hex) (string hex) hex))
         (hex-string  (cl-ppcre:regex-replace-all " " hex-string ""))
         (decimal     (parse-integer hex-string :radix 16))
         (bin-length  (* 4 (length hex-string)))
         (bin-string  (:bits decimal bin-length)))
    bin-string))

(defun hexpat (s)
  "returns a cycle of a list of T or NIL, of hex pattern S, with variable nr of beats"
  (declare (type string s))
  (make-cycle (parse1 (%hexbeat s))))

(defun choose (prob)
  (declare (type (single-float 0f0 1f0) prob))
  (if (< (random 1f0) prob) 1 0))

;; aka "p4"
(defun beat ()
  "returns the current beat, double precision"
  (declare (type double-float *sample-rate*))
  (/ (now) (* *sample-rate* (spb *tempo*))))
(defun rbeat ()
  "returns the current beat, floor to integer"
  (floor (beat)))
(defun 16beat ()
  (mod (floor (beat))
       16))

(defun nth-beat (beat l)
  "something kind of:
   xosc(phsb(1.7), array(1,2,3,4))
   It changes cycles overtime, producing some kind stutter. Might be math wrong..."
  (declare (type list l))
  (nth (floor (* (length l)
                 (/ (mod (beat) (* beat 4 4))
                    (* beat 4 4))))
       l))

;;--------------------------------------------------
;; Helpers for the grammar

(defun safe-length (e)
  (if (numberp e)
      1
      (length e)))

;; AKA numpy roll"ish"
(defun roll (array n)
  (declare (type fixnum n))
  (let ((indices (cl-arrows:-> (length array)
                               (iota)
                               (rotate n))))
    (loop :for i :in indices
          :collect (aref array i))))

(defun add (&rest rest)
  "helper over numcl:+ to allow padding of not equal arrays.
   > (add (bit-smasher:hex->bits \"ff\")
          (bit-smasher:hex->bits \"a\"))
   #(1 1 1 1 2 1 2 1)"
  (let* ((max-length (reduce #'max (mapcar #'safe-length rest)))
         (same-length-rest
           (mapcar (lambda (r) (if (and (not (numberp r))
                                   (not (= max-length (length r))))
                              (serapeum:pad-start r max-length 0)
                              r))
                   rest)))
    (apply #'numcl:+ same-length-rest)))

(defun minus (&rest rest)
  "helper over numcl:- to allow padding of not equal arrays.
   > (minus (bit-smasher:hex->bits \"ff\")
            (bit-smasher:hex->bits \"a\"))
   #(1 1 1 1 0 1 0 1)"
  (let* ((max-length (reduce #'max (mapcar #'safe-length rest)))
         (same-length-rest
           (mapcar (lambda (r) (if (and (not (numberp r))
                                   (not (= max-length (length r))))
                              (serapeum:pad-start r max-length 0)
                              r))
                   rest)))
    (apply #'numcl:- same-length-rest)))

;;--------------------------------------------------
;; Grammar definition

;; TODO: (), & , | , ^, ... -, /
(cl-lex:define-string-lexer hexpitch-lexer
  ("x[A-Fa-f0-9]+" (return (values :hex (bit-smasher:hex->bits
                                         (subseq $@ 1)))))
  ("[0-9]+"        (return (values :dec (parse-integer $@))))
  ("-"             (return (values :minus :minus)))
  ("\\("           (return (values :left-paren   :left-paren)))
  ("\\)"           (return (values :right-paren  :right-paren)))
  ("~"             (return (values :concat :concat))) ;; D like concat...
  ;; ("&"             (return (values :and    :and)))
  ;; ("|"             (return (values :or     :or)))
  ("%"             (return (values :mod :mod)))
  (">>"            (return (values :rshift :rshift)))
  ("<<"            (return (values :lshift :lshift)))
  ("\\+"           (return (values :add :add)))
  ("\\*"           (return (values :mul :mul))))

(defun hexpitch-lex-line (string)
  "REPL helper to test lexer"
  (loop :with lexer := (hexpitch-lexer string)
        :for  tok   := (funcall lexer)
        :while tok
        :collect tok))

;; 82 * 5 + f34234234 * 3 + 2
;; https://en.cppreference.com/w/c/language/operator_precedence
(yacc:define-parser hexpitch-parser
  (:start-symbol expression)
  (:terminals (:add :mul :hex :dec :rshift :lshift :concat
                    :mod :left-paren :right-paren :minus))
  (:precedence ((:left :mul) (:left :mod)
                (:left :add) (:left :minus)
                (:left :rshift) (:left :lshift)
                (:left :concat)))
  (expression
   (expression :add    expression (lambda (x _ z) (declare (ignore _)) `(add ,x ,z)))
   (expression :minus  expression (lambda (x _ z) (declare (ignore _)) `(minus ,x ,z)))
   (expression :mul    expression (lambda (x _ z) (declare (ignore _)) `(numcl:* ,x ,z)))
   (expression :mod    expression (lambda (x _ z) (declare (ignore _)) `(numcl:mod ,x ,z)))
   (expression :rshift expression (lambda (x _ z) (declare (ignore _)) `(roll ,x (- ,z))))
   (expression :lshift expression (lambda (x _ z) (declare (ignore _)) `(roll ,x ,z)))
   (expression :concat expression (lambda (x _ z) (declare (ignore _)) `(concatenate 'vector ,x ,z)))
   term)
  (parenthosis (:left-paren expression :right-paren
                            (lambda (x y z) (declare (ignore x z)) y)))
  (term :dec
        :hex
        parenthosis))

;;--------------------------------------------------
;; Grammar usage

(defun hexpitch (s)
  (let* ((to-eval (yacc:parse-with-lexer (hexpitch-lexer s) hexpitch-parser))
         (earray  (eval to-eval)))
    (values (make-cycle (coerce earray 'list))
            earray
            to-eval)))

;; FIXME: when the same pattern is used it returns the same cycle...u can add a space in the end to workarount it
(defun hexpitchc (s)
  (declare (type string s))
  (or (gethash s *hexpitch-cache*)
      (setf (gethash s *hexpitch-cache*) (hexpitch s))))

;; FIXME: they get generated at different time (unaligned)...u can clrhash it
(defmacro hexplay (beat &body body)
  `(when (not (zerop (next (hexpitchc ,beat))))
     ,@body))
