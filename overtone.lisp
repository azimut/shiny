(in-package #:somecepl)
;; ----------- Overtone / Clojure

;; http://www.appservgrid.com/hyper/hyp/lisp#take
;; SOMECEPL> (take 2 '(0 1 2 3 4 5))
;; (0 1)

(defun take (n l)
  (cond ((< n 0) (error "index negative"))
        ((= n 0) ())
        ((null l) (error "index too large"))
        (t (cons (car l) (take (- n 1) (cdr l))))))

;; Taking a similar function to cycle
(defun repeat (n l &optional (nl '()))
  (if (zerop n)
      nl
      (repeat (- n 1)
              (rotate l 1)
              (if (not nl)
                  (list (first l))
                  (append nl (list (first l)))))))

(defun comp (a b)
  (lambda (&rest args)
    (funcall a (apply b args))))

(defun mapcat (function &rest lists)
  (apply #'concatenate 'list (apply #'mapcar function lists)))

(defun reductions (function sequence &rest args
                   &key key from-end (start 0) end initial-value)
  (declare (ignore key from-end start end initial-value))
  "Return a list of intermediate values from reducing SEQUENCE with FUNCTION."
  (let* ((reductions (list))
         (result (apply #'reduce
                        (lambda (&rest arguments)
                          (let ((result (apply function arguments)))
                            (push result reductions)
                            result))
                        sequence
                        args)))
    (values (or (nreverse reductions)
                (list result))
            result)))

;;;
;; pitch.clj
;;;

(defun to-keyword (s)
  (cond ((stringp s) (intern (string-upcase s) "KEYWORD"))
        ((keywordp s) s)))

(defun name (n)
  (cond ((keywordp n) (symbol-name n))
        ((stringp n) n)))

(defun octave-note (octave interval)
  "Convert an octave and interval to a midi note."
  (+ (* octave 12) interval 12))

(defparameter *notes* (alexandria:alist-hash-table
                       '((:C  . 0)  (:B# . 0)
                         (:C# . 1)  (:DB . 1)
                         (:D  . 2)
                         (:D# . 3)  (:EB . 3)
                         (:E  . 4)
                         (:E# . 5)  (:F  . 5)
                         (:F# . 6)  (:GB . 6)
                         (:G  . 7)
                         (:G# . 8)  (:AB . 8)
                         (:A  . 9)
                         (:A# . 10) (:BB . 10)
                         (:B  . 11) (:CB . 11))))

(defun notes (n)
  (if (keywordp n)
      (gethash n *notes*)
      (error "not a keyword")))

(defvar *reverse-notes* (alexandria:alist-hash-table
                         '((0  . :C)   (1 . :C#)
                           (2  . :D)   (3 . :Eb)
                           (4  . :E)   (5 . :F)
                           (6  . :F#)  (7 . :F#)
                           (8  . :Ab)  (9 . :A)
                           (10 . :Bb) (11 . :B))))

(defun reverse-notes (n)
  (if (and (< n 12) (>= n 0))
      (gethash n *reverse-notes*)
      (error "out of range")))

(defvar *midi-note-re-str* "([a-gA-G][#bB]?)([-0-9]+)")
(defvar *only-midi-note-re-str*
  (concatenate 'string "\\A" *midi-note-re-str* "\\Z"))

(defun midi-string-matcher (mk)
  "Determines whether a midi keyword is valid or not. If valid,
  returns a regexp match object"
  (multiple-value-bind (_ m) (cl-ppcre:scan-to-strings *only-midi-note-re-str*
                                                       (name mk))
    (declare (ignore _))
    m))

(defun validate-midi-string! (mk)
  (let ((matches (midi-string-matcher mk)))
    (unless matches
      (error "invalid midi-string"))
    (let ((octave (aref matches 1)))
      (when (< (parse-integer octave) -1)
        (error "invalid midi-string too low")))
    matches))

(defun canonical-pitch-class-name (pc)
  "Returns the canonical version of the specified pitch class pc."
  (reverse-notes (notes (to-keyword pc))))

(defun note-info (midi-string)
  (let* ((matches     (validate-midi-string! midi-string))
         (pitch-class (aref matches 0))
         (octave      (aref matches 1))
         (pitch-class (canonical-pitch-class-name pitch-class))
         (octave      (parse-integer octave))
         (interval    (notes (to-keyword pitch-class)))
         (midi-note   (octave-note octave interval)))
    (alexandria:alist-hash-table `((:pitch-class . ,pitch-class)
                                   (:octave      . ,octave)
                                   (:interval    . ,interval)
                                   (:midi-note   . ,midi-note)))))

(defun note (n)
  "Resolves note to MIDI number format. Resolves upper and lower-case
  keywords and strings in MIDI note format. If given an integer or
  nil, returns them unmodified. All other inputs will raise an
  exception.

  Usage examples:

  (note \"C4\")  ;=> 60
  (note \"C#4\") ;=> 61
  (note \"eb2\") ;=> 39
  (note :F#7)    ;=> 102
  (note :db5)    ;=> 73
  (note 60)      ;=> 60
  (note nil)     ;=> nil"
  (cond ((null n) nil)
        ((integerp n) (if (>= n 0)
                          n
                          (error "Value out of range.")))
        ((keywordp n) (note (symbol-name n)))
        ((stringp n)  (gethash :midi-note (note-info n)))
        (t (error "Bad argument."))))

#|
(defgeneric chord (root)
  (:documentation "return a chord, ov style"))
(defmethod chord ((root chord-name))
  (chord root chord-name 0))


|#
;; (defmethod chord ((root chord-name inversion))
;;   (let* ((root  (note root))
;;          (chord (resolve-chord chord-name))
;;          (notes (map)))))

#|
Scales can be quickly generated using the scale function, which takes a root note and the type of scale as arguments.

whelmed.play> (scale :C3 :major)
(48 50 52 53 55 57 59 60)

whelmed.play> (SCALE :purvi)
[1 3 2 1 1 3 1]

whelmed.play> (degrees->pitches [:i :ii :iii] :dorian :E3)
(52 54 55)

Scale degrees can be augmented by either + or - to denote the octave above or below the root of the scale, and can be sharped or flatted using # or b.

whelmed.play> (degrees->pitches [:i :ii :ii+ :ii#] :dorian :E3)
(52 54 66 55)

https://github.com/overtone/overtone/blob/36221f68733fc5921aeb60a2a8b10e99426f236d/src/overtone/music/pitch.clj
|#
