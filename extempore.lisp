(in-package :somecepl)

;;;
;; Helpers
;;;
(defvar TWOPI 6.283185)
(defun filter (fn 1st) 
  (let ((ace nil)) 
    (dolist (x 1st) 
      (let ((val (funcall fn x))) 
        (if val (push val ace)))) 
    (nreverse ace))) 
(defun range (max &key (min 0) (step 1))
   (loop for n from min below max by step
      collect n))

(defvar *phrygian* '(0 1 3 5 7 8 10))

; Define basic diatonic major
(defvar *diatonic-major*
   '((i . (0 . ^))
     (i6 . (0 . ^6))
     (i64 . (0 . ^64))
     (i7 . (0 . ^7))
     (i- . (0 . -))
     (i-7 . (0 . -7))
     (n . (1 . ^)) ; neopolitan
     (n6 . (1 . ^6)) ; neopolitan
     (ii . (2 . -))
     (ii6 . (2 . -6))
     (ii7 . (2 . -7))
     (ii9 . (2 . -9))
     (ii^ . (2 . ^))
     (ii^7 . (2 . ^7))
     (iii . (4 . -))
     (iii6 . (4 . -6))
     (iii7 . (4 . -7))
     (iii^ . (4 . ^))
     (iii^7 . (4 . ^7))
     (iv . (5 . ^))
     (iv6 . (5 . ^6))
     (iv7 . (5 . ^7))
     (iv- . (5 . -))
     (iv-7 . (5 . -7))
     (v . (7 . ^))
     (v6 . (7 . ^6))
     (v7 . (7 . 7))
     (v- . (7 . -))
     (v-7 . (7 . -7))
     (b6 . (8 . ^))
     (vi . (9 . -))
     (vi6 . (9 . -6))
     (vi7 . (9 . -7))
     (vi^ . (9 . ^))
     (vi^7 . (9 . ^7))
     (b7 . (10 . ^))
     (viio . (11 . o))
     (viio7 . (11 . o7))
     (vii . (11 . o))
     (vii7 . (11 . -7b5))
     ))

;; Define basic diatonic minor
(defvar *diatonic-minor*
   '((i . (0 . -))
     (i6 . (0 . -6))
     (i64 . (0 . -64))
     (i7 . (0 . -7))
     (i^ . (0 . ^))
     (i^6 . (0 . ^6))
     (i^64 . (0 . ^64))
     (i^7 . (0 . ^7))
     (n . (1 . ^)) ; neopolitan
     (n6 . (1 . ^6)) ; neopolitan
     (ii . (2 . o))
     (ii6 . (2 . o6))
     (ii7 . (2 . o7))
     (ii- . (2 . -))
     (ii-6 . (2 . -6))
     (ii-7 . (2 . -7))
     (ii^ . (2 . ^))
     (ii^7 . (2 . ^7))
     (iii . (3 . ^))
     (iii6 . (3 . ^6))
     (iii7 . (3 . ^7))
     (iii- . (3 . -))
     (iii-6 . (3 . -6))
     (iii-7 . (3 . -7))
     (iv . (5 . -))
     (iv6 . (5 . -6))
     (iv7 . (5 . -7))
     (iv^ . (5 . ^))
     (iv^6 . (5 . ^6))
     (iv^7 . (5 . ^7))
     (v . (7 . ^)) 
     (v^ . (7 . ^))
     (v6 . (7 . ^6))
     (v7 . (7 . 7))
     (v- . (7 . -))
     (v-6 . (7 . -6))
     (v-6 . (7 . -6))
     (v-7 . (7 . -))
     (vi . (8 . ^))
     (vi6 . (8 . ^6))
     (vi7 . (8 . ^7))
     (vi- . (8 . -))
     (vi-6 . (8 . -6))
     (vi-7 . (8 . -7))
     (vii . (10 . ^))
     (vii6 . (10 . ^6))
     (vii7 . (10 . ^7))
     (viio . (11 . o)) ;raised 7 (dim)
     (viio6 . (11 . o6)) ;raised 7 (dim)
     (viio7 . (11 . o7)) ; raised 7 (dim)
     ))

; Define basic chord symbols
(defvar *chord-syms*
   '((^ . (0 4 7))
     (^sus . (0 5 7))
     (^6 . (4 7 0))
     (^64 . (7 0 4))
     (^7 . (0 4 7 11))
     (^65 . (4 7 11 0))
     (^43 . (7 11 0 4))
     (^42 . (11 0 4 7))
     (^2 . (11 0 4 7))
     (^7#4 . (0 4 7 11 6))
     (^9 . (0 4 7 11 2))
     (7 . (0 4 7 10))
     (9 . (0 4 7 10 2))
     (65 . (4 7 10 0))
     (43 . (7 10 0 4))
     (2 . (10 0 4 7))
     (42 . (10 0 4 7))
     (- . (0 3 7))
     (-sus . (0 5 7))
     (-6 . (3 7 0))
     (-64 . (7 0 3))
     (-7 . (0 3 7 10))
     (-65 . (3 7 10 0))
     (-43 . (7 10 0 3))
     (-42 . (10 0 3 7))
     (-2 . (10 0 3 7))
     (-9 . (0 3 7 10 2))
     (o . (0 3 6))
     (o6 . (3 6 0))
     (o64 . (6 0 3))
     (o7 . (0 3 6 8))
     (o65 . (3 6 8 0))
     (o43 . (6 8 0 3))
     (o42 . (8 0 3 6))
     (o2 . (8 0 3 6))
     (-7b5 . (0 3 6 9))))

;; various scales defined as pc sets
(defvar *scales*
  '((pentatonic . (2 2 3 2))
    (ryukyu . (4 1 2 4))
    (wholetone . (2 2 2 2 2)) 
    (chromatic . (1 1 1 1 1 1 1 1 1 1 1)) 
    (octatonic . (2 1 2 1 2 1 2)) 
    (messiaen1 . (2 2 2 2 2)) 
    (messiaen2 . (2 1 2 1 2 1 2)) 
    (messiaen3 . (2 1 1 2 1 1 2 1)) 
    (messiaen4 . (1 1 3 1 1 1 3)) 
    (messiaen5 . (1 4 1 1 4)) 
    (messiaen6 . (2 2 1 1 2 2 1)) 
    (messiaen7 . (1 1 1 2 1 1 1 1 2)) 
    (harmonic . (2 1 2 2 1 3)) 
    (ionian . (2 2 1 2 2 2)) 
    (dorian . (2 1 2 2 2 1)) 
    (phrygian . (1 2 2 2 1 2)) 
    (lydian . (2 2 2 1 2 2)) 
    (lydian-dominant . (2 2 2 1 2 1)) 
    (lydian-mixolydian . (2 1 2 1 2 1 2)) 
    (mixolydian . (2 2 1 2 2 1)) 
    (aeolian . (2 1 2 2 1 2)) 
    (locrian . (1 2 2 1 2 2))))

;; returns a scale based on a chord (standard jazz translations)
(defvar *pc-chord->scale*
   '((i . (0 . ionian))
     (i7 . (0 . ionian))
     (ii . (2 . dorian))
     (ii7 . (2 . dorian))
     (ii9 . (2 . dorian))
     (iii . (4 . phrygian))
     (iii7 . (4 . phrygian))
     (iv . (5 . lydian))
     (iv7 . (5 . lydian))
     (v . (7 . mixolydian))
     (v7 . (7 . mixolydian))
     (vi . (9 . aeolian))
     (vi7 . (9 . aeolian))
     (vii . (11 . locrian))
     (vii7 . (11 . locrian))))

(defun pc-random (lower upper pc)
  "select random pitch from pitch class
bounded by lower and upper (inclusive lower exclusive upper)

arg 1: lower bound (inclusive)
arg 2: upper bound (exclusive)
arg 3: pitch class

returns -1 if no valid pitch is possible"
  (if (not pc)
      -1
      (let ((choices (filter (lambda (x) (if (ispitch x pc) x))
                             (range upper :min lower))))
        (if (not choices)
            -1
            (random-list choices)))))

(defun pc-chord (root type)
  "returns a chord given a root and type
see *pc:chord-syms* for currently available types

e.g. (pc:chord 0 '^7)  => '(0 4 7 11)"
  (let ((chord (assoc type *chord-syms*)))
    (if chord
        (labels ((f (l newlst)
                   (if (not l)
                       (reverse newlst)
                       (f (cdr l)
                          (cons (mod (+ (car l) root) 12)
                                newlst)))))
          (f (cdr chord) '() )))))

(defun pc-chord-options (root maj-min pc)
  "returns chord options for root in maj-min key of pc
e.g. (pc:chord-options 0 '^ (pc:scale 0 'ionian))
=> ((0 4 7) (0 4 7 11) (0 5 7) (0 4 7 11 2) (0 4 7 11 6))"
  (let ((major7 '(^ ^7 ^sus ^9 ^7#4))
        (dom7   '(^ 7 ^sus 9))
        (minor7 '(- -7 -sus -9))
        (dim7   '(o -7b5 o7))
        (degree  (pc-degree root pc)))
    (mapcar (lambda (sym) (pc-chord root sym))
            (if (equal maj-min '^)
                (case degree
                  ((-1) '())
                  ((1 4) major7)
                  ((5) dom7)
                  ((2 3 6) minor7)
                  ((7) dim7))
                (case degree
                  ((-1) '())
                  ((1 4 6) minor7)
                  ((3) major7)
                  ((5) (append minor7 dom7))
                  ((2) dim7)
                  ((7) (append dom7 dim7)))))))

(defun pc-relative (pitch i pc)
  "select pitch from pitch class relative to a given pitch
 1st: bass pitch
 2nd: pc relationship to bass pitch (max is abs 7)
 3rd: pitch class

 example:
 (pc:relative 64 -2 '(0 2 4 5 7 9 11)) => 60
 (pc:relative 69 3 '(0 2 4 5 7 9 11)) => 74"
  (setf i (round i))
  (if (= i 0)
      pitch
      (let* ((inc (if (< i 0) '- '+)))
        (labels ((f (p cnt)
                   (progn (if (ispitch p pc)
                              (setf cnt (funcall inc cnt 1)))
                          (if (= cnt i)
                              p
                              (f (funcall inc p 1) cnt)))))
          (f (funcall inc pitch 1) 0)))))

(defun pc-degree (value pc)
  "Returns a scale degree of a given value (pitch) based on a pc"
  (labels ((f (i lst)
             (if (null lst)
                 -1
                 (if (= (car lst) (mod value 12))
                     i
                     (f (1+ i) (cdr lst))))))
    (f 1 pc)))

(defun pc-diatonic (root maj-min degree)
  "returns a chord following basic diatonic harmony rules
 based on root (0 for C etc.) maj/min ('- or '^) and degree (i-vii)
 ex: (pc:diatonic 0 '- 'i) => (0 3 7)"
  (if (typep degree 'integer)
      (setf degree (cdr (assoc degree '((0 i)   (1 ii)   (2 ii)
                                        (3 iii) (4 iii)  (5 iv)
                                        (6 iv)  (7 v)    (8 vi)
                                        (9 vi)  (10 vii) (11 vii))))))
  (let ((val (assoc degree (if (eq '^ maj-min)
                               *diatonic-major*
                               *diatonic-minor*))))
    (pc-chord (mod (+ root (cadr val)) 12) (cddr val))))

(defun cosr (centre amplitude period)
  "- the first argument is the \"centre\" to oscillate around
   - the second argument is the \"amplitude\" of the oscillation
   - the third argument is the \"period\" of the oscillation. 
   Ex: (cosr 5 3 1/2)"
  (+ centre
     (* amplitude
        (cos (* TWOPI (float (/ (incudine.util:sample->int (now)) 36000)) period)))))
        ;;(cos (* TWOPI (now) period)))))

(defun rcosr (centre amplitude period)
  (round (cosr centre amplitude period)))

(defun ispitch (pitch pc)
  "A predicate for calculating if pitch is in pc
arg 1: pitch to check against pc
arg 2: pc to check pitch against
retuns true or false"
  (if (member (mod pitch 12) pc) t))

(defun pc-quantize (pitch-in pc)
  "Always slelects a higher value before a lower value where distance is equal.
 arg 1: pitch to quantize to pc
 arg 2: pc to quantize pitch against
 returns quantized pitch or #f if non available"
    (labels ((f (inc pitch)
               (cond ((ispitch  (+ pitch inc) pc) (+ pitch inc))
                     ((ispitch  (- pitch inc) pc) (- pitch inc))
                     ((< inc 7) (f (+ inc 1) pitch))
                     (t (print "Derp!") nil))))
      (f 0 (round pitch-in))))

(defun pc-quantize-low (pitch-in pc)
  "Always slelects a lower value before a higher value where distance is equal.
 arg 1: pitch to quantize to pc
 arg 2: pc to quantize pitch against
  returns quntized pitch or #f if non available"
    (labels ((f (inc pitch)
               (cond ((ispitch  (- pitch inc) pc) (- pitch inc))
                     ((ispitch  (+ pitch inc) pc) (+ pitch inc))
                     ((< inc 7) (f (+ inc 1) pitch))
                     (t (print "Derp!") nil))))
      (f 0 (round pitch-in))))

(defun pc-quantize-list (lst pc)
  (mapcar (lambda (_) (pc-quantize _ pc))
          lst))

(defun ivl-retrograde (args) (reverse args))

(defun ivl-invert (lst &rest args)
  "invert list paying no attention to key"
  (let ((pivot (if (null args)
                   (car lst)
                   (car args))))
    (cons (car lst) (mapcar (lambda (_) (- pivot (- _ pivot)))
                            (cdr lst)))))

(defun ivl-transpose (val lst)
  "transpose list paying no attention to key"
  (mapcar (lambda (_) (+ _ val))
          lst))

;; TODO ivl:expand/contract

(defun pc-invert (lst pc &rest args)
  "invert the values of lst quantizing to pc"
  (if (null args)
      (pc-quantize-list (ivl-invert lst) pc)
      (pc-quantize-list (ivl-invert lst (car args)) pc)))

(defun pc-transpose (val lst pc)
  "transpose the values of lst quantizing to pc"
  (pc-quantize-list (ivl-transpose val lst) pc))

;; TODO ivl:expand/contract

(defun pc-chord->scale (root type)
  (scale (mod (+ (cadr (assoc type *pc-chord->scale*)) root) 12)
         (cddr (assoc type *pc-chord->scale*))))

;; ;; cosr with pc
;; (define-macro (qcosr pc . args)
;;   `(pc:quantize (cosr ,@args) ,pc))

(defun qcosr (pc center amplitude period)
  (pc-quantize (cosr center amplitude period) pc))

(defun scale (root type)
  "scheme<7099> (pc:scale 0 'aeolian)
=> (0 2 3 5 7 8 10) 
returns a scale type based on a given root"
  (if (assoc type *scales*)
      (labels ((f (l current newlst)
                 (if (not l)
                     (reverse (cons current newlst))
                     (f (cdr l)
                        (mod (+ current (car l)) 12)
                        (cons current newlst))
                     )))
        (f (cdr (assoc type *scales*))
           (mod root 12)
           '()))
      nil))

;;;
;; pc:make-chord
;;;
(defun make-chord (lower upper number pc)
  "creates a list of "number" pitches between "lower" and "upper"
bounds from the given "pc".  a division of the bounds
by the number of elements requested breaks down the selection into
equal ranges from which each pitch is selected.
make-chord attempts to select pitches of all degrees of the pc.
it is possible for elements of the returned chord to be -1 if no
possible pc is available for the given range.
non-deterministic (i.e. result can vary each time)

arg1: lower bound (inclusive)
arg2: upper bound (exclusive)
arg3: number of pitches in chord
arg4: pitch class

example: c7
(pc:make-chord 60 85 4 '(0 4 7 10)) => (60 70 76 79)"
  (let ((chord '()))
    (labels ((f (l u n p)
               (if (< n 1)
                   (mapcar (lambda (x) (round x))
                           (sort (remove -1 chord) '<))
                   (let* ((range (- u l))
                          (gap   (round (/ range n)))
                          (pitch (pc-random l (+ l gap) p)))
                     (if (< pitch 0)
                         (setf chord (cons (pc-random lower upper p)
                                           chord))
                         (setf chord (cons pitch chord)))
                     (f (+ l gap)
                        u
                        (- n 1)
                        (if (> (length p) 1)
                            (remove (mod (car chord) 12) p)
                            pc))))))
      (f (round lower) (round upper) number pc))))

;;;
;; From extempore - Scheme - instruments_ext-scm.xtm
;;;
;;

(defun note-name-to-midi-number (name)
  "scheme<7099> (regex:matched \"F#1\" \"([abcdefgABCDEFG])([#b])?(-?[0-9])\") => (\"F#1\" \"F\" \"#\" \"1\")"
  (let ((result (multiple-value-bind (a b) (cl-ppcre:scan-to-strings "([abcdefgABCDEFG])([#b])?(-?[0-9])" name)
                  (append (list a) (coerce b 'list)))))
        (if (null result)
            nil
            (let ((offset (+ 12 (* (parse-integer (cadddr result)) 12)))
                  (pc    (case (mod (- (mod (char-code (coerce (cadr result) 'character)) 16) 3) 7)
                                ((0) 0) ((1) 2) ((2) 4) ((3) 5) ((4) 7) ((5) 9) ((6) 11) (otherwise 0))))
              (+ offset pc
                 (cond ((string= (caddr result) "#")  1)
                       ((string= (caddr result) "b") -1)
                       (t 0)))))))

;;; ----------------------------
;; Extempore - Rhythm functions
;; -----------------------------

(defun make-metro (start-tempo &rest args)
  "(define *metro* (make-metro 120))
creates a metronome object
metro is basically a linear function that returns
a time in absolute samples when given a time in beats.

metro is instantiated with a starting tempo.
you can call the metro with the following symbols

'get-time ; which is also the default
'get-beat
'get-tempo
'set-tempo
'get-cycle
'set-cycle
'pos
'dur"
  (let* ((offset (if (null args) (round (now)) (caar args))) ; args
         (cycle 4)
         (mark offset)
         (loffset 0.0)
         (total-beats (if (null args) 0 (cdar args))) ; args
         (cycle-beats total-beats)
         (g-tempo (/ 60 start-tempo))
         (beat-pos (lambda (x1 y1 x2 y2)
                     (let* ((m (if (= 0 (- x2 x1)) 0 (/ (- y2 y1) (- x2 x1))))
                            (c (- y1 (* m x1))))
                       (lambda (time)
                         (+ (* time m) c)))))
         (beat-env (funcall beat-pos mark total-beats
                            (+ mark (* g-tempo *sample-rate*))
                            (+ total-beats 1)))
         (samp-env (funcall beat-pos total-beats mark
                            (+ total-beats 1)
                            (+ mark (* g-tempo *sample-rate*)))))
    (lambda (sym &rest args)
      (cond ((numberp sym)
             (+ (funcall samp-env sym) loffset))
            ((equal sym 'get-mark)
             (cons mark total-beats))
            ((equal sym 'get-time)
             (+ (funcall samp-env (car args)) loffset))
            ((equal sym 'get-cycle) cycle)
            ((equal sym 'get-cycle-mark) cycle-beats)
            ((equal sym 'set-cycle)
             (setf cycle-beats (cadr args))
             (setf cycle (car args)))
            ((equal sym 'pos)
             (mod (- (car args) cycle-beats) cycle))
            ((equal sym 'beat-at-time)
             (funcall beat-env (car args))) ;; FIXME
            ((equal sym 'set-tempo)
             (let ((time (if (null (cdr args)) (round (now)) (cadr args))))
               (if (or (null (cdr args))
                       (null (cddr args)))
                   (setf total-beats
                         (+ total-beats (/ (- time mark)
                                           (* *sample-rate* g-tempo))))
                   (setf total-beats (caddr args)))
               (setf g-tempo (/ 60 (car args)))
               (setf mark time)
               (setf samp-env (funcall beat-pos total-beats
                                       mark
                                       (+ total-beats 1)
                                       (+ mark (* g-tempo *sample-rate*))))
               (setf beat-env (funcall beat-pos mark
                                       total-beats
                                       (+ mark (* g-tempo *sample-rate*))
                                       (+ total-beats 1)))
               (car args)))
            ((equal sym 'get-tempo) (* (/ 1 g-tempo) 60))
            ((equal sym 'dur) (* *sample-rate* g-tempo (car args)))
            ((equal sym 'push) (setf loffset (+ loffset 256)))
            ((equal sym 'pull) (setf loffset (- loffset 256)))
            ((equal sym 'get-beat)
             (let ((val (+ total-beats
                           (/ (- (round (now)) mark)
                              (* *sample-rate* g-tempo))))
                   (quantize (if (null args) 1.0 (car args))))
               (round (+ val (- quantize (mod val quantize))))))
            (t 'bad-method-name)
            ))))

(defun make-metre (metre base)
  "creates a meter where metre is a list of numerators
and base is a shared denominator (relative to impromptu beats. i.e. 1 = crotchet,  0.5 = eighth etc.)
e.g. 
     (define *metre* (make-metre '(2 3 2) 0.5)) = 2/8 3/8 2/8 rotating cycle.

then call meter with time and beat if beat matches time then #t else #f

e.g. give the above define
     (*metre* 2.5 1.0) => #t because 0.0 = 1, 0.5 = 2, 1.0 = 1, 1.5 = 2, 2.0 = 3, 2.5 = 1, 3.0 = 2 and repeat."
  (let ((metre-length (apply '+ metre)))
    (lambda (time &rest beat)
      (let ((b (do ((qtime (mod (/ time base) metre-length))
                    (lst metre (cdr lst))
                    (valuea (car metre) (+ valuea (cadr lst)))
                    (valueb 0 (+ valueb (car lst))))
                   ((< qtime valuea)
                    (+ 1.0 (- qtime valueb))))))
        (if (null beat)
            b
            (if (= (car beat) b) t nil))))))