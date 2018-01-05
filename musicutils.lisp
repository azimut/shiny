(in-package :somecepl)

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


(setf *random-state* (make-random-state t))

;; https://stackoverflow.com/questions/6158990/generating-randoms-numbers-in-a-certain-range-for-common-lisp
(defun rrandom (start end)
  (+ start (random (+ 1 (- end start)))))

;; rotate list
;; https://programmingpraxis.com/2010/10/12/rotate-an-array/
(defun rotate (lst arg)
  (cond ((null lst) lst)
        ((= (mod arg (length lst)) 0) lst)
        (t (rotate (nconc (cdr lst) (list (car lst))) (- arg 1)))))

;; (define TWOPI (* 2.0 PI))
(defvar TWOPI 6.283185)

;; -------------------------------


(defun filter (fn 1st) 
  (let ((ace nil)) 
    (dolist (x 1st) 
      (let ((val (funcall fn x))) 
        (if val (push val ace)))) 
    (nreverse ace))) 

(defun range (max &key (min 0) (step 1))
   (loop for n from min below max by step
      collect n))

#|
;; select random pitch from pitch class
;; bounded by lower and upper (inclusive lower exclusive upper)
;;
;; arg 1: lower bound (inclusive)
;; arg 2: upper bound (exclusive)
;; arg 3: pitch class
;;
;; returns -1 if no valid pitch is possible
;;
(define pc:random
  (lambda (lower upper pc) 
    (if (null? pc) -1
        (let ((choices (filter (lambda (x) (pc:? x pc)) (range lower upper))))
          (if (null? choices) -1
              (random choices))))))
|#
(defun pcrrandom (lower upper pc)
  (if (not pc) -1
      (let ((choices (filter (lambda (x) (if (ispitch x pc) x)) (range upper :min lower))))
        (if (not choices) -1
            (random-list choices)))))

;; -------------------------------

#|
 returns a chord given a root and type
 see *pc:chord-syms* for currently available types

 e.g. (pc:chord 0 '^7)  => '(0 4 7 11)
 (define pc:chord
   (lambda (root type)
       (let ((chord (assoc type *pc:chord-syms*)))
          (if chord
              (let loop ((l (cdr chord))
                         (newlst '()))
                 (if (null? l)
                     (reverse newlst)
                     (loop (cdr l)
                           (cons (modulo (+ (car l) root)
                                         12)
                                 newlst))))
              (begin (print-notification "Chord type not found." type) #f)))))
|#
(defun chord (root type)
  (let ((chord (assoc type *chord-syms*)))
    (if chord
        (labels ((f (l newlst)
                   (if (not l) (reverse newlst)
                               (f (cdr l) (cons (mod (+ (car l) root) 12) newlst)))
                   ))
    (f (cdr chord) '() )
    ))))

;; -------------------------------

#|
 select pitch from pitch class relative to a given pitch

 1st: bass pitch
 2nd: pc relationship to bass pitch (max is abs 7)
 3rd: pitch class

 example:
 (pc:relative 64 -2 '(0 2 4 5 7 9 11)) => 60
 (pc:relative 69 3 '(0 2 4 5 7 9 11)) => 74

 (define pc:relative
   (lambda (pitch i pc)
     (set! i (real->integer (round i)))
     (if (= i 0) pitch
     (let ((inc (if (negative? i) - +)))
       (let loop ((p (inc pitch 1)) (cnt 0))
         (if (pc:? p pc) (set! cnt (inc cnt 1)))
         (if (= cnt i) p 
         (loop (inc p 1) cnt)))))))
|#
(defun relative (pitch i pc)
  (setf i (round i))
  (if (= i 0) pitch
  (let* ((inc (if (< i 0) '- '+)))
    (labels ((f (p cnt)
               (progn (if (ispitch p pc)
                          (setf cnt (funcall inc cnt 1)))
                      (if (= cnt i) p
                          (f (funcall inc p 1) cnt)))))
      (f (funcall inc pitch 1) 0)))))


;; -------------------------------

;; returns a chord following basic diatonic harmony rules
;; based on root (0 for C etc.) maj/min ('- or '^) and degree (i-vii)
;; ex: (pc:diatonic 0 '- 'i) => (0 3 7)
;; (define pc:diatonic
;;   (lambda (root maj-min degree)
;;     (if (number? degree)
;;         (set! degree (cdr (assoc degree '((0 . i) (1 . ii) (2 . ii) 
;;                                           (3 . iii) (4 . iii) (5 . iv) 
;;                                           (6 . iv) (7 . v) (8 . vi) (9 . vi) 
;;                                           (10 . vii) (11 . vii))))))
;;     (let ((val (assoc degree
;;                       (if (equal? '^ maj-min)
;;                           *pc:diatonic-major*
;;                           *pc:diatonic-minor*))))
;;       (pc:chord (modulo (+ root (cadr val)) 12) (cddr val)))))
(defun diatonic (root maj-min degree)
  (if (typep degree 'integer)
      (setf degree (cdr (assoc degree '((0 i)   (1 ii)   (2 ii)
                                        (3 iii) (4 iii)  (5 iv)
                                        (6 iv)  (7 v)    (8 vi)
                                        (9 vi)  (10 vii) (11 vii))))))
  (let ((val (assoc degree (if (eq '^ maj-min)
                               *diatonic-major*
                               *diatonic-minor*))))
    (chord (mod (+ root (cadr val)) 12) (cddr val)))
  )

;; -------------------------------

;; cosr
;; https://groups.google.com/forum/#!searchin/extemporelang/cosr|sort:date/extemporelang/9O-yhrLQ-ag/MGbpKigwyDgJ
;; - the first argument is the "centre" to oscillate around
;; - the second argument is the "amplitude" of the oscillation
;; - the third argument is the "period" of the oscillation. 
;; Ex: (cosr 5 3 1/2)

;; (macro (cosr args)
;;    (if (> (length args) 4)
;;        `(+ ,(caddr args) (* ,(cadddr args) (cos (* TWOPI (+ beat ,(cadr args)) ,(car (cddddr args))))))
;;        `(+ ,(cadr args) (* ,(caddr args) (cos (* TWOPI beat ,(cadddr args)))))))
(defun cosr (centre amplitude period)
  (+ centre
     (* amplitude
        (cos (* TWOPI (float (/ (incudine.util:sample->int (now)) 36000)) period)))))

;; -------------------------------

;; root 60

;; scheme<7099> (pc:scale 0 'aeolian)
;; => (0 2 3 5 7 8 10)

;; ;; A predicate for calculating if pitch is in pc
;; ;;
;; ;; arg 1: pitch to check against pc
;; ;; arg 2: pc to check pitch against
;; ;; retuns true or false
;; ;;
;; (define pc:?
;;    (lambda (pitch pc)
;;       (list? (member (modulo pitch 12) pc))))
(defun ispitch (pitch pc)
  (if (member (mod pitch 12) pc) t))

;; quantize pc
;; Always slelects a higher value before a lower value where distance is equal.
;;
;; arg 1: pitch to quantize to pc
;; arg 2: pc to quantize pitch against
;;
;; returns quantized pitch or #f if non available
;;
;; (define pc:quantize
;;    (lambda (pitch-in pc) 
;;       (let loop ((inc 0)
;;                  (pitch (round pitch-in)))
;;          (cond ((pc:? (+ pitch inc) pc) (+ pitch inc))
;;                ((pc:? (- pitch inc) pc) (- pitch inc))
;;                ((< inc 7) (loop (+ inc 1) pitch))
;;                (else (print-notification "no pc value to quantize to" pitch pc) 
;;                      #f)))))
(defun quant (pitch-in pc)
    (labels ((f (inc pitch)
               (cond ((ispitch  (+ pitch inc) pc) (+ pitch inc))
                     ((ispitch  (- pitch inc) pc) (- pitch inc))
                     ((< inc 7) (f (+ inc 1) pitch))
                     (t (print "Derp!") nil))))
      (f 0 (round pitch-in))
      ))

;; quantize pc
;; Always slelects a lower value before a higher value where distance is equal.
;;
;; arg 1: pitch to quantize to pc
;; arg 2: pc to quantize pitch against
;;
;; returns quntized pitch or #f if non available
;;
;; (define pc:quantize-low
;;    (lambda (pitch-in pc)
;;       (let loop ((inc 0)
;;                  (pitch (round pitch-in)))
;;          (cond ((pc:? (- pitch inc) pc) (- pitch inc))
;;                ((pc:? (+ pitch inc) pc) (+ pitch inc))
;;                ((< inc 7) (loop (+ inc 1) pitch))
;;                (else (print-notification "no pc value to quantize to" pitch pc)
;;                      #f)))))
(defun quant-low (pitch-in pc)
    (labels ((f (inc pitch)
               (cond ((ispitch  (- pitch inc) pc) (- pitch inc))
                     ((ispitch  (+ pitch inc) pc) (+ pitch inc))
                     ((< inc 7) (f (+ inc 1) pitch))
                     (t (print "Derp!") nil))))
      (f 0 (round pitch-in))
      ))

;; -------------------------------

;; (qcosr (0 2 3 5 7 8 10)
;;        60
;;        5
;;        3/2)

;; ;; cosr with pc
;; (define-macro (qcosr pc . args)
;;   `(pc:quantize (cosr ,@args) ,pc))

(defun qcosr (pc center amplitude period)
  (quant (cosr center amplitude period) pc))

;; -------------------------------

#|
scheme<7099> (pc:scale 0 'aeolian)
=> (0 2 3 5 7 8 10) 

;; returns a scale type based on a given root
(define pc:scale
   (lambda (root type)
      (if (assoc type *pc:scales*)
          (let loop ((l (cdr (assoc type *pc:scales*)))
                     (current (modulo root 12))
                     (newlst '()))
             (if (null? l)
                 (reverse (cons current newlst))
                 (loop (cdr l) (modulo (+ current (car l)) 12) (cons current newlst))))
          (begin (print-notification "Scale type not found." *pc:scales*) #f))))
|#
(defun scale (root type)
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

;; -------------------------------

;; pc:make-chord
;; creates a list of "number" pitches between "lower" and "upper"
;; bounds from the given "pc".  a division of the bounds
;; by the number of elements requested breaks down the selection into
;; equal ranges from which each pitch is selected.
;; make-chord attempts to select pitches of all degrees of the pc.
;; it is possible for elements of the returned chord to be -1 if no
;; possible pc is available for the given range.
;; non-deterministic (i.e. result can vary each time)
;;
;; arg1: lower bound (inclusive)
;; arg2: upper bound (exclusive)
;; arg3: number of pitches in chord
;; arg4: pitch class
;;
;; example: c7
;; (pc:make-chord 60 85 4 '(0 4 7 10)) => (60 70 76 79)
;;
#|
(define pc:make-chord
   (lambda (lower upper number pc)
      (let ((chord '()))
         (let loop ((l (round lower))
                    (u (round upper))
                    (n number)
                    (p pc))
            (if (< n 1)
                (map (lambda (x)
                       (real->integer x))
                     (cl:sort (cl:remove -1 chord) <)) ; lowest pitch to highest pitch remove -1s
                (let* ((range (- u l))
                       (gap   (round (/ range n)))
                       (pitch (pc:random l (+ l gap) p)))
                   (if (< pitch 0) ; if new pitch is -1 try from whole range
                       (set! chord (cons (pc:random lower upper p) chord))
                       (set! chord (cons pitch chord)))
                   (loop (+ l gap)
                         u
                         (- n 1)
                         (if (> (length p) 1)
                             (cl:remove (modulo (car chord) 12) p)
                             pc))))))))
|#
(defun make-chord (lower upper number pc)
  (let ((chord '()))
    (labels ((f (l u n p)
               (if (< n 1)
                   (mapcar (lambda (x) (round x)) (sort (remove -1 chord) '<))
                   (let* ((range (- u l))
                          (gap   (round (/ range n)))
                          (pitch (pcrrandom l (+ l gap) p)))
                     (if (< pitch 0)
                         (setf chord (cons (pcrrandom lower upper p) chord))
                         (setf chord (cons pitch chord)))
                     (f (+ l gap)
                        u
                        (- n 1)
                        (if (> (length p) 1)
                            (remove (mod (car chord) 12) p)
                            pc))))))
      (f (round lower) (round upper) number pc))))

;; -------------------------------

;; 
(defun random-list (mylist)
  (let* ((n (length mylist))
         (r (random n)))
    (nth r mylist)))

; From extempore - Scheme
;; (define note-name-to-midi-number
;;   (lambda (name)
;;     (let ((result (regex:matched name "([abcdefgABCDEFG])([#b])?(-?[0-9])")))
;;         (if (null? result)
;;             #f
;;             (let ((offset (+ 12 (* (string->number (cadddr result)) 12)))
;;                   (pc (case (modulo (- (modulo (char->integer (car (string->list (cadr result)))) 16) 3) 7)
;;                         ((0) 0) ((1) 2) ((2) 4) ((3) 5) ((4) 7) ((5) 9) ((6) 11))))
;;               (+ offset pc
;;                  (cond ((string=? (caddr result) "#") 1)
;;                        ((string=? (caddr result) "b") -1)
;;                        (else 0))))))))

;;scheme<7099> (regex:matched "F#1" "([abcdefgABCDEFG])([#b])?(-?[0-9])")
;;=> ("F#1" "F" "#" "1")  

(defun note-name-to-midi-number (name)
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

#|
(defn utter [object time duration velocity]
  (cond
    (number? object)     [{:pitch object :time time :duration duration :velocity velocity}]
    (sequential? object) (mapcat #(utter % time duration velocity) object)
    (map? object)        (utter (-> object vals sort) time duration velocity)
    (nil? object)        [{:time time :duration duration}]))
|#
;; (defun utter (object time duration velocity)
;;   (cond
;;     (numberp object) 
;;     ()))

#|
(defn phrase

  "Translates a sequence of durations and pitches into a melody.
  nil pitches signify rests, vectors represent clusters, and maps
  represent chords. Vector durations represent repeated notes.
  e.g. (phrase [1/2 1/2 3/2 3/2] [0 1 nil 4])
  (phrase [1 1 2] [4 3 [0 2]])
  (phrase [1 [1 2]] [4 3])
  (phrase (repeat 4) (map #(-> triad (root %))) [0 3 4 3])"

  ([durations pitches velocities]
   (let [wrap (fn [x] (if (sequential? x) x [x]))
         counts (map (comp count wrap) durations)
         normalised-pitches (mapcat repeat counts pitches)
         normalised-durations (mapcat wrap durations)
         times (reductions + 0 normalised-durations)]
     (mapcat utter normalised-pitches times normalised-durations velocities)))
  ([durations pitches]
   (->> (phrase durations pitches (repeat nil))
        (map #(dissoc % :velocity)))))
|#
;; (defun phrase (durations pitches velocities)
;;   (let ((wrap   (lambda (x) (if (typep x 'list) x '(x))))
;;         (counts (mapcar (comp 'count 'wrap) durations))
;;         (normalised-pitches (mapcat repeat counts pitches))
;;         (normalised-durations (mapcat wrap durations))
;;         (times (reductions + 0 normalised-durations)))
;;     (mapcat utter normalised-pitches times normalised-durations velocities)))q

;; ----------------------
;; functional-composition
;; https://github.com/ctford/functional-composition
;; ----------------------

(defun midihz (midi)
  (*
   8.1757989156
   (expt 2 (/ midi 12))))




;; ---------------------
;; Euclidian composition
;; code from: cl-patterns
;; ---------------------
(defun bjorklund (pulses &optional steps (offset 0))
  "Generate a list representing a Euclidean rhythm using the Bjorklund algorithm. PULSES is the number of \"hits\" in the sequence, STEPS is number of divisions of the sequence, and OFFSET is the number to rotate the sequence by. This function returns a list, where 1 represents a note and 0 represents a rest. If you want to use bjorklund in a pattern, you may be more interested in `pbjorklund' instead, which returns events with the correct duration and type.

Example: (bjorklund 3 7) ;=> (1 0 1 0 1 0 0)

See also: `pbjorklund'"
  (if (and (null steps) (typep pulses 'ratio))
      (bjorklund (numerator pulses) (denominator pulses))
      (progn
        (assert (> steps 0) (steps))
        (assert (>= steps pulses) (pulses))
        (labels ((from-array (arr)
                   (destructuring-bind (a b) (split arr)
                     (if (and (> (length b) 1) (> (length a) 0))
                         (from-array (lace a b))
                         (alexandria:flatten (append a b)))))
                 (split (arr)
                   (let ((index (position (car (last arr)) arr :test #'equal)))
                     (list (subseq arr 0 index)
                           (subseq arr index))))
                 (lace (a b)
                   (append (loop
                              :for x :in a
                              :for i :from 0
                              :collect (list x (nth i b)))
                           (when (<= (length a) (length b))
                             (subseq b (length a))))))
          (alexandria:rotate
           (from-array
            (append (make-list pulses :initial-element (list 1))
                    (make-list (- steps pulses) :initial-element (list 0))))
           offset)))))

;; ---------------------
;; Algorithmic composition
;; https://quod.lib.umich.edu/s/spobooks/bbv9810.0001.001/1:18/--algorithmic-composition-a-gentle-introduction-to-music?rgn=div1;view=fulltext
;; ---------------------

(defun 1-over-f (number)
  (do* ((counter 0 (incf counter))
        (blue (+ 1 (random 5)) (if (= counter 4)
                                   (+ 1 (random 5)) blue))
        (green (+ 1 (random 5)) (if (or
                                     (= counter 2)
                                     (= counter 4)
                                     (= counter 6))
                                    (+ 1 (random 5)) green))
        (red (+ 1 (random 5)) (+ 1 (random 5)))
        (total (+ blue green red) (+ blue green red))
        (the-list (cons total ()) (cons total the-list)))
       ((= counter (- number 1)) (reverse the-list))))
