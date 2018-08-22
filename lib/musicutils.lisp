(in-package :somecepl)

;; Nice keys2 - Program names
;; Nice-Keys-Ultimate-V2.3.sf2
;; $ echo "inst 1" | fluidsynth /path/to/FluidR3_GM.sf2 |
;; | tr '[[:upper:]]' '[[:lower:]]'
;; | tr -dc '[[:alnum:]]\n ' 
;; | tr ' ' '-'
;; | sed -e 's/^\(..\)-\(.*\)/\*nk-\2\* \1/g' 
;; | sed 's/^/(defvar /g'
;; | sed 's/$/)/g'

(defun cumsum (l)
  "> (cumsum '(20 30 40 50 60))
  (20 50 90 140 200)"  
  (loop :for x :in l :with z = 0
     :collect (incf z x)))

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

(defun my-rotate (length shift)
  "Return a list of given LENGTH, rotated by SHIFT."
  (nconc
   (loop for i from (1+ shift) to (- length shift -2) collect i)
   (loop for i from 1 to shift collect i)))

(defun rotate-list (list shift)
  "Rotate the given LIST by the specified SHIFT."
  (let ((len (length list)))
    (setq shift (mod shift len)) ; handle circular shifts
    (append (nthcdr (- len shift) list)
            (butlast list shift))))
 
(defun random-list (mylist)
  (let* ((n (length mylist))
         (r (random n)))
    (nth r mylist)))

;;  cl-losh/losh.lisp
(defun random-elt (seq &optional (generator #'random))
  "Return a random element of `seq`, and whether one was available.

  This will NOT be efficient for lists.

  Examples:

    (random-elt #(1 2 3))
    => 1
       T

    (random-elt nil)
    => nil
       nil

  "
  (let ((length (length seq)))
    (if (zerop length)
      (values nil nil)
      (values (elt seq (funcall generator length)) t))))

;; ----------------------
;; functional-composition
;; https://github.com/ctford/functional-composition
;; ----------------------
(defun midihz (midi)
  (declare (number midi))
  (* 8.1757989156
     (expt 2 (/ midi 12))))

;; ---------------------
;; https://en.wikipedia.org/wiki/Musical_note
;; where 69 is the number of semitones between
;; C|-1 (note 0) and A|4
;; --------------------
(defun hzmidi (hz)
  (round (+ 69 (* 12 (log (/ hz 440) 2)))))

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

(defun bjorklund-s (pulses steps)
  (format nil "~{~A~}" (bjorklund pulses steps)))

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

(defun brownian-motion (start number-of-notes)
  (do* ((counter 0 (incf counter))
        (the-array (make-array 7 :initial-contents
                               '(-3 -2 -1 0 1 2 3)))
        (note start (+ note (aref the-array (random 7))))
        (the-list (cons start ()) (cons note the-list)))
       ((= counter (- number-of-notes 1)) (reverse the-list))))

;; http://stevelosh.com/blog/2016/08/playing-with-syntax/
(defmacro mulf (place factor)
  `(setf ,place (* ,place ,factor)))

(defmacro divf (place divisor)
  `(setf ,place (/ ,place ,divisor)))

;; Set if not defined, needs a better name (?
(defmacro setp (var values)
  `(when (null ,var)
     (setf ,var ,values)))

;;--------------------------------------------------

;; from Fluxus Fluxa
(defun zmod (number divisor &optional (rest 0))
  "zero module - play every N, where number is samples
  > (zmod (get-internal-real-time) 4)"
  (= (mod number divisor) rest))

(defun zmodt (divisor &optional (rest 0))
  (zmod (get-universal-time) divisor rest))

;;--------------------------------------------------

(defun b2d (bin-integer)
  (reduce (lambda (x y) (+ (* 2 x) y))
          (loop :for c :in
             (mapcar #'parse-integer
                     (mapcar #'string
                             (coerce (string bin-integer) 'list))) :collect c)))

;;--------------------------------------------------

(defun merge-beats (beats notes &optional (rot 0 rot-def))
  "takes a list of rhythms on a 1 and 0 form
   and a list of notes, it will place the notes on the 1 slots
   leaving notes of if necessary"
  (when rot-def
      (setf notes (rotate notes rot)))
  (loop :for beat :in beats
     :collect (if (> beat 0)
                  (pop notes)
                  0)))

;;--------------------------------------------------
;; Visual utils
;;--------------------------------------------------
(defun make-trigger (&optional (max-life 1))
  "thing/closure that has 1 live until is regen
   unless is bulletproof in which case is alive forever"
  (let* ((mf max-life)
         (alive mf)
         (bulletproof nil))
    (lambda (sym)
      (cond ((eq sym 'bulletproof) (setf bulletproof t
                                         alive mf))
            ((eq sym 'bulletproof-p) bulletproof)
            ((eq sym 'curse) (setf bulletproof nil))
            ((eq sym 'alive) (setf alive mf
                                   bulletproof nil))
            ((eq sym 'alive-p) alive)
            ((eq sym 'bleed) (progn
                               (when (> alive 0)
                                 (decf alive .05))
                               alive))
            ((eq sym 'shoot) (if bulletproof
                                 mf
                                 (if (> alive 0)
                                     (progn (setf alive 0) mf)
                                     0)))))))

;; (defmacro with-trigger ((trigger t-trigger) &body body)
;;   "makes a mortal trigger-once call"
;;   (let ((it (intern "IT")))
;;     `(let ((,it ,t-trigger))
;;        (callback ,t-trigger #'funcall ,trigger 'alive)
;;        ,@body)))

(defmacro with-trigger ((trigger) &body body)
  "makes a mortal trigger-once call"
  `(progn
     (funcall ,trigger 'alive)
     ,@body))

(defmacro with-trigger-expire ((trigger dur) &body body)
  "makes a inmortal trigger-once call"
  `(progn
     (funcall ,trigger 'bulletproof)
     (at (+ (now) (* ,dur 44100d0)) #'(lambda () (funcall ,trigger 'curse)))
     ,@body))

;; <3
;; Might be I can generalize the pick and pickl of CM too.
(defun pick-random-list (list &optional (end 0))
  "pick a random number of elements from a list, up to max elements,
   useful if you have a 4note chord that you want to avoid"
  (let* ((l (max end (length list)))
         (n (- l (random l))))
    (loop :for x :in list :collect x :repeat n)))

;; like overtone's choose-n
(defun pickn (n l)
  "get n random elements from list"
  (subseq (cm:shuffle l)
          0 (min (length l) n)))

;; Apparently same idea of nudruz.lisp
(defun nths (lnth l)
  "get nth elements from lnth list"
  (remove nil (loop :for i :in lnth :collect (nth i l))))


(defun nth-set (n new-value l)
  "Change the value at n on l for new-value
   > (nth-set 1 20 '(60 70 80 90 100))
   (60 20 80 90 100)"
  (when (not (listp l))
    (setf l (list l)))
  (loop :for i :upfrom 0
     :for e :in l
     :collect
     (if (= n i)
         new-value
         e)))

(defun nth-inc (n inc-by l)
  "Increment by inc-by the value at n on l
   > (nth-inc 1 -5 '(30 40 50 60 70 80))
   (30 35 50 60 70 80)"
  (when (not (listp l))
    (setf l (list l)))
  (loop :for i :upfrom 0
     :for e :in l
     :collect
     (if (= n i)
         (+ e inc-by)
         e)))
