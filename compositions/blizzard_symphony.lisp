(in-package :somecepl)

(defvar subkick   1)
(defvar clapsnare 2)
(defvar glitchy   3)
(defvar clickhat  4)
(defvar openhat   5)

(defparameter pats
  `((,subkick   . (1---))   ;; 4
    (,clapsnare . (----1--1---1-1--)) ;; 16
    (,glitchy   . (-1--------------)) ;; 16
    (,clickhat  . (-1----)) ;; 6
    (,openhat   . (--1-)))) ;; 4

(defparameter pats
  `((,subkick   (1---))   ;; 4
    (,clapsnare (----1--1---1-1--)) ;; 16
    (,glitchy   (-1--------------)) ;; 16
    (,clickhat  (-1----)) ;; 6
    (,openhat   (--1-)))) ;; 4

(setf (bpm *tempo*) 200)
(all-piano)

(defparameter *pats*
  '((1  (1---))
    (2  (--1-))
    (3  (-1----))
    (4  (-1--------------))
    (5  (----1--1---1-1--))))

(p (now) 60 80 1 0)

(defun live-perc-sequencer (metro live-patterns &optional beat start-beat)
  (if (not beat)
      (let ((cur-beat (funcall metro 'get-beat)))
        (live-perc-sequencer metro live-patterns cur-beat cur-beat))
      (progn
        (dolist (pattern live-patterns)
          (when (= 1 (nth (mod (- beat start-beat) (length pattern)) pattern))
            (p (now) 60 90 1 pattern)))
        (let ((next-beat (funcall metro 'get-beat 1)))))))

;; Simple version
(defun live-perc-sequencer (time patterns &optional (beat 0))
  (dolist (pattern patterns)
    (let ((inst  (car pattern))
          (beats (coerce (string (cadr pattern)) 'list)))
      (when (eql #\1 (nth (mod beat (length beats)) beats))
        (p time 60 90 1 inst))))
  (aat (+ time #[1 b]) #'live-perc-sequencer it patterns (1+ beat)))

(live-perc-sequencer (now) pats)

;; Simple, fix all integer bug
(defun live-perc-sequencer (time &optional (beat 0))
  (dolist (pattern pats)
    (let ((inst  (car pattern))
          (beats (coerce (write-to-string (caadr pattern)) 'list)))
      (when (eql #\1 (nth (mod beat (length beats)) beats))
        (p time 60 80 1 inst))))
  ;;(when (odds .1) (fluidsynth:program-change *synth* (1+ (random 5)) (random 100)))
  (aat (+ time #[.5 b]) #'live-perc-sequencer it (1+ beat)))

;; qcosr
(defun live-perc-sequencer (time &optional (beat 0))
  (dolist (pattern *pats*)
    (let* ((inst      (car pattern))
           (raw-beats (caadr pattern))
           (beats (if (integerp raw-beats)
                      (write-to-string raw-beats)
                      (symbol-name raw-beats)))
           (beats     (coerce beats 'list)))
      (when (eql #\1 (nth (mod beat (length beats)) beats))
        (p time (qcosr *phrygian* 65 10 1/2) 40 (pick 1 3/4) inst))))
  ;;(when (odds .1)
  ;;  (fluidsynth:program-change *synth* (1+ (random 5)) (random 100)))
  (aat (+ time #[1 b]) #'live-perc-sequencer it (1+ beat)))

(live-perc-sequencer (now))

(defparameter *pats*
  '((3  (1-1-))
    (4  (----1---))))

(defparameter *pats*
  '((3  (01011101010111010))
    (4  (01101100010010011))
    (5  (00010010010010011))
    (1  (00110110110010110))))

(all-piano)
(off-with-the-notes)
(flush-pending)

(fluidsynth:program-change *synth* 1 33)
(fluidsynth:program-change *synth* 2 13)
(fluidsynth:program-change *synth* 3 23)
(fluidsynth:program-change *synth* 4 40)

(fluidsynth:program-change *synth* 3 53)
(fluidsynth:program-change *synth* 4 9)

(fluidsynth:program-change *synth* 5 41)

;; --------------------------

(defun pcc (time chord)
  (mapcar (lambda (x) (p time x 70 1 1))
          (apply #'chord chord)))

(defun pcc (time chord)
  (dolist (x (apply #'chord chord))
    (p time x 70 2 1)))

(defun pc (time chord)
  (dolist (x chord)
    (p time x 70 2 1)))

(defvar chord-progression '((:b3 :m6) (:b3 :7) (:d4 :M7) (:b3 :7)))

(pc  (now) (chord :b3 :m6))
(pcc (now) (pickl chord-progression))

(defun f (time chords)
  (pcc time (pickl chords))
  (aat (+ time #[ (pick 1 2 3/2) b]) #'f it chords))

(defun f (time chords)
  (pcc time (car chords))
  (aat (+ time #[ (pick 1 2 3/2) b]) #'f it (rotate chords 1)))

(defun f (time chords)
  (pcc time (car chords))
  (aat (+ time #[ (pick 4 3/4) b]) #'f it (rotate chords 1)))

(f (now) chord-progression)

;;;
;; mad_beats.clj
;;;

;; funky organ - 33 - 17

(fluidsynth:program-change *synth* 1 33)
(try-sounds (now) 1 33)

(defparameter chord-progression
  '((:A#2 :9sus4) (:A#2 :9sus4) (:B2 :M7) (:C#3 :7)
    (:D#3 :m7)    (:D#3 :m9)    (:G#2 :7) (:G#2 :7)))

;;;
;; screen_cast.clj
;;;

(defparameter chord-progression
  '((:G#2 :m7) (:G#2 :m7) (:A#2 :m7) (:A#2 :7)
    (:D#3 :m7) (:D#3 :m9) (:D#3 :7)  (:D#3 :9)))


;;;
;; endlessly_rising.clj
;;;
;; Here the need for a way to associate a note with a rhythm is bigger

(defparameter *notes* '(:f4 :d4 :f4 :a4 :d5 :c5 :b4
     :c5 :f#4 :e4 :f#4 :g4 :d4 :g4 :a4 :a#4
     :a#4 :a4 :g4 :f4 :g4 :f4 :e4
     :d4 :d#4 :e4 :e4 :f4 :e4 :d4
     :c#4 :d4 :c#4 :d4 :e4 :d4 :c4 :b3 :a3 :b3 :c4 :d4 :b3 :c4 :d4
     :e4 :d5 :c5 :b4 :c5 :a4 :f#4 :b4 :a4
     :g#4 :a4 :b4 :c5 :d#5 :e4 :_
     :_ :e4 :g4 :f#4 :e4 :d#4 :e4 :f#4 :g4 :a4 :c5 :b4 :a4))

(defparameter *rhythms* '(1/16 1/16 1/16 1/16 1/2 1/8 1/8
     5/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16 3/8
     1/8 1/8 1/8 5/16 1/16 1/16 1/16
     1/8 1/8 1/4 1/8 1/8 1/8 1/8
     1/8 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16
     1/8 1/4 1/16 1/16 1/8 1/8 1/8 1/16 1/16
     1/8 1/16 1/16 1/8 1/8 1/4 1/4
     1/8 3/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16))



(setf (bpm *tempo*) 10)

(setf (fluidsynth:setting *fluid-settings* "synth.gain") .1)
(fluidsynth:program-change *synth* 1 0)

(defun f (time notes rhythms)
  (let ((note   (car notes))
        (rhythm (car rhythms)))
    (when (not (eql note :_))
      (p time (+ 1 (note note)) 60 rhythm 1))
    (aat (+ time #[rhythm b]) #'f it (rotate notes -1) (rotate rhythms -1))))

(f (now) *notes* *rhythms*)
