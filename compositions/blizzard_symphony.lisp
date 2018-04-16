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

(setf (bpm *tempo*) 200)
(all-piano *synth*)

(defparameter pats
  '((1  (1---))
    (2  (--1-))
    (3  (-1----))
    (4  (-1--------------))
    (5  (----1--1---1-1--))))

(p (now) 60 100 1 0)

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
;; qcosr
(defun live-perc-sequencer (time &optional (beat 0))
  (dolist (pattern pats)
    (let ((inst  (car pattern))
          (beats (coerce (write-to-string (caadr pattern)) 'list)))
      (when (eql #\1 (nth (mod beat (length beats)) beats))
        (p time (qcosr *phrygian* 65 10 1/2) 80 (pick 1 1/2 1) inst))))
  ;;(when (odds .1) (fluidsynth:program-change *synth* (1+ (random 5)) (random 100)))
  (aat (+ time #[.5 b]) #'live-perc-sequencer it (1+ beat)))

(live-perc-sequencer (now))

(off-with-the-notes *synth*)
(flush-pending)

(fluidsynth:program-change *synth* 1 0)
(fluidsynth:program-change *synth* 2 13)
(fluidsynth:program-change *synth* 3 23)
(fluidsynth:program-change *synth* 4 40)
(fluidsynth:program-change *synth* 5 41)
