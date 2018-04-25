(in-package :somecepl)

;; Run it on the repl
(set-rt-block-size 64)
;; (rt-start)
(ql:quickload :incudine-fluidsynth)

(defvar *fluid-settings* (fluidsynth:new-settings
                          `(("synth.polyphony" 128)
                            ("synth.midi-channels" 32)
                            ("synth.sample-rate" ,*sample-rate*)
                            ("audio.sample-format" "float"))))

(defvar *synth* (fluidsynth:new *fluid-settings*))

(defun fp (channel program)
  (fluidsynth:program-change *synth* channel program))

(defun off-with-the-notes ()
  "turn off all the notes"
  (loop :for c :from 0 :upto 50 :do
     (mapcar (lambda (x) (fluidsynth:noteoff *synth* c x))
             (range 120))))

(defun try-sounds (time chan sf &optional (beats 8))
  "iterate over soundfonts to test sounds"
  (print sf)
  (fluidsynth:program-change *synth* chan (mod sf 100))
  (aat (+ time #[beats b]) #'try-sounds it chan (1+ sf) beats))

(defun all-piano (&optional (sf 0))
  "set all the midi channels to ..."
  (dotimes (i 32)
    (fluidsynth:program-change *synth* i sf) ))

(dsp! fluid-test ((synth fluidsynth:synth))
  (with ((len   (block-size))
         (left  (make-f32-array len))
         (right (make-f32-array len)))
    (fluidsynth:write-float synth len left 0 1 right 0 1)
    (foreach-frame
      (out (f32-ref left current-frame)
           (f32-ref right current-frame)))))

;;(fluidsynth:sfload *synth* "/usr/share/sounds/sf2/FluidR3_GM.sf2" 1)
(fluidsynth:sfload *synth* "/home/sendai/samples/Touhou.sf2" 1)
(fluid-test *synth*)

(defun play-midi-note (time pitch velocity dur c)
  (when (and pitch (> pitch 2))
    (at time #'fluidsynth:noteon *synth* c pitch velocity)
    (at (+ time #[dur b]) #'fluidsynth:noteoff *synth* c pitch)))

(defgeneric p (time pitch velocity duration channel)
  (:method (time (pitch list) velocity duration channel)
    "Play chord of notes"
    (mapcar (lambda (x) (p time x velocity duration channel))
            pitch))
  (:method (time (pitch list) velocity duration (channel list))
    "Play chord of notes, on provided channels"
    (mapcar (lambda (x y) (p time x velocity duration y))
            pitch
            channel))
  (:method (time (pitch integer) velocity (duration number) channel)
    "Play given numerical pitch"
    (at time #'fluidsynth:noteon *synth* channel pitch velocity)
    (at (+ time #[duration b]) #'fluidsynth:noteoff *synth* channel pitch))
  (:method (time (pitch integer) velocity (duration symbol) channel)
    "Play given numerial pitch, at CM rythm"
    (let ((d (cm:rhythm duration)))
      (at time #'fluidsynth:noteon *synth* channel pitch velocity)
      (at (+ time #[d b]) #'fluidsynth:noteoff *synth* channel pitch)))
  (:method (time (pitch symbol) velocity (duration symbol) channel)
    "Play given note, at CM rhythm"
    (unless (eql :_ pitch)
      (let ((n (note pitch))
            (d (cm:rhythm duration)))
        (at time #'fluidsynth:noteon
            *synth* channel n velocity)
        (at (+ time #[d b]) #'fluidsynth:noteoff
            *synth* channel n))))
  (:method (time (pitch symbol) velocity duration channel)
    "Play given note"
    (unless (eql :_ pitch)
      (let ((n (note pitch)))
        (at time #'fluidsynth:noteon
            *synth* channel n velocity)
        (at (+ time #[duration b]) #'fluidsynth:noteoff
            *synth* channel n)))))

(defun pc (time notes velocity duration channel)
  (mapcar (lambda (x) (p time x velocity duration channel))
          notes))

(defgeneric pa (time notes offset velocity &key channel dur)
  (:method (time notes (offset number) (velocity integer)
            &key (channel 1) (dur offset))
    "Play arpeggio, with a constant offset and velocity"
    (let* ((lnotes  (length notes))
           (offsets (loop :for i :from 0 :by offset :collect i :repeat lnotes)))
      (mapcar (lambda (x y) (p (+ time #[y b]) x velocity dur channel))
              notes
              offsets)))
  (:method (time notes (offset number) (velocity list)
            &key (channel 1) (dur offset))
    "Play arpeggio, with a constant offset and provided velocities"
    (let* ((lnotes  (length notes))
           (offsets (loop :for i :from 0 :by offset :collect i :repeat lnotes)))
      (mapcar (lambda (x y z) (p (+ time #[y b]) x z dur channel))
              notes
              offsets
              velocity)))
  (:method (time notes (offset list) (velocity integer) &key (channel 1) (dur 1))
    "Play arpeggio, with provided offsets and constant velocity"
    (mapcar (lambda (x y) (p (+ time #[y b]) x velocity dur channel))
            notes
            offset))
  (:method (time notes (offset list) (velocity list) &key (channel 1) (dur 1))
    "Play arpeggio, with provided offsets and velocities"
    (mapcar (lambda (x y z) (p (+ time #[y b]) x z dur channel))
            notes
            offset
            velocity)))

(defun loop-rhythm (time notes rhythms velocity
                    &optional (hownotes 'cdr) (howrhythms 'cdr))
  (let ((note   (if (eql hownotes 'next) (cm:next notes) (car notes)))
        (rhythm (if (eql howrhythms 'next) (cm:next rhythms) (car rhythms))))
    (p time note velocity rhythm 0)
    (aat (+ time #[rhythm b]) #'loop-rhythm
         it 
        (case hownotes
           (cdr     (cdr notes))
           (rotate  (rotate notes  1))
           (rrotate (rotate notes -1))
           (next    notes))
         (case howrhythms
           (cdr     (cdr rhythms))
           (rotate  (rotate rhythms  1))
           (rrotate (rotate rhythms -1))
           (next    rhythms))
         velocity
         hownotes howrhythms)))

#|
(fluidsynth:program-change *synth* 0 0)
(fluidsynth:program-change *synth* 1 52)
(fluidsynth:program-change *synth* 2 53)

(fluidsynth:set-reverb *synth* 0.7d0 0.9d0 0.5d0 0.9d0)
  
(setf (fluidsynth:setting *fluid-settings* "synth.gain") 1.2)
(setf (fluidsynth:setting *fluid-settings* "synth.gain") .9)
(setf (fluidsynth:setting *fluid-settings* "synth.polyphony") 128)
(setf (fluidsynth:setting *fluid-settings* "synth.midi-channels") 24)
|#
