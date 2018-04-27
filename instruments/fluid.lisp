(in-package :somecepl)

;; Run it on the repl
;; (rt-start)

;; OPTIONAL
;;(set-rt-block-size 64)

(ql:quickload :incudine-fluidsynth)

(defvar *fluid-settings* (fluidsynth:new-settings
                          `(("synth.polyphony" 128)
                            ("synth.midi-channels" 32)
                            ("synth.sample-rate" ,*sample-rate*)
                            ("audio.sample-format" "float"))))

(defvar *synth* (fluidsynth:new *fluid-settings*))

(defun freset ()
  "reset all the things! (programs and synths)"
  (fluidsynth:system-reset *synth*))

;; ---------------------------------------------------
;; http://midi.teragonaudio.com/tech/midispec/pressure.htm
(defun fpress (channel pressure)
  "Set the MIDI channel pressure controllre value.
   a.k.a wibbly-woobly
   0>=pressure>=127"
  (declare (type integer channel pressure))
  (fluidsynth:channel-pressure *synth* channel pressure))

;; http://midi.teragonaudio.com/tech/midispec.htm
(defun fsens (channel sens)
  "Set MIDI pitch wheel (aka sensitivity) on a MIDI channel.
   0>=sens>=72"
  (fluidsynth:pitch-wheel-sens *synth* channel sens))

(defun fpitch (channel bend)
  "Set the MIDI pitch bend controller value on a MIDI channel.
   0>=bend>=16383"
  (fluidsynth:pitch-bend *synth* channel bend))

;; ---------------------------------------------------
(defun fchorus (&key (nr 3 nr-set)
                  (level 2.0d0 level-set)
                  (speed 0.3d0 speed-set)
                  (depth 8.0d0 depth-set)
                  (mode 0 mode-set))
  (assert (and (<= 0 nr 99) (<= 0d0 level 10d0)
               (<= 0.29d0 speed 5.0d0) (<= 0.0d0 depth 21.0d0)
               (or (= mode 0) (= mode 1))))
  (when (or nr-set level-set speed-set
            depth-set mode-set)
    (fluidsynth:set-chorus *synth*
                           nr level speed
                           depth mode)))

(defun freverb (&key (roomsize 0.2d0 roomsize-set)
                  (damp 0.0d0 damp-set)
                  (width 0.5d0 width-set)
                  (level 0.9d0 level-set))
  (when (or roomsize-set damp-set width-set level-set)
    (fluidsynth:set-reverb *synth*
                           roomsize damp
                           width level)))
;; ---------------------------------------------------

(defun fp (channel program &optional (bank 0 bank-set))
  "short-hand to set the fluidsynth channel to program"
  (declare (type integer channel program bank))
  (when bank-set
    (fluidsynth:bank-select *synth* channel bank))
  (fluidsynth:program-change *synth* channel program))

(defun fg (gain)
  "short-hand to set the gain on fluidsynth"
  (declare (type float gain))
  (setf (fluidsynth:setting *fluid-settings* "synth.gain")
        gain))

(defun off-with-the-notes (&optional (max-channels 50))
  "turn off all the notes"
  (loop :for c :below max-channels
     :do (fluidsynth:all-notes-off *synth* c)))

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

(fluidsynth:sfload *synth* "/usr/share/sounds/sf2/FluidR3_GM.sf2" 1)
;;(fluidsynth:sfload *synth* "/usr/share/sounds/sf2/FluidR3_GS.sf2" 1)
;;(fluidsynth:sfload *synth* "/home/sendai/samples/Touhou.sf2" 1)
(fluid-test *synth*)
;;(fluidsynth:delete *synth*)

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

(defgeneric pa (time notes offset velocity channel &key dur)
  (:method (time notes (offset number) (velocity integer) (channel integer)
            &key (dur offset))
    "Play arpeggio, with a constant offset and velocity"
    (let* ((lnotes  (length notes))
           (offsets (loop :for i :from 0 :by offset :collect i :repeat lnotes)))
      (mapcar (lambda (x y) (p (+ time #[y b]) x velocity dur channel))
              notes
              offsets)))
  (:method (time notes (offset number) (velocity integer) (channel list)
            &key (dur offset))
    "Play arpeggio, with a constant offset and velocity"
    (let* ((lnotes  (length notes))
           (offsets (loop :for i :from 0 :by offset :collect i :repeat lnotes)))
      (mapcar (lambda (x y z) (p (+ time #[y b]) x velocity dur z))
              notes
              offsets
              channel)))
  (:method (time notes (offset number) (velocity list) (channel integer)
            &key (dur offset))
    "Play arpeggio, with a constant offset and provided velocities"
    (let* ((lnotes  (length notes))
           (offsets (loop :for i :from 0 :by offset :collect i :repeat lnotes)))
      (mapcar (lambda (x y z) (p (+ time #[y b]) x z dur channel))
              notes
              offsets
              velocity)))
  (:method (time notes (offset number) (velocity list) (channel list)
            &key (dur offset))
    "Play arpeggio, with a constant offset and provided velocities"
    (let* ((lnotes  (length notes))
           (offsets (loop :for i :from 0 :by offset :collect i :repeat lnotes)))
      (mapcar (lambda (x y z a) (p (+ time #[y b]) x z dur a))
              notes
              offsets
              velocity
              channel)))
  (:method (time notes (offset list) (velocity integer) (channel integer)
            &key (dur 1))
    "Play arpeggio, with provided offsets and constant velocity"
    (mapcar (lambda (x y) (p (+ time #[y b]) x velocity dur channel))
            notes
            offset))
  (:method (time notes (offset list) (velocity integer) (channel list)
            &key (dur 1))
    "Play arpeggio, with provided offsets and constant velocity"
    (mapcar (lambda (x y z) (p (+ time #[y b]) x velocity dur z))
            notes
            offset
            channel))
  (:method (time notes (offset list) (velocity list) (channel integer)
            &key (dur 1))
    "Play arpeggio, with provided offsets and velocities"
    (mapcar (lambda (x y z) (p (+ time #[y b]) x z dur channel))
            notes
            offset
            velocity))
  (:method (time notes (offset list) (velocity list) (channel list)
            &key (dur 1))
    "Play arpeggio, with provided offsets and velocities"
    (mapcar (lambda (x y z a) (p (+ time #[y b]) x z dur a))
            notes
            offset
            velocity
            channel)))

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
