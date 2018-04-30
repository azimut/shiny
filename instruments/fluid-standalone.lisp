(in-package :somecepl)

(defvar *settings* (fluidsynth:new-settings
                    `(("synth.polyphony" 128)
                      ("synth.midi-channels" 32)
                      ("audio.driver" "alsa")
                      ("synth.sample-rate" ,*sample-rate*)
                      ("audio.sample-format" "float"))))

(defvar *synth* (fluidsynth:new-synth *settings*))

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
  (assert (and (<= 0d0 roomsize 1d0)
               (<= 0d0 damp 1d0)
               (<= 0d0 width 100d0)
               (<= 0d0 level 1d0)))
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

(defun all-piano (&optional (sf 0))
  "set all the midi channels to ..."
  (dotimes (i 32)
    (fluidsynth:program-change *synth* i sf) ))

(fluidsynth:sfload *synth* "/usr/share/sounds/sf2/FluidR3_GM.sf2" 1)

(defvar *audio-driver* (fluidsynth:new-audio-driver *settings* *synth*))
(defvar *player* (fluidsynth:new-player *synth*))

;;(fluidsynth:sfload *synth* "/usr/share/sounds/sf2/FluidR3_GS.sf2" 1)
;;(fluidsynth:sfload *synth* "/home/sendai/samples/Touhou.sf2" 1)
;;(fluidsynth:delete *synth*)

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

(defgeneric p (time pitch velocity duration channel)
  (:method ((time double-float) (pitch list) velocity duration channel)
    "Play chord of notes"
    (mapcar (lambda (x) (p time x velocity duration channel))
            pitch))
  (:method ((time double-float) (pitch list) velocity duration (channel list))
    "Play chord of notes, on provided channels"
    (mapcar (lambda (x y) (p time x velocity duration y))
            pitch
            channel))
  (:method ((time double-float) (pitch integer) (velocity integer) (duration number) (channel integer))
    "Play given numerical pitch"
    (callback time #'fluidsynth:noteon *synth* channel pitch velocity)
    (callback (+ time duration) #'fluidsynth:noteoff *synth* channel pitch))
  (:method ((time double-float) (pitch integer) (velocity integer) (duration symbol) (channel integer))
    "Play given numerial pitch, at CM rythm"
    (let ((d (cm:rhythm duration)))
      (callback time #'fluidsynth:noteon *synth* channel pitch velocity)
      (callback (+ time d) #'fluidsynth:noteoff *synth* channel pitch)))
  (:method ((time double-float) (pitch symbol) (velocity integer) (duration symbol) (channel integer))
    "Play given note, at CM rhythm"
    (unless (eql :_ pitch)
      (let ((n (note pitch))
            (d (cm:rhythm duration)))
        (callback time #'fluidsynth:noteon
            *synth* channel n velocity)
        (callback (+ time d) #'fluidsynth:noteoff
                  *synth* channel n))))
  (:method ((time double-float) (pitch symbol) (velocity integer) (duration number) (channel integer))
    "Play given note"
    (unless (eql :_ pitch)
      (let ((n (note pitch)))
        (callback time #'fluidsynth:noteon
                  *synth* channel n velocity)
        (callback (+ time duration) #'fluidsynth:noteoff
                  *synth* channel n)))))

(defgeneric pa (time notes offset velocity channel &key dur)
  (:method ((time double-float) (notes list) (offset number) (velocity integer) (channel integer)
            &key (dur offset))
    "Play arpeggio, with a constant offset and velocity"
    (let* ((lnotes  (length notes))
           (offsets (loop :for i :from 0 :by offset :collect i :repeat lnotes)))
      (mapcar (lambda (x y) (p (+ time y) x velocity dur channel))
              notes
              offsets)))
  (:method ((time double-float) (notes list) (offset number) (velocity integer) (channel list)
            &key (dur offset))
    "Play arpeggio, with a constant offset and velocity"
    (let* ((lnotes  (length notes))
           (offsets (loop :for i :from 0 :by offset :collect i :repeat lnotes)))
      (mapcar (lambda (x y z) (p (+ time y) x velocity dur z))
              notes
              offsets
              channel)))
  (:method ((time double-float) (notes list) (offset number) (velocity list) (channel integer)
            &key (dur offset))
    "Play arpeggio, with a constant offset and provided velocities"
    (let* ((lnotes  (length notes))
           (offsets (loop :for i :from 0 :by offset :collect i :repeat lnotes)))
      (mapcar (lambda (x y z) (p (+ time y) x z dur channel))
              notes
              offsets
              velocity)))
  (:method ((time double-float) (notes list) (offset number) (velocity list) (channel list)
            &key (dur offset))
    "Play arpeggio, with a constant offset and provided velocities"
    (let* ((lnotes  (length notes))
           (offsets (loop :for i :from 0 :by offset :collect i :repeat lnotes)))
      (mapcar (lambda (x y z a) (p (+ time y) x z dur a))
              notes
              offsets
              velocity
              channel)))
  (:method ((time double-float) (notes list) (offset list) (velocity integer) (channel integer)
            &key (dur 1))
    "Play arpeggio, with provided offsets and constant velocity"
    (mapcar (lambda (x y) (p (+ time y) x velocity dur channel))
            notes
            offset))
  (:method ((time double-float) (notes list) (offset list) (velocity integer) (channel list)
            &key (dur 1))
    "Play arpeggio, with provided offsets and constant velocity"
    (mapcar (lambda (x y z) (p (+ time y) x velocity dur z))
            notes
            offset
            channel))
  (:method ((time double-float) (notes list) (offset list) (velocity list) (channel integer)
            &key (dur 1))
    "Play arpeggio, with provided offsets and velocities"
    (mapcar (lambda (x y z) (p (+ time y) x z dur channel))
            notes
            offset
            velocity))
  (:method ((time double-float) (notes list) (offset list) (velocity list) (channel list)
            &key (dur 1))
    "Play arpeggio, with provided offsets and velocities"
    (mapcar (lambda (x y z a) (p (+ time y) x z dur a))
            notes
            offset
            velocity
            channel)))
