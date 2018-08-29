(in-package :somecepl)

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

(defun fchorus-toggle (toggle)
  (case toggle
    (0 (fluidsynth:set-chorus-on *synth* 0))
    (1 (fluidsynth:set-chorus-on *synth* 1))
    (t (format nil "set 0 or 1 dummy!"))))

(defun fchorus (&key
                  (nr        3 nr-set)
                  (level 2.0d0 level-set)
                  (speed 0.3d0 speed-set)
                  (depth 8.0d0 depth-set)
                  (mode      0 mode-set))
  (assert (and (<= 0 nr 99)
               (<= 0d0 level 10d0)
               (<= 0.29d0 speed 5.0d0)
               (<= 0.0d0 depth 21.0d0)
               (or (= mode 0) (= mode 1))))
  (when (or nr-set level-set speed-set
            depth-set mode-set)
    (fluidsynth:set-chorus *synth*
                           nr level speed
                           depth mode)))

;;--------------------------------------------------

(defun freverb-toggle (toggle)
  (case toggle
    (0 (fluidsynth:set-reverb-on *synth* 0))
    (1 (fluidsynth:set-reverb-on *synth* 1))
    (t (format nil "set 1 to enable or 0 disable"))))

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

;; fluid_synth.c
(defun freverb-preset (preset)
  (case preset
    (1 (freverb :roomsize 0.2d0 :damp 0.0d0 :width 0.5d0 :level 0.9d0))
    (2 (freverb :roomsize 0.4d0 :damp 0.2d0 :width 0.5d0 :level 0.8d0))
    (3 (freverb :roomsize 0.6d0 :damp 0.4d0 :width 0.5d0 :level 0.7d0))
    (4 (freverb :roomsize 0.8d0 :damp 0.7d0 :width 0.5d0 :level 0.6d0))
    (5 (freverb :roomsize 0.8d0 :damp 1.0d0 :width 0.5d0 :level 0.5d0))
    (6 (freverb :roomsize 0.6d0 :damp 0.1d0 :width 0.9d0 :level 1d0))
    (t (format nil "choose a value between 1-6 dummy"))))

;;--------------------------------------------------

(defun fp (channel program &optional (bank 0 bank-set))
  "short-hand to set the fluidsynth channel to program"
  (declare (type integer channel program bank))
  (when bank-set
    (fluidsynth:bank-select *synth* channel bank))
  (fluidsynth:program-change *synth* channel program))

(defun fg (gain)
  "short-hand to set the gain on fluidsynth"
  (declare (type float gain))
  (setf (fluidsynth:setting *settings* "synth.gain")
        gain))

(defun off-with-the-notes (&optional (max-channels 50))
  "turn off all the notes"
  (loop :for c :below max-channels
     :do (fluidsynth:all-notes-off *synth* c)))

(defun all-piano (&optional (sf 0))
  "set all the midi channels to ..."
  (dotimes (i 32)
    (fluidsynth:program-change *synth* i sf) ))

(defun try-sounds (time chan sf &optional (beats 8) (bank 0 bank-set))
  "iterate over soundfonts to test sounds"
  (print sf)
  (when bank-set
    (fluidsynth:bank-select *synth* chan bank))
  (fluidsynth:program-change *synth* chan (mod sf 100))
  (aat (+ time beats) #'try-sounds it chan (1+ sf) beats))

;;--------------------------------------------------

;;(fluidsynth:sfload *synth* "/usr/share/sounds/sf2/FluidR3_GM.sf2" 1)
;;(fluidsynth:sfload *synth* "/home/sendai/Downloads/Nice-Keys-Suite-V1.0.sf2" 1)
;;(fluidsynth:sfload *synth* "/home/sendai/Downloads/sf2/CTK-230_SoundFont.sf2" 1)
;;(fluidsynth:sfload *synth* "/home/sendai/Downloads/sf2/The_Ultimate Megadrive_Soundfont.sf2" 1)
;;(fluidsynth:sfload *synth* "/home/sendai/Downloads/sf2/999/Imaginary.sf2" 1)
;;(fluidsynth:sfload *synth* "/usr/share/sounds/sf2/FluidR3_GM.sf2" 1)
;;(fluidsynth:sfload *synth* "/home/sendai/samples/Touhou.sf2" 1)
;;(fluidsynth:sfload *synth* "/home/sendai/Touhou.sf2.sf2" 1)
;;(fluidsynth:sfload *synth* "/home/sendai/Downloads/KBH_Real_and_Swell_Choir.sf2" 1)
;;(fluidsynth:sfload *synth* "/home/sendai/Downloads/Majora_s_Mask_N64_Soundfont.sf2" 1)
;;(fluidsynth:sfload *synth* "/home/sendai/Downloads/sf2/Super_Nintendo_Unofficial_update.sf2" 1)
;;(fluidsynth:sfload *synth* "/home/sendai/Downloads/Nice-Keys-Suite-V1.0.sf2" 1)
;;(fluidsynth:delete *synth*)


;; (defgeneric pa (time notes offset velocity channel duration &key pan)
;;   (:documentation "Play the given notes as an arpeggio")
;;   (:method ((time double-float) (notes number) (offset number) (velocity integer) (channel integer) (duration number) &key pan)
;;     (if pan
;;         (p time notes velocity offset channel :pan pan)
;;         (p time notes velocity offset channel)))
;;   (:method ((time double-float) (notes list) (offset number) (velocity integer) (channel integer) (duration number) &key pan)
;;     (let* ((lnotes  (length notes))
;;            (offsets (loop :for i :from 0 :by offset :collect i :repeat lnotes)))
;;       (when pan
;;         (fluidsynth:cc *synth* channel 10 pan))
;;       (mapcar (lambda (n o) (p (+ time o) n velocity duration channel))
;;               notes
;;               offsets)))
;;   (:method ((time double-float) (notes list) (offset number) (velocity integer) (channel list) (duration number) &key pan)
;;     (let* ((lnotes  (length notes))
;;            (offsets (loop :for i :from 0 :by offset :collect i :repeat lnotes)))
;;       (mapcar (lambda (n o c) (p (+ time o) n velocity duration c))
;;               notes
;;               offsets
;;               channel)))
;;   (:method ((time double-float) (notes list) (offset number) (velocity list) (channel integer) (duration number) &key pan)
;;     (let* ((lnotes  (length notes))
;;            (offsets (loop :for i :from 0 :by offset :collect i :repeat lnotes)))
;;       (mapcar (lambda (n o v) (p (+ time o) n v duration channel))
;;               notes
;;               offsets
;;               velocity)))
;;   (:method ((time double-float) (notes list) (offset number) (velocity list) (channel list) (duration number) &key pan)
;;     (let* ((lnotes  (length notes))
;;            (offsets (loop :for i :from 0 :by offset :collect i :repeat lnotes)))
;;       (mapcar (lambda (n o v c) (p (+ time o) n v duration c))
;;               notes
;;               offsets
;;               velocity
;;               channel)))  
;;   (:method ((time double-float) (notes list) (offset number) (velocity integer) (channel integer) (duration list) &key pan)
;;     (let* ((lnotes  (length notes))
;;            (offsets (loop :for i :from 0 :by offset :collect i :repeat lnotes)))
;;       (mapcar (lambda (n o d) (p (+ time o) n velocity d channel))
;;               notes
;;               offsets
;;               duration)))
;;   (:method ((time double-float) (notes list) (offset number) (velocity integer) (channel list) (duration list) &key pan)
;;     (let* ((lnotes  (length notes))
;;            (offsets (loop :for i :from 0 :by offset :collect i :repeat lnotes)))
;;       (mapcar (lambda (n o c d) (p (+ time o) n velocity d c))
;;               notes
;;               offsets
;;               channel
;;               duration)))
;;   (:method ((time double-float) (notes list) (offset number) (velocity list) (channel integer) (duration list) &key pan)
;;     (let* ((lnotes  (length notes))
;;            (offsets (loop :for i :from 0 :by offset :collect i :repeat lnotes)))
;;       (mapcar (lambda (n o v d) (p (+ time o) n v d channel))
;;               notes
;;               offsets
;;               velocity
;;               duration)))
;;   (:method ((time double-float) (notes list) (offset number) (velocity list) (channel list) (duration list) &key pan)
;;     (let* ((lnotes  (length notes))
;;            (offsets (loop :for i :from 0 :by offset :collect i :repeat lnotes)))
;;       (mapcar (lambda (n o v c d) (p (+ time o) n v d c))
;;               notes
;;               offsets
;;               velocity
;;               channel
;;               duration)))
;;   (:method ((time double-float) (notes list) (offset list) (velocity integer) (channel integer) (duration number) &key pan)
;;     (mapcar (lambda (n o) (p (+ time o) n velocity duration channel))
;;             notes
;;             offset))
;;   (:method ((time double-float) (notes list) (offset list) (velocity integer) (channel list) (duration number) &key pan)
;;     (mapcar (lambda (n o c) (p (+ time o) n velocity duration c))
;;             notes
;;             offset
;;             channel))
;;   (:method ((time double-float) (notes list) (offset list) (velocity list) (channel integer) (duration number) &key pan)
;;     (mapcar (lambda (n o v) (p (+ time o) n v duration channel))
;;             notes
;;             offset
;;             velocity))
;;   (:method ((time double-float) (notes list) (offset list) (velocity list) (channel list) (duration number) &key pan)
;;     (mapcar (lambda (n o v c) (p (+ time o) n v duration c))
;;             notes
;;             offset
;;             velocity
;;             channel))
;;   (:method ((time double-float) (notes list) (offset list) (velocity integer) (channel integer) (duration list) &key pan)
;;     (mapcar (lambda (n o d) (p (+ time o) n velocity d channel))
;;             notes
;;             offset
;;             duration))
;;   (:method ((time double-float) (notes list) (offset list) (velocity integer) (channel list) (duration list) &key pan)
;;     (mapcar (lambda (n o c d) (p (+ time o) n velocity d c))
;;             notes
;;             offset
;;             channel
;;             duration))
;;   (:method ((time double-float) (notes list) (offset list) (velocity list) (channel integer) (duration list) &key pan)
;;     (mapcar (lambda (n o v d) (p (+ time o) n v d channel))
;;             notes
;;             offset
;;             velocity
;;             duration))
;;   (:method ((time double-float) (notes list) (offset list) (velocity list) (channel list) (duration list) &key pan)
;;     (mapcar (lambda (n o v c d) (p (+ time o) n v d c))
;;             notes
;;             offset
;;             velocity
;;             channel
;;             duration)))
