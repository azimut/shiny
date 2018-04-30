(in-package :somecepl)

;; OPTIONAL
;;(set-rt-block-size 64)
(ql:quickload :incudine-fluidsynth)

;; Run it on the repl
;; (rt-start)

;; C-c C-k

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
