(in-package :shiny)

;; Using https://github.com/gogins/csound-aeolus plugin.
;; I shouln't need Csound at all and I should be able to use
;; Aeolus standalone with MIDI, but, I cannot start Aeolus without Jack.
;; So this helps by letting csound in charge of controling
;; the audio driver.

;; Update: in the end ALSA has hiccups so this is only useful to play around
;;         this is due the 64 on ksmps mandatory by the plugin.

;;(setf *csound-options* '("-odac" "-m3"))

   ;; sr = 44100
   ;; kr = 4410
   ;; ksmps = 10
   ;; nchnls = 2

(setf *csound-globals* "
   sr = 44100
   kr = 4410
   ksmps = 128
   nchnls = 2")

(setf *csound-globals* "
  sr = 44100
  ksmps = 64
  nchnls = 2")

;; (setf *csound-globals* "
;;   sr = 48000
;;   ksmps = 64
;;   nchnls = 2")

(make-orc
 :aeolus 
 :globals "
 0dbfs = 1
 gi_aeolus aeolus_init \"/home/sendai/testfield/stops\", \"Aeolus\", \"waves\", 0, 10"
 :sco "
 i2 0 -1"
 :orc "
 instr 1 
 aeolus_note gi_aeolus, p4, p5, p6
 endin

 instr 2 
 aeolus_preset gi_aeolus, 1, 1, \"/home/sendai/testfield/stops/.aeolus-presets\"
 a_out[] init 2
 a_out aeolus_out gi_aeolus
 out a_out
 endin")

(start-csound (get-orchestra :aeolus))
(start-thread)

(make-play flute "i1" :chan 2 :midi 60 :vel 60)
(make-play honk  "i1" :chan 0 :midi 60 :vel 60)
(make-play organ "i1" :chan 1 :midi 60 :vel 60)
(make-play eorgan "i1" :chan 3 :midi 60 :vel 60)

(play-flute 60 1)

(defun f (time)
  (play-eorgan 64 .2 :vel 20)
  (aat (+ time #[1 b]) #'f it))

(defun f ())

(f (now))
