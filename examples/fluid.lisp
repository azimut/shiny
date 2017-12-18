(in-package :somecepl)


(defvar *fluid-settings* (fluidsynth:new-settings
                           `(("synth.polyphony" 128)
                             ("synth.sample-rate" ,*sample-rate*)
                             ("audio.sample-format" "float"))))


(defvar *synth* (fluidsynth:new *fluid-settings*))

(dsp! fluid-test ((synth fluidsynth:synth))
  (with ((len (block-size))
         (left (make-f32-array len))
         (right (make-f32-array len)))
    (fluidsynth:write-float synth len left 0 1 right 0 1)
    (foreach-frame
      (out (f32-ref left current-frame)
           (f32-ref right current-frame)))))

(fluidsynth:sfload *synth* "/usr/share/sounds/sf2/FluidR3_GM.sf2" 1)
(fluidsynth:sfload *synth* "/home/sendai/Downloads/Sonatina_Symphonic_Orchestra.sf2" 1)
(fluidsynth:sfload *synth* "/home/sendai/Downloads/Nice-Keys-Ultimate-V2.3.sf2" 1)

(setf (fluidsynth:setting *fluid-settings* "synth.gain") 0.5)

(set-rt-block-size 64)

(rt-start)
(fluid-test *synth*)

(fluidsynth:noteon *synth* 0 60 50)

#|
(define play-midi-note
  (lambda (time device pitch velocity duration channel)
    (callback time 'midi_send device *midi-note-on* channel pitch velocity)
    (callback (+ time duration) 'midi_send device *midi-note-off* channel pitch velocity)))
|#

(defun play-midi-note (pitch velocity dur)
  (at (now) #'fluidsynth:noteon *synth* 3 pitch velocity)
  (at (+ (now) #[dur s]) #'fluidsynth:noteoff *synth* 3 pitch)
  )

(play-midi-note 60 60 10)

;; ----------------------
;; DANCING PHALANGES
(setf (bpm *tempo*) 60)
(defvar *root* nil)
(setf *root* 0)
(defvar *degree* nil)
(setf *degree* 'i)
(defvar *scale* nil)
(setf *scale* (scale 0 'phrygian))

(defun melody (ms rs)
  (if (not ms)
      'done
      (let ((p (car ms)))
        (play-midi-note (quant (+ 48 *root* p) *scale*)
                        (round (cosr 60 40 1/2)) 
                        (* (car rs) (cosr 4. .2 1/2)))
        (play-midi-note (quant (+ 55 *root* p) *scale*)
                        (round (cosr 60 40 1/2))
                        (* (car rs) (cosr 4. .2 1/2)))
        (at (+ (now) #[(car rs) b]) #'melody (cdr ms) (cdr rs))
        )))

(defun right (degree)
  (setf *degree* degree)
  (setf *root* (car (diatonic 0 '- degree)))
  (at (+ (now) #[4 b]) #'right (random-list (cdr (assoc degree '((i n6 iii vii)
                                                                 (n6 v7)
                                                                 (iii v7)
                                                                 (vii vi)
                                                                 (vi vii v7 v7)
                                                                 (v7 i))))))
  )

(right 'i)

(defun left (dur)
  (if (> (random 1.0) .85) (melody '(0 1 0 -1) '(1/3 2/3 2/3 1)))
  (play-midi-note (+ 48 *root*)
                  (round (cosr 80 30 1)) 10)
                  ;(* dur (cosr 2.2 .3 1/7)))
  (play-midi-note 36 (round (cosr 80 30 1)) 10)
                    ; (* dur (cosr 2.2 .3 1/7)))
  (at (+ (now) #[dur b]) #'left (random-list '(1 2/3))))

(left 2/3)
(flush-pending)
