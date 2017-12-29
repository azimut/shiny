(in-package :somecepl)

(rt-start)

(+ (now) #[1 b])

(ql:quickload :incudine-fluidsynth)

(defvar *fluid-settings* (fluidsynth:new-settings
                           `(("synth.polyphony" 128)
                             ("synth.sample-rate" ,*sample-rate*)
                             ("audio.sample-format" "float"))))


(defvar *synth* (fluidsynth:new *fluid-settings*))

;; (defparameter *env1* (make-envelope '(0 1 1 0) '(0 .9 .1)))

(dsp! fluid-test ((synth fluidsynth:synth))
  (with ((len (block-size))
         (left (make-f32-array len))
         (right (make-f32-array len)))
    (fluidsynth:write-float synth len left 0 1 right 0 1)
    (foreach-frame
      (out (f32-ref left current-frame)
           (f32-ref right current-frame)))))


(fluidsynth:sfload *synth* "/home/sendai/Downloads/fluid-soundfont-3.1/FluidR3_GM.sf2" 1)
(fluidsynth:sfload *synth* "/usr/share/sounds/sf2/FluidR3_GM.sf2" 1)
(fluidsynth:sfload *synth* "/home/sendai/Downloads/samples/GeneralUser GS 1.471/GeneralUser GS v1.471.sf2" 1)
(fluidsynth:sfload *synth* "/home/sendai/Downloads/Sonatina_Symphonic_Orchestra.sf2" 1)
(fluidsynth:sfload *synth* "/home/sendai/Downloads/Nice-Keys-Ultimate-V2.3.sf2" 1)

#|
(fluidsynth:get-active-voice-count *synth*)
(fluidsynth:stop *synth* 1)
(incudine:free 0)
(fluidsynth:delete *synth*)
|#

(fluidsynth:set-reverb *synth* 0.7d0 0.9d0 0.5d0 0.9d0)
(set-rt-block-size 64)

(setf (fluidsynth:setting *fluid-settings* "synth.gain") .9)
(setf (fluidsynth:setting *fluid-settings* "synth.midi-channels") 24)

(rt-stop)
(rt-start)
(fluid-test *synth*)
(fluidsynth:program-change *synth* 24 24)
(fluidsynth:noteon *synth* 24 60 100)

#|
(incudine:free 1)
(flush-pending)
(define play-midi-note
  (lambda (time device pitch velocity duration channel)
    (callback time 'midi_send device *midi-note-on* channel pitch velocity)
    (callback (+ time duration) 'midi_send device *midi-note-off* channel pitch velocity)))
|#

(defun play-midi-note (time pitch velocity dur)
     (at time #'fluidsynth:noteon *synth* 3 pitch velocity)
     (at (+ time #[dur s]) #'fluidsynth:noteoff *synth* 3 pitch)
  )

(play-midi-note (now) 60 60 3)

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
;  (if (> (random 1.0) .8) (melody '(0 1 0 -1) '(1/3 2/3 2/3 1)))
  (play-midi-note (+ 48 *root*)
                  (round (cosr 80 30 1)) 10)
                  ;(* dur (cosr 2.2 .3 1/7)))
  (play-midi-note 36 (round (cosr 80 30 1)) 10)
                    ; (* dur (cosr 2.2 .3 1/7)))
  (at (+ (now) #[dur b]) #'left (random-list '(1 2/3))))

(left 2/3)
(flush-pending)
(incudine:free 0)

;; ----------

(defvar *piece* nil)
(setf *piece* '(:E4 :F#4 :B4 :C#5 :D5 :F#4 :E4 :C#5 :B4 :F#4 :D5 :C#5))
(setf (bpm *tempo*) 35)

(defun player (time speed notes)
  (let ((n      (first notes))
        (notes  (cdr notes))
        (t-next (+ time speed)))
    (when n
      (play-midi-note (note-name-to-midi-number (symbol-name n)) 50 10)
      (at t-next #'player t-next speed notes))))

#|
(flush-pending)
|#

(let ((time (now)))
  (player time #[.338 b] (repeat 1000 *piece*))
  (player time #[.335 b] (repeat 1000 *piece*)))
