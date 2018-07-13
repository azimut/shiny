(in-package :somecepl)

;; Incudine

;; OPTIONAL
;;(set-rt-block-size 64)

(ql:quickload :incudine-fluidsynth)

;; Run it on the repl
;; (rt-start)

(defvar *settings* (fluidsynth:new-settings
                    `(("synth.polyphony" 128)
                      ("synth.midi-channels" 32)
                      ("synth.sample-rate" ,*sample-rate*)
                      ("audio.sample-format" "float"))))

(defvar *synth* (fluidsynth:new *settings*))

;; ---------------------------------------------------

(dsp! fluid-test ((synth fluidsynth:synth))
  (with ((len   (block-size))
         (left  (make-f32-array len))
         (right (make-f32-array len)))
    (fluidsynth:write-float synth len left 0 1 right 0 1)
    (foreach-frame
      (out (f32-ref left current-frame)
           (f32-ref right current-frame)))))

(fluid-test *synth*)

;; --------------------------------------------------

;; Incudine replaced macro, using bare beat call instead of #[] (less clutter)
(defmacro aat (time function &rest arguments)
  (let* ((it    (intern "IT"))
         (delta (caddr time))
         (real  (cadr time))
         (next  `(+ ,real #[,delta b])))
    `(let ((,it ,next))
       (at ,it ,function ,@arguments))))

(defun quant (beats)
  "returns the time (+ (now) beats), sc-collider like thing"
  (+ (now) #[beats b]))

;; --------------------------------------------------

(defgeneric p (time pitch velocity duration channel &key pan)
  (:method ((time double-float) (pitch list) (velocity integer) (duration number) (channel integer) &key pan)
    "Play chord of notes"
    (if pan
        (mapcar (lambda (x) (p time x velocity duration channel :pan pan))
                pitch)
        (mapcar (lambda (x) (p time x velocity duration channel))
                pitch)))
  (:method ((time double-float) (pitch list) (velocity integer) (duration number) (channel list) &key pan)
    "Play chord of notes, on provided channels"
    (mapcar (lambda (x y) (p time x velocity duration y))
            pitch
            channel))
  (:method ((time double-float) (pitch integer) (velocity integer) (duration number) (channel integer) &key pan)
    "Play given pitch"
    (when (and (> pitch 0)
               (< pitch 127)
               (> duration 0))
      (when pan
        (fluidsynth:cc *synth* channel 10 pan))
      (at time #'fluidsynth:noteon *synth* channel pitch velocity)
      (at (+ time #[duration b]) #'fluidsynth:noteoff *synth* channel pitch)))
  (:method ((time double-float) (pitch integer) (velocity integer) (duration symbol) (channel integer) &key pan)
    "Play given pitch, at CM rythm"
    (let ((d (cm:rhythm duration)))
      (when (> d 0)
        (at time #'fluidsynth:noteon *synth* channel pitch velocity)
        (at (+ time #[d b]) #'fluidsynth:noteoff *synth* channel pitch))))
  (:method ((time double-float) (pitch symbol) (velocity integer) (duration symbol) (channel integer) &key pan)
    "Play given note on american notation, at CM rhythm"
    (unless (eql :_ pitch)
      (let ((n (note pitch))
            (d (cm:rhythm duration)))
        (when (> d 0)
          (at time #'fluidsynth:noteon
                    *synth* channel n velocity)
          (at (+ time #[d b]) #'fluidsynth:noteoff
                    *synth* channel n)))))
  (:method ((time double-float) (pitch symbol) (velocity integer) (duration number) (channel integer) &key pan)
    "Play given note on american notation"
    (when (and (> duration 0)
               (not (eql :_ pitch)))
      (let ((n (note pitch)))
        (at time #'fluidsynth:noteon
                  *synth* channel n velocity)
        (at (+ time #[duration b]) #'fluidsynth:noteoff
                  *synth* channel n)))))
