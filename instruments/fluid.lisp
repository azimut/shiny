(in-package :shiny)

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
        (fluidsynth:cc *synth* channel 10 (min (max 0 pan) 127)))
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
    (unless (and (eql :_ pitch) (eql 'cm::r pitch))
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
               (not (eql :_ pitch))
               (not (eql 'cm::r pitch)))
      (let ((n (note pitch)))
        (at time #'fluidsynth:noteon
                  *synth* channel n velocity)
        (at (+ time #[duration b]) #'fluidsynth:noteoff
                  *synth* channel n)))))

(defgeneric pa (time notes offset velocity channel duration &key pan)
  (:documentation "Play the given notes as an arpeggio")
  (:method ((time double-float) (notes number) (offset number) (velocity integer) (channel integer) (duration number) &key pan)
    (if pan
        (p time notes velocity offset channel :pan pan)
        (p time notes velocity offset channel)))
  (:method ((time double-float) (notes list) (offset number) (velocity integer) (channel integer) (duration number) &key pan)
    (let* ((lnotes  (length notes))
           (offsets (loop :for i :from 0 :by offset :collect i :repeat lnotes)))
      (when pan
        (fluidsynth:cc *synth* channel 10 pan))
      (mapcar (lambda (n o) (p (+ time #[o b]) n velocity duration channel))
              notes
              offsets)))
  (:method ((time double-float) (notes list) (offset number) (velocity integer) (channel list) (duration number) &key pan)
    (let* ((lnotes  (length notes))
           (offsets (loop :for i :from 0 :by offset :collect i :repeat lnotes)))
      (mapcar (lambda (n o c) (p (+ time #[o b]) n velocity duration c))
              notes
              offsets
              channel)))
  (:method ((time double-float) (notes list) (offset number) (velocity list) (channel integer) (duration number) &key pan)
    (let* ((lnotes  (length notes))
           (offsets (loop :for i :from 0 :by offset :collect i :repeat lnotes)))
      (mapcar (lambda (n o v) (p (+ time #[o b]) n v duration channel))
              notes
              offsets
              velocity)))
  (:method ((time double-float) (notes list) (offset number) (velocity list) (channel list) (duration number) &key pan)
    (let* ((lnotes  (length notes))
           (offsets (loop :for i :from 0 :by offset :collect i :repeat lnotes)))
      (mapcar (lambda (n o v c) (p (+ time #[o b]) n v duration c))
              notes
              offsets
              velocity
              channel)))  
  (:method ((time double-float) (notes list) (offset number) (velocity integer) (channel integer) (duration list) &key pan)
    (let* ((lnotes  (length notes))
           (offsets (loop :for i :from 0 :by offset :collect i :repeat lnotes)))
      (mapcar (lambda (n o d) (p (+ time #[o b]) n velocity d channel))
              notes
              offsets
              duration)))
  (:method ((time double-float) (notes list) (offset number) (velocity integer) (channel list) (duration list) &key pan)
    (let* ((lnotes  (length notes))
           (offsets (loop :for i :from 0 :by offset :collect i :repeat lnotes)))
      (mapcar (lambda (n o c d) (p (+ time #[o b]) n velocity d c))
              notes
              offsets
              channel
              duration)))
  (:method ((time double-float) (notes list) (offset number) (velocity list) (channel integer) (duration list) &key pan)
    (let* ((lnotes  (length notes))
           (offsets (loop :for i :from 0 :by offset :collect i :repeat lnotes)))
      (mapcar (lambda (n o v d) (p (+ time #[o b]) n v d channel))
              notes
              offsets
              velocity
              duration)))
  (:method ((time double-float) (notes list) (offset number) (velocity list) (channel list) (duration list) &key pan)
    (let* ((lnotes  (length notes))
           (offsets (loop :for i :from 0 :by offset :collect i :repeat lnotes)))
      (mapcar (lambda (n o v c d) (p (+ time #[o b]) n v d c))
              notes
              offsets
              velocity
              channel
              duration)))
  (:method ((time double-float) (notes list) (offset list) (velocity integer) (channel integer) (duration number) &key pan)
    (mapcar (lambda (n o) (p (+ time #[o b]) n velocity duration channel))
            notes
            offset))
  (:method ((time double-float) (notes list) (offset list) (velocity integer) (channel list) (duration number) &key pan)
    (mapcar (lambda (n o c) (p (+ time #[o b]) n velocity duration c))
            notes
            offset
            channel))
  (:method ((time double-float) (notes list) (offset list) (velocity list) (channel integer) (duration number) &key pan)
    (mapcar (lambda (n o v) (p (+ time #[o b]) n v duration channel))
            notes
            offset
            velocity))
  (:method ((time double-float) (notes list) (offset list) (velocity list) (channel list) (duration number) &key pan)
    (mapcar (lambda (n o v c) (p (+ time #[o b]) n v duration c))
            notes
            offset
            velocity
            channel))
  (:method ((time double-float) (notes list) (offset list) (velocity integer) (channel integer) (duration list) &key pan)
    (mapcar (lambda (n o d) (p (+ time #[o b]) n velocity d channel))
            notes
            offset
            duration))
  (:method ((time double-float) (notes list) (offset list) (velocity integer) (channel list) (duration list) &key pan)
    (mapcar (lambda (n o c d) (p (+ time #[o b]) n velocity d c))
            notes
            offset
            channel
            duration))
  (:method ((time double-float) (notes list) (offset list) (velocity list) (channel integer) (duration list) &key pan)
    (mapcar (lambda (n o v d) (p (+ time #[o b]) n v d channel))
            notes
            offset
            velocity
            duration))
  (:method ((time double-float) (notes list) (offset list) (velocity list) (channel list) (duration list) &key pan)
    (mapcar (lambda (n o v c d) (p (+ time #[o b]) n v d c))
            notes
            offset
            velocity
            channel
            duration)))
