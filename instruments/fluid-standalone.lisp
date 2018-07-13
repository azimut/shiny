(in-package :somecepl)

;; Use this with https://github.com/zazzerino/cl-fluidsynth
;; This version of cl-fluidsynth adds the standalone audio-driver support

(defvar *settings* (fluidsynth:new-settings
                    `(("synth.polyphony" 128)
                      ("synth.midi-channels" 32)
                      ("audio.driver" "alsa")
                      ("synth.sample-rate" ,*sample-rate*)
                      ("audio.sample-format" "float"))))

(defvar *synth* (fluidsynth:new-synth *settings*))
(defvar *audio-driver* (fluidsynth:new-audio-driver *settings* *synth*))

;; Incudine like macro, using callback instead (less clutter)
(defmacro aat (time function &rest arguments)
  (let ((it (intern "IT")))
    `(let ((,it ,time))
       (callback ,it ,function ,@arguments))))

;; TODO: complete time list param (use pa instead)
;;       use pan
(defgeneric p (time pitch velocity duration channel &key pan)
  ;; (:method ((time list) (pitch integer) (velocity integer) (duration number) (channel integer) &key pan)
  ;;   (mapcar (lambda (istime) (p istime pitch velocity duration channel))
  ;;           time))
  ;; (:method ((time list) (pitch list) (velocity integer) (duration number) (channel integer) &key pan)
  ;;   (mapcar (lambda (istime ispitch) (p istime ispitch velocity duration channel))
  ;;           time
  ;;           pitch))
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
      (callback time #'fluidsynth:noteon *synth* channel pitch velocity)
      (callback (+ time duration) #'fluidsynth:noteoff *synth* channel pitch)))
  (:method ((time double-float) (pitch integer) (velocity integer) (duration symbol) (channel integer) &key pan)
    "Play given pitch, at CM rythm"
    (let ((d (cm:rhythm duration)))
      (when (> d 0)
        (callback time #'fluidsynth:noteon *synth* channel pitch velocity)
        (callback (+ time d) #'fluidsynth:noteoff *synth* channel pitch))))
  (:method ((time double-float) (pitch symbol) (velocity integer) (duration symbol) (channel integer) &key pan)
    "Play given note on american notation, at CM rhythm"
    (unless (eql :_ pitch)
      (let ((n (note pitch))
            (d (cm:rhythm duration)))
        (when (> d 0)
          (callback time #'fluidsynth:noteon
                    *synth* channel n velocity)
          (callback (+ time d) #'fluidsynth:noteoff
                    *synth* channel n)))))
  (:method ((time double-float) (pitch symbol) (velocity integer) (duration number) (channel integer) &key pan)
    "Play given note on american notation"
    (when (and (> duration 0)
               (not (eql :_ pitch)))
      (let ((n (note pitch)))
        (callback time #'fluidsynth:noteon
                  *synth* channel n velocity)
        (callback (+ time duration) #'fluidsynth:noteoff
                  *synth* channel n)))))

;; --------------------------------------------------



