(in-package :shiny)

(defvar *buf* (make-buffer 512 :channels 1))

(dsp! bplay
    ((buf buffer) rate start-pos
     (loop-p boolean) attenuation rms (index fixnum))
  (:defaults 0d0 1 0 nil .00001 0 0)
  (with-samples
      ((in (incudine.vug:buffer-play
            buf rate start-pos loop-p #'incudine:free))
       (inn (* in attenuation))
       (inn (+ inn)))
    (setf index (mod (1+ index) (buffer-size *buf*)))
    (setf (smp-ref (incudine::buffer-base-data-ptr *buf*) index)
          inn)
    (out inn inn)))
