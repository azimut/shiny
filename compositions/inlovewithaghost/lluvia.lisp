(in-package :somecepl)

(buffer-read "/home/sendai/Downloads/lluvia.wav")

(defsynth :sample ((bufnum 0) (rate 1) (start 0) (amp .5) (out 0))
  (let ((sig (play-buf.ar 2 bufnum (* rate (buf-rate-scale.ir bufnum))
                          :start-pos (* start (buf-frames.ir bufnum))
                          :act :free
                          :loop 1)))
    (out.ar out (* amp sig))))

(defparameter *some* (synth 'sample :bufnum 0))
(ctrl *some* :rate .1)
(free *some*)
