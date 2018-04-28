(in-package :somecepl)

(defvar *brain*
  (buffer-load "/home/sendai/Downloads/177479__unfa__slowly-raining-loop.flac"))

(dsp! bplay ((buf buffer) rate start-pos (loop-p boolean))
  (foreach-channel
    (cout (incudine.vug:buffer-play buf rate start-pos loop-p #'stop))))

(bplay *brain* 1 0 t :id 3)
(set-control 3 :rate 1)
(set-control 3 :loop-p nil)
