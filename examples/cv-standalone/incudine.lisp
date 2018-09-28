(in-package :shiny)

(defun video-show
    (name filepath time dur
     &key hsv glitch (repeat 1) (rotation 0f0)
       (scale 1f0) (ypos 0) (xpos 0))
  (if (make-cvideo name filepath :hsv hsv :glitch glitch :repeat repeat :rotation rotation :scale scale :ypos ypos :xpos xpos)
      (at (+ time #[dur b]) #'queue-delete name)))

(defun f (time)
  (aat (+ time #[1 b]) #'f it))

