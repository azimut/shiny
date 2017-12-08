;;;; somecepl.lisp

(in-package #:somecepl)

;;; "somecepl" goes here. Hacks and glory await!

(defvar *box* nil)
(defvar *vert-stream* nil)

(defun-g draw-verts-vert-stage ((vert g-pnt) &uniform (resolution :vec2) (time :float)
                                (perspective :mat4))
  (let* ((pos (pos vert))
         (w (+ 1.6 (z pos)))
         (pos (+ pos (v! (sin time) 0 -4)))
         )
    (* perspective (v! pos 1))))

(defun-g draw-verts-frag-stage (&uniform (resolution :vec2) (time :float))
  (v! 0 1 0 0))

(defpipeline-g draw-verts-pipeline ()
  :vertex   (draw-verts-vert-stage g-pnt)
  :fragment (draw-verts-frag-stage))

(defun now ()
  (/ (float (get-internal-real-time))
     3000))

(defun draw! ()
   (step-host)
   (setf (resolution (current-viewport))
         (surface-resolution (current-surface *cepl-context*)))
   (clear)
   (map-g #'draw-verts-pipeline *box*
          :resolution (viewport-resolution (current-viewport))
          :time (now)
          :perspective (rtg-math.projection:perspective
                        (x (resolution (current-viewport)))
                        (y (resolution (current-viewport)))
                        0.1 30f0 60f0))
   (swap))

(defun init ()

  (when *vert-stream*
    (free *vert-stream*))

  (setf *box* (box 1.0 1.0 1.0)))

(def-simple-main-loop play (:on-start #'init)
  (draw!))
