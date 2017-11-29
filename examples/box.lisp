;;;; somecepl.lisp

(in-package #:somecepl)

;;; "somecepl" goes here. Hacks and glory await!

(defvar *box* nil)
(defvar *vert-stream* nil)

(defun-g draw-verts-vert-stage ((vert g-pnt))
  (v! (pos vert) 1))

(defun-g draw-verts-frag-stage (&uniform (resolution :vec2))
  (v! 1 0 0 0))

(defpipeline-g draw-verts-pipeline ()
  :vertex   (draw-verts-vert-stage g-pnt)
  :fragment (draw-verts-frag-stage))

(defun now ()
  (get-internal-real-time))

(defun draw! ()
   (step-host)
   (setf (resolution (current-viewport))
         (surface-resolution (current-surface *cepl-context*)))
   (clear)
   (map-g #'draw-verts-pipeline *box*
          :resolution (viewport-resolution (current-viewport)))
   (swap))

(defun init ()

  (when *vert-stream*
    (free *vert-stream*))

  (setf *box* (box 1.0 1.0 1.0)))

(def-simple-main-loop play (:on-start #'init)
  (draw!))
