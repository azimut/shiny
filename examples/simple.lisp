;;;; somecepl.lisp

(in-package #:somecepl)

;;; "somecepl" goes here. Hacks and glory await!

(defvar *gpu-verts-arr* nil)
(defvar *gpu-index-arr* nil)
(defvar *vert-stream* nil)

(defun-g draw-verts-vert-stage ((vert :vec2))
  (v! vert 0 1))

(defun-g draw-verts-frag-stage (&uniform (resolution :vec2) (time :float))
  (let* ((st    (v! (/ (x gl-frag-coord) (x resolution))
                    (/ (y gl-frag-coord) (y resolution))))
         (color (v! (step .5 (x st))
                    (step .5 (x st))
                    (step .5 (x st)))))
    (v! (s~ color :xy) (sin time) 1.0)
  ))

(defpipeline-g draw-verts-pipeline ()
  :vertex   (draw-verts-vert-stage :vec2)
  :fragment (draw-verts-frag-stage))

(defun draw! ()
   (step-host)
;;   (setf (resolution (current-viewport))
;;         (surface-resolution (current-surface *cepl-context*)))
   (clear)
   (map-g #'draw-verts-pipeline *vert-stream*
          :resolution (viewport-resolution (current-viewport))
          :time (mynow))
   (swap))

(defun runinit ()

  (when *gpu-verts-arr*
    (free *gpu-verts-arr*))
  (when *gpu-index-arr*
    (free *gpu-index-arr*))
  (when *vert-stream*
    (free *vert-stream*))
  
  (setf *gpu-verts-arr*
        (make-gpu-array
         (list (v! -1.0  1.0)
               (v! -1.0 -1.0)
               (v!  1.0 -1.0)
               (v!  1.0  1.0))
         :element-type :vec2))
  
  (setf *gpu-index-arr*
        (make-gpu-array
         (list 0 1 2
               0 2 3)))

  (setf *vert-stream*
        (make-buffer-stream *gpu-verts-arr*
                            :index-array *gpu-index-arr*)))

(def-simple-main-loop runplay (:on-start #'runinit)
  (draw!))
