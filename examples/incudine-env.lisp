;;;; somecepl.lisp

(in-package #:somecepl)

;;; "somecepl" goes here. Hacks and glory await!

(defvar *gpu-verts-arr* nil)
(defvar *gpu-index-arr* nil)
(defvar *vert-stream* nil)
(defvar *env1* nil)

(defun-g draw-verts-vert-stage ((vert :vec2))
  (v! (- vert (v2! .4) ) 0 1))

(defun-g draw-verts-frag-stage (&uniform (resolution :vec2))
  (v4! 1.0))

(defpipeline-g draw-verts-pipeline (:points)
  :vertex   (draw-verts-vert-stage :vec2)
  :fragment (draw-verts-frag-stage))

(defun draw! ()
  (step-host)
  (setf (resolution (current-viewport))
        (surface-resolution (current-surface *cepl-context*)))
  (clear)
  (map-g #'draw-verts-pipeline *vert-stream*
         :resolution (viewport-resolution (current-viewport)))
  (swap))

(defun init ()

  (when *gpu-verts-arr*
    (free *gpu-verts-arr*))
  (when *gpu-index-arr*
    (free *gpu-index-arr*))
  (when *vert-stream*
    (free *vert-stream*))
  ;; (when *env1*
  ;;   (incudine:free *env1*))

  ;; (setf *env1*
  ;;       (make-envelope '(0 1 0) '(.2 .8) :curve :exp))
  
  ;; (setf *gpu-verts-arr*
  ;;       (make-gpu-array (loop for beats below 0.99 by .001 collect (v! beats (coerce (envelope-at *env1* beats) 'single-float)))
  ;;        :element-type :vec2))
  
  (setf *env1*
        (make-perc .001 .4))
  
  (setf *gpu-verts-arr*
        (make-gpu-array
         (loop for beats below 0.39 by .001
            collect
              (v! beats (coerce (envelope-at *env1* beats) 'single-float)))
         :element-type :vec2))

  (setf *vert-stream*
        (make-buffer-stream *gpu-verts-arr*
                            :primitive :points)))

(def-simple-main-loop play (:on-start #'init)
  (draw!))
