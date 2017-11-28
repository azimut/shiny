;;;; somecepl.lisp

(in-package #:somecepl)

;;; "somecepl" goes here. Hacks and glory await!

(defvar *gpu-verts-arr* nil)
(defvar *gpu-index-arr* nil)
(defvar *vert-stream* nil)

(defparameter *texture* nil)
(defparameter *sampler* nil)

(defun-g draw-verts-vert-stage ((vert :vec2))
  (values (v! vert 0 1)
          (+ (v2! 0.5) (* vert 0.5))))

(defun-g draw-verts-frag-stage ((uv :vec2) &uniform (resolution :vec2)
                                         (time :float)
                                         (sam :sampler-2d))
  (let* ((st    (/ (s~ gl-frag-coord :xy)
                   (v! 1024. 768.) ))
;         (v     (* 5.0 0.4))
;         (mode 0.0)
;         (scTime (* time .01))
         (dt0   (* 100. (y st) (step (+ 0.8 (* 0.8 (sin time)))
                                     (mod time 1.0))))
         (dt    (* 10.  (x st) (step (+ 0.5 (* 0.5 (sin time)))
                                     (mod time 1.0))))
         (dt2   (* 10.  (y st) (step (+ 0.5 (* 0.5 (sin time)))
                                     (mod time 1.0))))
         (dx    100.)
         (minVX (step 0.2
                      (* 5.0 (mod (+ (x st) dt)  0.2))))
         (minVY (step 0.2
                      (* 5.0 (mod (+ (y st) dt2) 0.2))))
         )
    (v! minVX 0.0 0.0 1.0)
  ))

(defpipeline-g draw-verts-pipeline ()
  :vertex   (draw-verts-vert-stage :vec2)
  :fragment (draw-verts-frag-stage :vec2))

(defun now ()
  (* (get-internal-real-time)
     .001))

(defun draw! ()
   (step-host)
   (setf (resolution (current-viewport))
         (surface-resolution (current-surface *cepl-context*)))
   (clear)
   (map-g #'draw-verts-pipeline *vert-stream*
          :resolution (viewport-resolution (current-viewport))
          :time (now)
          :sam *texture*)
   (swap))

(defun init ()

  (when *gpu-verts-arr*
    (free *gpu-verts-arr*))
  (when *gpu-index-arr*
    (free *gpu-index-arr*))
  (when *vert-stream*
    (free *vert-stream*))

  (when *texture*
    (free *texture*))

  (setf *texture*
        (tex "patitos.jpg"))
  
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

(def-simple-main-loop play (:on-start #'init)
  (draw!))
