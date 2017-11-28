;;;; somecepl.lisp

(in-package #:somecepl)

;;; "somecepl" goes here. Hacks and glory await!

(defvar *gpu-verts-arr* nil)
(defvar *gpu-index-arr* nil)
(defvar *vert-stream* nil)

(defun-g vert-stage ((pos :vec2))
  (values (v! pos 0 1)
          (* 0.5 (+ pos (v2! 1.0)))))

(defun-g hash33 ((p :vec3) (time :int))
  (let* ((p   (fract (* p (v!  (* 0.00000000001 time)
                               "7.2973"
                               "1.1871"))))
         (p (+ p (v3! (dot (s~ p :zxy)
                               (+ (v3! 19.27)
                                 (s~ p :yxz) ))))))
    (fract (v! (* (x p) (y p))
               (* (z p) (x p))
               (* (y p) (z p))))))


(defun-g stars ((p :vec3) (resolution :vec2) (time :int))
  (let* ((c (v3! 0.0))
         (res (* 1. (x resolution))))
    (for (i 0.) (< i 4) (++ i)
         (let* ((q  (- (fract (* p (* .15 res)))
                       (v3! 0.5)))
                (rn (s~ (hash33 (floor (* p (* .15 res))) time) :xy))
                (c2 (* (step (x rn) (+ .0005 (* i i .001))) 
                       (- 1.0 (smoothstep 0.0 .6 (length q))))))
           (setf c (+ c (* c2 (+ (v3! 0.9)
                                 (* .1 (mix (v! 1.0 .49 .1)
                                            (v! .75 .9 1.)
                                            (y rn)))))))
           (setf p (* p (v3! 1.3)))))
    (* c c .8 )))


(defun-g frag-stage ((uv :vec2) &uniform (resolution :vec2))
  (let* ((st    (v! (/ (x gl-frag-coord) (x resolution))
                    (/ (y gl-frag-coord) (y resolution))))
         (color (v! (step .5 (x st))
                    (step .5 (x st))
                    (step .5 (x st)))))
  ;;   (graph (lambda ((x :float)) (sin x))
  ;;          uv
  ;;          (v! -5 5 -2 2))

    ;; (graph (lambda ((x :float)) (x (hash33 (v! x x x) 1)))
    ;;        uv
    ;;        (v! -.01 .01 -1 1))

    
    ;; (graph (lambda ((x :float)) (y (stars (v! 1. x 1.)
    ;;                                       (v! 320. 240.)
    ;;                                       1)))
    ;;        uv
    ;;        (v! -3 3 -1 1))
    
    (plot  .5
           uv
           (v! -3 3 -1 1))
    ))

(defpipeline-g draw-verts-pipeline ()
  :vertex   (vert-stage :vec2)
  :fragment (frag-stage :vec2))

(defun now ()
  (get-internal-real-time))

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
