;;;; somecepl.lisp

(in-package #:somecepl)

;;; "somecepl" goes here. Hacks and glory await!

(defvar *gpu-verts-arr* nil)
(defvar *gpu-index-arr* nil)
(defvar *vert-stream* nil)

(defun-g draw-verts-vert-stage ((vert :vec2))
  (v! vert 0 1))

; https://www.shadertoy.com/view/XtGGRt
;; (defun-g mm2 ((a :float))
;;   (let* ((c (cos a))
;;          (s (sin a)))
;;     (mat2 c s (* -1 s) c)))
(defun-g hash33 ((p :vec3))
  (let* ((pp   (fract (* p (v! 443.8975 397.2973 491.1871))))
         (pp~1 (+ pp (v3! (dot (s~ p :zxy) (+ (v3! 19.27) (s~ p :yxz) ))))))
    (fract (v! (* (x pp~1) (y pp~1))
               (* (z pp~1) (x pp~1))
               (* (y pp~1) (z pp~1))))))

;; (defun-g bg ((rd :vec3))
;;   (let* ((sd (pow (+ 0.5 (* 0.5 (dot (normalize (v! -.5 -.6 .9))
;;                                      rd)))
;;                   5.0))
;;          (col (mix (v! .05 .1 .2) (v! .1 .05 .2) sd)))
;;     (* (v3! .63) col)))

(defun-g bg ((rd :vec3))
  (let* ((sd (pow (+ 0.9
                     (* 0.5
                        (dot (normalize (v! -.5 -.6 .9))
                              rd)))
                  5.0))
         (col (mix (v! .05 .1 .2) (v! .1 .05 .2) sd)))
    (* (v3! .8) col)))

(defun-g stars ((p :vec3) (resolution :vec2))
  (let* ((c (v! 0.0 0.0 0.0))
         (res (* 1.0 (x resolution))))
    (for (i 0) (< i 4) (++ i)
         (let* ((q  (- (fract (* p (* .15 res))) (v3! 0.5)))
               (rn (s~ (hash33 (floor (* p (* .15 res)))) :xy))
               (c2 (* (step (x rn) (+ 0.005 (* i i 0.001))) 
                      (- 1.0 (smoothstep 0.0 .6 (length q))))))
           (setf c (+ c (* c2 (+ (v3! 0.9) (* 0.1 (mix (v! 1.0 0.49 0.1)
                                                 (v! 0.75 0.9 1.0)
                                                 (y rn)))))))
           (setf p (* p (v3! 1.3)))))
    (* c c .8)))

(defun-g draw-verts-frag-stage (&uniform (resolution :vec2))
  (let* ((q (/ (s~ gl-frag-coord :xy)
               (s~ resolution :xy)))
         (p (v! (x (* (- q (v2! 0.5)) (/ (x resolution) (y resolution))))
                (y (- q (v2! 0.5)))))
         (ro (v! 0.0 0.0 -6.7))
         (rd (normalize (v! p 1.3)))
         (fade (+ 0.9 (* 0.1 (smoothstep 0.0 0.01 (abs (y rd))))))
         (col (* fade (bg rd))))
         
    (v! (+ rd (stars col resolution)) 1.0)
  ))


(defun-g draw-verts-frag-stage (&uniform (resolution :vec2))
  (let* ((st    (v! (/ (x gl-frag-coord) (x resolution))
                    (/ (y gl-frag-coord) (y resolution))))
         (color (v! (step .5 (x st))
                    (step .5 (x st))
                    (step .5 (x st)))))
    (v! 0.0 0.0 (* .1  (x (hash33 (v! .2 100. .5)))) 1.0)
  ))

(defpipeline-g draw-verts-pipeline ()
  :vertex   (draw-verts-vert-stage :vec2)
  :fragment (draw-verts-frag-stage))

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
