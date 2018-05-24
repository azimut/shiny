;;;; somecepl.lisp

(in-package #:somecepl)

;;; "somecepl" goes here. Hacks and glory await!

(defvar *gpu-verts-arr* nil)
(defvar *gpu-index-arr* nil)
(defvar *vert-stream* nil)

(defun-g draw-verts-vert-stage ((vert :vec2))
  (v! vert 0 1))

;; float stroke(float x, float s, float w){ 
;;     float d = step(s,x+w*.5) - 
;;               step(s,x-w*.5);
;;     return clamp(d, 0., 1.);
;; }
(defun-g gstroke ((x :float) (s :float) (w :float))
  (let* ((d (- (step s (+ x (* .5 w)))
               (step s (- x (* .5 w))))))
    (clamp d 0.0 1.0))
  )

(defun-g grotate ((st :vec2) (a :float))
  (let* ((st (* (mat2 (cos a)
                   (* -1 (sin a))
                   (sin a)
                   (cos a))
                (- st (v2! .5)))))
    (+ (v2! .5) st)))


(defun-g aastep ((threshold :float) (value :float))
  (let* ((afwidth (* 0.7 (length (v! (dFdx value) (dFdy value))))))
    (smoothstep (- threshold afwidth) (+ threshold afwidth) value)))

(defun-g aastep ((threshold :float) (value :float))
  (step threshold value))

(defun-g gfill ((x :float) (size :float))
  (- 1.0 (aastep size x)))

(defun-g rectSDF ((st :vec2) (s :vec2))
  (let* ((st (- (* st (v2! 2.0)) (v2! 1.0))))
    (max (abs (/ (x st) (x s)))
         (abs (/ (y st) (y s))))))

; Title: The stone
(defun-g draw-verts-frag-stage (&uniform (resolution :vec2))
  (let* ((st     (v! (/ (x gl-frag-coord) (x resolution))
                     (/ (y gl-frag-coord) (y resolution))))
         (st     (grotate st (radians 45.)))
         (color  (gfill (rectSDF st (v2! 1.)) .4))
         (color  (* color (- 1.0 (gstroke (x st) .5 .02))))
         (color  (* color (- 1.0 (gstroke (y st) .5 .02)))))
    (v! color color color 1.0)
         )
  )

; Title: Temperance
(defun-g draw-verts-frag-stage (&uniform (resolution :vec2))
  (let* ((st     (v! (/ (x gl-frag-coord) (x resolution))
                     (/ (y gl-frag-coord) (y resolution))))
         (offset (* .15 (cos (* 3.1415 (y st)))))
         (color  (+  (v3! (gstroke (x st) (+ .28 offset) .1))
                     (v3! (gstroke (x st) (+ .5  offset) .1))
                     (v3! (gstroke (x st) (+ .72 offset) .1)))))
    (v! color 1.0)
  ))



(defun-g draw-verts-frag-stage (&uniform (resolution :vec2))
  (let* ((st    (v! (/ (x gl-frag-coord) (x resolution))
                    (/ (y gl-frag-coord) (y resolution))))
         (color (v! (step .5 (x st))
                    (step .5 (x st))
                    (step .5 (x st)))))
    (v! color 1.0)
  ))

;; // Number: VIII
;; // Title: Strength
;; // Author: Patricio Gonzalez Vivo
;; uniform vec2 u_resolution;
;; void main() {
;;     vec3 color = vec3(0.);
;;     vec2 st = gl_FragCoord.xy/u_resolution;
;;     color += step(.5+cos(st.y*PI)*.25, 
;;                   st.x);    
;;     gl_FragColor = vec4(color,1.);
;; }
(defun-g draw-verts-frag-stage (&uniform (resolution :vec2))
  (let* ((st    (v! (/ (x gl-frag-coord) (x resolution))
                    (/ (y gl-frag-coord) (y resolution))))
         (color (v! (step (+ .5 (* .25 (cos (* (y st) 3.14159))) ) (x st))
                    (step (+ .5 (* .25 (cos (* (y st) 3.14159))) ) (x st))
                    (step (+ .5 (* .25 (cos (* (y st) 3.14159))) ) (x st)))))
    (v! color 1.0)
  ))

;; // Number: XIII
;; // Title: Death
;; // Author: Patricio Gonzalez Vivo
;; uniform vec2 u_resolution;
;; void main() {
;;     vec3 color = vec3(0.);
;;     vec2 st = gl_FragCoord.xy/u_resolution;
;;     color += step(.5,(st.x+st.y)*.5);
;;     gl_FragColor = vec4(color,1.);
;; }
(defun-g draw-verts-frag-stage (&uniform (resolution :vec2))
  (let* ((st    (v! (/ (x gl-frag-coord) (x resolution))
                    (/ (y gl-frag-coord) (y resolution))))
         (color (v! (step .5 (* .5 (+ (y st) (x st))))
                             (step .5 (* .5 (+ (y st) (x st))))
                             (step .5 (* .5 (+ (y st) (x st))))
                             )))
    (v! color 1.0)
  ))

;; // Title: The Wall
;; // Author: Patricio Gonzalez Vivo
;; uniform vec2 u_resolution;
;; void main() {
;;     vec3 color = vec3(0.);
;;     vec2 st = gl_FragCoord.xy/u_resolution;
;;     color += stroke(st.x, .5, .15);
;;     gl_FragColor = vec4(color,1.);
;; }
(defun-g draw-verts-frag-stage (&uniform (resolution :vec2)
                                         (step :float)
                                         (other :float)
                                         (time :float))
  (let* ((st    (v! (/ (x gl-frag-coord) (x resolution))
                    (/ (y gl-frag-coord) (y resolution))))
         (color (v! (gstroke (x st) step .15)
                    (gstroke (x st) (sin (+ step time)) .1)
                    (gstroke (x st) (cos other) .1))))
    (v! color 1.0)
  ))

(defpipeline-g draw-verts-pipeline ()
  :vertex   (draw-verts-vert-stage :vec2)
  :fragment (draw-verts-frag-stage))

(defun now ()
  (* .001 (get-internal-real-time)))

(defun draw! ()
   (step-host)
   (setf (resolution (current-viewport))
         (surface-resolution (current-surface (cepl-context))))
   (clear)
   (map-g #'draw-verts-pipeline *vert-stream*
          :step (cm:interp *current-note* 30 0.0 90 1.0)
          :other (cm:interp *other-note* 30  0.0 90 1.0)
          :time (now)
          :resolution (viewport-resolution (current-viewport)))
   (swap))

(defparameter *current-note* 40)
(defparameter *other-note* 40)

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
