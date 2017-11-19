;;;; somecepl.lisp

(in-package #:somecepl)

;;; "somecepl" goes here. Hacks and glory await!

(defvar *gpu-verts-arr* nil)
(defvar *gpu-index-arr* nil)
(defvar *vert-stream* nil)
;(defvar *viewport* nil)

;vec2 random(vec2 st){
;  st = vec2( dot(st, vec2(127.1,311.7)),
;             dot(st, vec2(269.5,183.3)) );
;  return -1.0 + 2.0 * fract(sin(st)*43758.5453123);
;}
(defun-g grandom ((st :vec2))
  (let* ((s (v! (dot st (v! 127.1 311.7))
                (dot st (v! 269.5 183.3)) )))
    (- (* 2.0 (fract (* (sin s) 43758.5453123))) (v! 1.0 1.0) )))

;// Value Noise by Inigo Quilez - iq/2013
;// https://www.shadertoy.com/view/lsf3WH
;float noise(vec2 st) {
;  vec2 i = floor(st);
;  vec2 f = fract(st);
;  vec2 u = f*f*(3.0-2.0*f);
;  return mix( mix( dot( random(i + vec2(0.0,0.0) ), f - vec2(0.0,0.0) ), 
;                   dot( random(i + vec2(1.0,0.0) ), f - vec2(1.0,0.0) ), u.x),
;              mix( dot( random(i + vec2(0.0,1.0) ), f - vec2(0.0,1.0) ), 
;                   dot( random(i + vec2(1.0,1.0) ), f - vec2(1.0,1.0) ), u.x), u.y);
;}
(defun-g gnoise ((st :vec2))
  (let* ((i (floor st))
         (f (fract st))
         (u (* f (* f (- (v! 3.0 3.0)  (* 2.0 f) )))))
    (mix (mix (dot (grandom (+ i (v! 0.0 0.0)))
                   (-          f (v! 0.0 0.0)))
              (dot (grandom (+ i (v! 1.0 0.0)))
                   (-          f (v! 1.0 0.0)))
              (x u))
         (mix (dot (grandom (+ i (v! 0.0 1.0)))
                   (-          f (v! 0.0 1.0)))
              (dot (grandom (+ i (v! 1.0 1.0)))
                   (-          f (v! 1.0 1.0)))
              (x u))
         (y u))
  ))

(defun-g draw-verts-vert-stage ((vert :vec2))
  (v! vert 0 1))

;; uniform vec2 resolution;
;; uniform float time;
;; uniform float val1;
;; uniform float val2;
;; uniform float val3;
;; uniform float subD;
;; uniform float mouseX;
;; uniform float mode;

;; vec2 st = gl_FragCoord.xy/resolution.xy;
;; float tt = -time*0.0005;
;; float val1s = val1*.5; //frequencie. 0.2
;; float val2s = val2*10.;//amplitude   5.0
;; float val3s = val3*5.;//contrast     2.5
;; float subDs = subD*10.;            100.0
;; st*=subDs;
;; float nV = (noise(vec2( st.s + sin(st.s*val1s*5.)*(val2s) , st.t + sin(st.t*val1s*5.)*(val2s) +tt)));
;; nV += .5;
;; nV = ((nV-.5)*val3s )+.5;
;; vec4 col = vec4(vec3(nV),1. );
;; gl_FragColor = col;

(defun-g draw-verts-frag-stage (&uniform (time :float)
                                         (mouse :vec2)
                                         (screen-res :vec2)
                                         (mouse-buttons :vec2))
  (let* ((u1 (* .5   0.5))
         (u2 (* .5   10.0))
         (u3 (* .5   5.0))
         (u4 (* 5.0))
         (tt (* 1.0 time))
         (st (* u4 (v! (/ (x gl-frag-coord) (x screen-res))
                       (/ (y gl-frag-coord) (y screen-res)) )))
         (nv (+ .5 (gnoise (v! (+ (x st) (* 5.0 (sin (* 5.0 0.2 (x st)))))
                               (+ (y st) (* 5.0 (sin (* 5.0 0.2 (y st)))) tt)))))
         (nvv (+ .5 (* 2.5 (- nv .5)))))
    (v! nvv nvv nvv 1.0)
  ))


;; (defun-g draw-verts-frag-stage (&uniform (time :float)
;;                                          (mouse :vec2)
;;                                          (screen-res :vec2)
;;                                          (mouse-buttons :vec2))
;;   (v! (abs (sin time))
;;       (/ (x mouse) (x screen-res))
;;       (/ (x gl-frag-coord) (x screen-res) )
;;       0))

(defpipeline-g draw-verts-pipeline ()
  :vertex   (draw-verts-vert-stage :vec2)
  :fragment (draw-verts-frag-stage))

(defun get-mouse-buttons ()
  (v! (if (mouse-down-p mouse.left) 1f0 0f0)
      (if (mouse-down-p mouse.right) 1f0 0f0)))

(defun get-mouse ()
  (let ((pos (mouse-pos (mouse 0)))
        (h (y (viewport-resolution (current-viewport)))))
    (v! (x pos) (- h (y pos)))))

(defun now ()
  (get-internal-real-time))

(defun draw! ()
;;  (with-viewport *viewport*
   (step-host)
   (setf (resolution (current-viewport))
         (surface-resolution (current-surface *cepl-context*)))
   (clear)
   (map-g #'draw-verts-pipeline *vert-stream*
          :time (* (now) .0001 )
          :mouse (get-mouse)
          :screen-res (viewport-resolution (current-viewport))
          :mouse-buttons (get-mouse-buttons))
   (swap))

(defun init ()

;  (when *viewport*
;    (free *viewport*))
  (when *gpu-verts-arr*
    (free *gpu-verts-arr*))
  (when *gpu-index-arr*
    (free *gpu-index-arr*))
  (when *vert-stream*
    (free *vert-stream*))

;  (setf *viewport*
;    (make-viewport '(400 400)))
  
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
