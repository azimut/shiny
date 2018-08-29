;;;; somecepl.lisp

(in-package #:shiny)

;;; "somecepl" goes here. Hacks and glory await!

(defvar *ctex* nil)
(defvar *tex* nil)
(defvar *sam* nil)
(defvar *gpu-verts-arr* nil)
(defvar *gpu-index-arr* nil)
(defvar *vert-stream* nil)
(defvar *viewport* nil)
(setf *viewport* (make-viewport '(400 400)))

;; ------------------------------------------

(defun-g vert ((vert :vec2))
  (v! vert 0 1))

;; ------------------------------------------

#|
void main() {
    vec2 st = gl_FragCoord.xy/u_resolution.xy;
    vec3 color = vec3(0.);
    float total = 4.;
    float col = floor(st.x*total);
    color += float(floor(mod(u_time,total)) == col);
    gl_FragColor = vec4(color,1.);    
}
|#
;; http://thebookofshaders.com/edit.php?log=180220141500
(defun-g frag (&uniform
               (resolution :vec2)
               (time :float))
  (let* ((st (/ (s~ gl-frag-coord :xy)
                resolution))
         (color (v3! 0))
         (total 4.0)
         (col   (floor (* total (x st))))
         (color (+ color
                   (v3!
                    (float
                     (eql
                      (floor (mod time total))
                      col))))))
    (v! color 0)))
#|
http://thebookofshaders.com/edit.php?log=180220141210
void main() {
    vec2 st = gl_FragCoord.xy/u_resolution.xy;
   
    float dt = 2.; 
    float t = step(mod(u_time, dt),.2);
    float t2 = step(mod(u_time-(1.*dt*.1), dt),.2);
    float t3 = step(mod(u_time-(2.*dt*.1), dt),.2);
    float t4 = step(mod(u_time-(3.*dt*.1), dt),.2);

    float c =step(st.x,.25)*t;
    c+= (step(st.x,.5)-step(st.x,.25))*t2;
    c+= (step(st.x,.75)-step(st.x,.5))*t3;
    c+= (step(st.x,1.)-step(st.x,.75))*t4;
    
    gl_FragColor = vec4(vec3(c),1.);
    
}
|#
(defun-g frag (&uniform
               (resolution :vec2)
               (time :float))
  (let* ((st (/ (s~ gl-frag-coord :xy)
                resolution))
         (dt 2.0)
         (t1 (step (mod time dt) .2))
         (t2 (step (mod (- time (* 1. dt .1)) dt) .2))
         (t3 (step (mod (- time (* 2. dt .1)) dt) .2))
         (t4 (step (mod (- time (* 3. dt .1)) dt) .2))
         (c  (* t1 (step (x st) .25)))
         (c  (+ c (* t2 (- (step (x st) .5) (step (x st) .25)))))
         (c  (+ c (* t3 (- (step (x st) .75) (step (x st) .5)))))
         (c  (+ c (* t4 (- (step (x st) 1.) (step (x st) .75))))))
    (v! c c c 0)))

#|
void main() {
    vec2 st = gl_FragCoord.xy/u_resolution.xy;   
    float dt = 2.; 
    float t = step(mod(u_time, dt),.2);
    float t2 = step(mod(u_time-(1.*dt*.1), dt),.2);
    float t3 = step(mod(u_time-(2.*dt*.1), dt),.2);
    float t4 = step(mod(u_time-(3.*dt*.1), dt),.2);
    float c1 =step(st.x,.25)*t;
    float c2 = (step(st.x,.5)-step(st.x,.25))*t2;
    float c3= (step(st.x,.75)-step(st.x,.5))*t3;
    float c4= (step(st.x,1.)-step(st.x,.75))*t4;
    float c = c1*sin(st.x*200.)
        + c2*sin(st.y*300.)
	+ c3*sin((st.x+st.y)*200.)
    	+ c4*sin((st.x-st.y)*200.);
    gl_FragColor = vec4(vec3(c),1.);
}
|#
(defun-g frag (&uniform
             (resolution :vec2)
             (time :float))
  (let* ((st (/ (s~ gl-frag-coord :xy)
                resolution))
         (dt 3.)
         (t1 (step (mod time dt) .2))
         (t2 (step (mod (- time (* 1. dt .1)) dt) .2))
         (t3 (step (mod (- time (* 2. dt .1)) dt) .2))
         (t4 (step (mod (- time (* 3. dt .1)) dt) .2))
         (c1 (* t1 (step (x st) .25)))
         (c2 (* t2 (- (step (x st) .5) (step (x st) .25))))
         (c3 (* t3 (- (step (x st) .75) (step (x st) .5))))
         (c4 (* t4 (- (step (x st) 1.) (step (x st) .75))))
         (c (+ (* c4 (sin (* 200. (x st))))
               (* c3 (sin (* 300. (y st))))
               (* c2 (sin (* 200. (+ (x st) (y st)))))
               (* c1 (sin (* 200. (- (x st) (y st))))))))
    (v! c c c 1)))
#|
void main() {
    vec2 st = gl_FragCoord.xy/u_resolution.xy;
    float dt = 1.; 
    float t = step(mod(u_time, dt),.25);
    float t2 = step(mod(u_time-(1.*dt*.25), dt),.25);
    float t3 = step(mod(u_time-(2.*dt*.25), dt),.25);
    float t4 = step(mod(u_time-(3.*dt*.25), dt),.25);
    vec2 cent = st*.5+.25;
    float d = distance(st, cent);
    float c1 = sin(d*100.)*t;
    float c2 = sin(st.x*200.)*t2;
    float c3 = sin(st.y*100.)*t3;
    float c4= sin((st.x+st.y)*50.)*t4;
    float c = c1
        + c2+c3+c4;
    gl_FragColor = vec4(vec3(c),1.);    
}
|#
(defun-g frag (&uniform
               (time :float)
               (resolution :vec2))
  (let* ((st (/ (s~ gl-frag-coord :xy)
                resolution))
         (dt 1.)
         (t1 (step (mod time dt) .25))
         (t2 (step (mod (- time (* 1. dt .25)) dt) .25))
         (t3 (step (mod (- time (* 2. dt .25)) dt) .25))
         (t4 (step (mod (- time (* 3. dt .25)) dt) .25))
         (cent (+ (v2! .25)
                  (* (v2! .5)
                     st)))
         (d (distance st cent))
         (c1 (* t1 (sin (* 100. d))))
         (c2 (* t2 (sin (* 200. (x st)))))
         (c3 (* t3 (sin (* 100. (y st)))))
         (c4 (* t4 (sin (* 50. (+ (x st) (y st))))))
         (c  (+ c1 c2 c3 c4)))
    (v! c c c 0)))


;; ------------------------------------------

;; formulanimations tutorial ::
;; the principles of painting with maths
;; https://www.youtube.com/watch?v=0ifChJ0nJfM

;; gradiant 1
(defun-g frag (&uniform (time :float) (resolution :vec2))
  (let* ((st (/ (s~ gl-frag-coord :xy)
                resolution))
         (col (v! 1. .4 .1))
         (col (* col (x st))))
    (v! col 0)))

;; gradiant 2
(defun-g frag (&uniform (time :float) (resolution :vec2))
  (let* ((st (/ (s~ gl-frag-coord :xy)
                resolution))
         (col (v! 1. .4 .1))
         (col (* col (/ (x st)
                        (y st)))))
    (v! col 0)))

;; gradiant 3
(defun-g frag (&uniform (time :float) (resolution :vec2))
  (let* ((st (/ (s~ gl-frag-coord :xy)
                resolution))
         (col (v! 1. .4 .1))
         (q   (- st (v! .5 .5)))
         (col (* col (length q))))
    (v! col 0)))

;; shadertone vvv.glsl
(defun-g frag (&uniform (time :float) (resolution :vec2))
  (let* ((uv (/ (s~ gl-frag-coord :xy)
                resolution))
         (A   300)
         (B   300)
         ;; put A, B into 0,1 range
         (a  (+ .5 (/ (- A 300.) 50. 2.)))
         (b  (+ .5 (/ (- B 300.) 100. 2.)))
         ;; make a vertical step for A
         (ac (smoothstep 0. 0.05 (- (x uv) a)))
         ;; make a horizontal step for B
         (bc (smoothstep 0. 0.05 (- (y uv) b)))
         (c  (* (v3! ac) (v! 0. .5 1.)))
         (c  (+ c (* (v3! bc) (v! 1. .5 0)))))
    (v! c 0)))

;; ------------------------------------------





(defun-g frag (&uniform (time :float)
                        (tt :sampler-2d)
                        (resolution :vec2))
  (let* ((st (/ (s~ gl-frag-coord :xy)
                (/ resolution .02)))
         (ttt (texture tt st))
         (col (v! 1. .4 .1))
         (col (* col (x st))))
    (v! (s~ ttt :xxx) 0)))


(defpipeline-g draw-verts-pipeline ()
  :vertex   (vert :vec2)
  :fragment (frag))

;; ------------------------------------------

(defun draw! ()
  (step-host)
  (clear)
  (with-viewport *viewport*
    (map-g #'draw-verts-pipeline *vert-stream*
           :resolution (viewport-resolution
                        (current-viewport))
           :tt *sam*
           :time (mynow)))
  (swap))

(defun runinit ()

  (when *gpu-verts-arr*
    (free *gpu-verts-arr*))

  (when *gpu-index-arr*
    (free *gpu-index-arr*))
  (when *vert-stream*
    (free *vert-stream*))
  (when *tex*
    (free *tex*))
  ;; (setf *tex* (make-texture
  ;;              (loop :for j :below 2 :collect
  ;;                 (loop :for i :below 512 :collect
  ;;                    (random 1.0)))
  ;;              :element-type :float
  ;;              :dimensions '(2 512)))  
  (setf *tex* (make-texture
               (loop :for j :below 512 :collect
                  (loop :for i :below 512 :collect
                     (random 254)))
               :element-type :uint8
               :dimensions '(512 512)))  
  ;; (setf *tex* (make-texture
  ;;              (loop :for j :below 512 :collect
  ;;                 (loop :for i :below 2 :collect
  ;;                    (random 254)))
  ;;              :element-type :uint8
  ;;              :dimensions '(512 2)))  
  ;; (setf *tex* (make-texture
  ;;              (loop :for j :below 2 :collect
  ;;                 (loop :for i :below 512 :collect
  ;;                    (random 1.0)))
  ;;              :element-type :r8
  ;;              :dimensions '(2 512)))  
  (setf *sam* (cepl:sample *tex*))
  
  (setf *gpu-verts-arr*
        (make-gpu-array
         (list (v! -1  1)
               (v! -1 -1)
               (v!  1 -1)
               (v!  1  1))
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
