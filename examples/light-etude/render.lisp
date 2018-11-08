(in-package :shiny)

(defstruct-g assimp-mesh
  (pos :vec3)
  (normal :vec3)
  (uv :vec2)
  (tangent :vec3)
  (bitangent :vec3))

;; https://learnopengl.com/Advanced-OpenGL/Depth-testing
(defun-g linearize-depth ((depth :float) (near :float) (far :float))
  (let ((z (- (* depth 2f0) 1f0))) ;; to NDC
    (/ (* 2f0 near far)
       (+ far near
          (- (* z (- far near)))))))

;;--------------------------------------------------

(defun-g vert
    ((vert g-pnt) &uniform
     (model-world :mat4) (world-view :mat4) (view-clip :mat4) (scale :float))
  (let* ((pos       (* scale (pos vert)))
         (norm      (norm vert))
         (tex       (tex vert))
         (norm      (* (m4:to-mat3 model-world) (norm vert)))
         (world-pos (* model-world (v! pos 1)))
         (view-pos  (* world-view  world-pos))
         (clip-pos  (* view-clip   view-pos)))
    (values clip-pos
            tex
            norm
            (s~ world-pos :xyz))))

;; http://wiki.ogre3d.org/tiki-index.php?page=-Point+Light+Attenuation
(defun-g frag
    ((uv :vec2) (frag-norm :vec3) (frag-pos :vec3) &uniform
     (time :float) (color :vec3) (cam-pos :vec3))
  (let* (;; (light-pos (v! -2
         ;;                2
         ;;                ;;(* 4 (+ .5 (* .5 (sin time))))
         ;;                2))
         (light-pos (v! 0
                        4
                        0))
         (light-color (v! 1 .7 .4))
         (light-strength 1f0)         
         ;;--------------------
         (vec-to-light (- light-pos frag-pos))
         (dir-to-light (normalize vec-to-light))
         ;;--------------------
         (direc-light-strength
          (* .2f0
             light-color
             (saturate
              (dot frag-norm
                   (normalize (- (v! 40 30 40) frag-pos))))))
         (point-light-strength
          (* light-color
             light-strength
             ;; saturate?
             (saturate (dot frag-norm dir-to-light))
             ;; attenuation - constant, linear, cuadratic
             (/ 1f0 (+ 1f0
                       (* .014 (length vec-to-light))
                       (* .07 (pow (length vec-to-light)
                                    2))))))
         (final-color (+ (* color point-light-strength)
                         (* color direc-light-strength)))
         (dist (/ (z gl-frag-coord) (w gl-frag-coord)) )
         ;;(fog (clamp (/ (- 80 dist) (- 80 20)) 0f0 1f0))
         ;;(fog (clamp (/ 1f0 (exp (* dist .05))) 0 1))
         ;;(fog (clamp (/ 1f0 (exp (* (* dist .05) (* dist .05)))) 0 1))
         )
    (values
     (v! final-color 1)
     (v! 0 0 0 1))))

;;--------------------------------------------------

(defun-g frag-tex
    ((uv :vec2) (frag-norm :vec3) (frag-pos :vec3) &uniform
     (time :float) (color :vec3) (tex :sampler-2d))
  (let* ((color (texture tex uv)))
    color))

;;--------------------------------------------------

(defun-g vert-2d ((vert :vec2))
  (let* ((uv  (+ .5 (* .5 vert)))
         ;;(uvs (nineveh.anti-aliasing:fxaa2-calc-uvs uv (v2! (/ 1 320f0))))
         )
    (values (v! vert 0 1)
            uv
            (v! 1 1 1 1)
           ;; uvs
            )))

(defparameter *exposure* .6f0)
(defun-g frag-2d ((uv :vec2) &uniform (sam :sampler-2d))
  (let* ((color (s~ (texture sam uv) :xyz))
         ;;(color (s~ (nineveh.anti-aliasing:fxaa3 uv sam (v2! (/ 1 320f0)))
         ;;            :xyz))
         (ldr (nineveh.tonemapping:tone-map-reinhard color *exposure*))
         ;;(luma (rgb->luma-bt601 ldr))
         )
    (v! (pow ldr (vec3 2.2)) 1)
    
    ;;(v! ldr luma)
    ;;(v! color 1)
    ))
;;--------------------------------------------------

(defpipeline-g generic-2d-pipe (:points)
  :fragment (frag-2d :vec2))

;; No vextex logic, sphere shading
(defpipeline-g generic-pipe ()
  :vertex (vert g-pnt)
  :fragment (frag :vec2 :vec3 :vec3))

(defpipeline-g generic-tex-pipe ()
  :vertex (vert g-pnt)
  :fragment (frag-tex :vec2 :vec3 :vec3))

;;--------------------------------------------------

(defun-g light-frag
    ((uv :vec2) (frag-norm :vec3) (frag-pos :vec3)
     &uniform (time :float) (color :vec3))
  (values (v! color 1)
          (v! color 1)))

;; 3d - solid color frag
(defpipeline-g light-pipe ()
  :vertex (vert g-pnt)
  :fragment (light-frag :vec2 :vec3 :vec3))

;;--------------------------------------------------

(defun-g sample-box
    ((uv :vec2) (delta :float) (sam :sampler-2d) (x :float) (y :float))
  (let* ((o (* (v! x y x y) (v! (- delta) (- delta) delta delta)))
         (s (+ (texture sam (+ uv (s~ o :xy)))
               (texture sam (+ uv (s~ o :zy)))
               (texture sam (+ uv (s~ o :xw)))
               (texture sam (+ uv (s~ o :zw))))))
    (* s .18)))

(defun-g some-frag
    ((uv :vec2) &uniform (sam :sampler-2d) (x :float) (y :float) (delta :float))
  (let ((color (sample-box uv delta sam x y))
        ;;(color (texture sam uv))
        )
    color))

(defpipeline-g bloom-pipe (:points)
  :fragment (some-frag :vec2))


(defun-g other-frag
    ((uv :vec2)
     &uniform (light-sam :sampler-2d) (sam :sampler-2d)
     (x :float) (y :float) (delta :float))
  (let* ((c (texture light-sam uv))
         (c (v! (+ (s~ (sample-box uv delta sam x y) :xyz)
                   (s~ c :xyz))
                (w c))))
    c))

(defpipeline-g dobloom-pipe (:points)
  :fragment (other-frag :vec2))
