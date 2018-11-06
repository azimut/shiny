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
                        3
                        0))
         (light-color (v! 1 1 1))
         (light-strength 1f0)         
         ;;--------------------
         (vec-to-light (- light-pos frag-pos))
         (dir-to-light (normalize vec-to-light))
         ;;--------------------
         (direc-light-strength
          (* .1f0
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
                         (* color direc-light-strength))))
    (v! final-color 1)))

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

(defparameter *exposure* .4f0)
(defun-g frag-2d ((uv :vec2) &uniform (sam :sampler-2d))
  (let* ((color (s~ (texture sam uv) :xyz))
         ;;(color (nineveh.anti-aliasing:fxaa3 uv sam (v2! (/ 1 320f0))))
         (ldr (nineveh.tonemapping:tone-map-reinhard color *exposure*))
         ;;(luma (rgb->luma-bt601 ldr))
         )
        ;;(pow ldr (vec3 2.2)) 
    ;;(v! ldr luma)
    color
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

(defun-g bloom-frag
    ((uv :vec2) (frag-norm :vec3) (frag-pos :vec3) &uniform
     (time :float) (color :vec3))
  (v! color 1))

(defpipeline-g bloom-pipe ()
  :vertex (vert g-pnt)
  :fragment (bloom-frag :vec2 :vec3 :vec3))


