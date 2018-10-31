(in-package :shiny)

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
     (time :float) (color :vec3))
  (let* (;; (light-pos (v! -2
         ;;                2
         ;;                ;;(* 4 (+ .5 (* .5 (sin time))))
         ;;                2))
         (light-pos (v! 0 25 0))
         (light-color (v! 1 1 1))
         (light-strength 10f0)
         (vec-to-light (- light-pos frag-pos))
         (dir-to-light (normalize vec-to-light))
         (point-light-strength
          (* light-color
             light-strength
             ;; saturate?
             (saturate (dot dir-to-light frag-norm))
             ;; attenuation - constant, linear, cuadratic
             (/ 1f0 (+ 1f0
                       (* .14 (length vec-to-light))
                       (* .07 (pow (length vec-to-light)
                                  2.2)))))))
    (v! (* color
           point-light-strength)
        0)))

;;--------------------------------------------------

(defun-g vert-2d ((vert :vec2))
  (let* ((uv  (+ .5 (* .5 vert)))
         (uvs (nineveh.anti-aliasing:fxaa2-calc-uvs uv (v2! (/ 1 320f0))))
         )
    (values (v! vert 0 1)
            uv
            uvs
            )))

(defparameter *exposure* .3f0)
(defun-g frag-2d ((uv :vec2) (uvs :vec4) &uniform (sam :sampler-2d))
  (let* ((color (texture sam uv))
;;         (color (nineveh.anti-aliasing:fxaa2 uvs sam (v2! (/ 1 320f0))))
         (ldr (nineveh.tonemapping:tone-map-reinhard (s~ color :xyz) *exposure*)))
    ;;  (pow ldr (vec3 2.2))
    ldr
    ))
;;--------------------------------------------------

(defpipeline-g generic-2d-pipe ()
  :vertex (vert-2d :vec2)
  :fragment (frag-2d :vec2 :vec4))

;; No vextex logic, sphere shading
(defpipeline-g generic-pipe ()
  :vertex (vert g-pnt)
  :fragment (frag :vec2 :vec3 :vec3))

(defpipeline-g assimp-pipe ()
  :vertex (assimp-vert assimp-mesh)
  :fragment (frag :vec2 :vec3 :vec3))

;;----------------------------------------
(defun-g assimp-norm-frag ()
  (v! 1 1 0 1))

(defun-g assimp-norm-vert ((vert assimp-mesh)
                           &uniform
                           (model->world :mat4)
                           (world->view :mat4)
                           (view->clip :mat4)
                           (scale :float))
  (with-slots (pos normal uv) vert
    (let* ((model-pos (v! (* pos scale) 1))
           (world-pos (* model->world model-pos))
           (world-norm (* (m4:to-mat3 model->world) normal))
           (view-pos (* world->view world-pos))
           (view-norm (* (mat4 (m4:to-mat3 world->view)) (v! world-norm 0)))
           (clip-pos (* view->clip view-pos))
           (clip-norm (* view->clip view-norm)))
      (values clip-pos
              (s~ clip-norm :xyz)))))

(defun-g assimp-norm-geom ((normals (:vec3 3)))
  (declare (output-primitive :kind :line-strip :max-vertices 6))
  (labels ((gen-line ((index :int))
             (let* ((magnitude 2)
                    (p0 (gl-position (aref gl-in index)))
                    (p1 (+ p0 (* (v! (aref normals index) 0) magnitude))))
               (setf gl-position p0)
               (emit-vertex)
               (setf gl-position p1)
               (emit-vertex)
               (end-primitive)
               (values))))
    (gen-line 0)
    (gen-line 1)
    (gen-line 2)
    (values)))


(defpipeline-g assimp-norm-pipeline ()
  :vertex (assimp-norm-vert assimp-mesh)
  :geometry (assimp-norm-geom (:vec3 3))
  :fragment (assimp-norm-frag))

(defun-g assimp-vert
    ((vert assimp-mesh) &uniform
     (model-world :mat4) (world-view :mat4) (view-clip :mat4) (scale :float))
  (with-slots (pos normal uv) vert
      (let* ((pos       (* scale pos))
             (norm      normal)
             (tex       uv)
             (norm      (* (m4:to-mat3 model-world) normal))
             (world-pos (* model-world (v! pos 1)))
             (view-pos  (* world-view  world-pos))
             (clip-pos  (* view-clip   view-pos)))
        (values clip-pos
                tex
                norm
                (s~ world-pos :xyz)))))
