(in-package :shiny)

(defparameter *exposure* 3f0)

(defstruct-g assimp-mesh
  (pos :vec3)
  (normal :vec3)
  (uv :vec2)
  (tangent :vec3)
  (bitangent :vec3))

;;--------------------------------------------------
;; (defun-g assimp-norm-geom ((normals (:vec3 3)))
;;   (declare (output-primitive :kind :line-strip :max-vertices 6))
;;   (labels ((gen-line ((index :int))
;;              (let* ((magnitude 2)
;;                     (p0 (gl-position (aref gl-in index)))
;;                     (p1 (+ p0 (* (v! (aref normals index) 0) magnitude))))
;;                (setf gl-position p0)
;;                (emit-vertex)
;;                (setf gl-position p1)
;;                (emit-vertex)
;;                (end-primitive)
;;                (values))))
;;     (gen-line 0)
;;     (gen-line 1)
;;     (gen-line 2)
;;     (values)))

;; (defun-g assimp-norm-vert ((vert g-pnt) (tb tb-data)
;;                            &uniform
;;                            (model-world :mat4)
;;                            (world-view :mat4)
;;                            (view-clip :mat4)
;;                            (scale :float)
;;                            (normal-map :sampler-2d))
;;   (with-slots (tangent bitangent) tb
;;     (with-slots (position texture) vert
;;       (let* ((model-pos (v! (* position scale) 1))
;;              (world-pos (* model-world model-pos))
;;              (view-pos (* world-view world-pos))
;;              (clip-pos (* view-clip view-pos))

;;              (t0 (normalize
;;                   (s~ (* model-world
;;                          (v! tangent 0))
;;                       :xyz)))
;;              ;; (b0 (normalize
;;              ;;      (s~ (* model-world
;;              ;;             (v! bitangent 0))
;;              ;;          :xyz)))
;;              (n0 (normalize
;;                   (s~ (* model-world
;;                          (v! (norm vert) 0))
;;                       :xyz)))
;;              (t0 (normalize (- t0 (* (dot t0 n0) n0))))
;;              (b0 (cross n0 t0))
;;              (tbn (mat3 t0 b0 n0))
;;              (world-norm (* (m4:to-mat3 model-world) (norm vert)))
;;              (norm-from-map (norm-from-map normal-map texture))
;;              (new-world-normal (* tbn norm-from-map))
;;              (view-norm (* (mat4 (m4:to-mat3 world-view))
;;                            (v! new-world-normal 0)))
;;              (clip-norm (* view-clip view-norm)))
;;         (values clip-pos
;;                 (s~ clip-norm :xyz))))))

;; (defun-g assimp-norm-frag ()
;;   (v! 1 1 0 1))

;; (defpipeline-g assimp-norm-pipeline ()
;;   :vertex (assimp-norm-vert g-pnt tb-data)
;;   :geometry (assimp-norm-geom (:vec3 3))
;;   :fragment (assimp-norm-frag))
;;--------------------------------------------------

(defun-g parallax-mapping ((tex-coords :vec2)
                           (view-dir :vec3)
                           (depth-map :sampler-2d)
                           (height-scale :float))
  (let* ((height (x (texture depth-map tex-coords)))
         (p      (* (/ (s~ view-dir :xy) (z view-dir))
                    (* height height-scale))))
    (- tex-coords p)))

;;--------------------------------------------------

(defun-g treat-uvs ((uv :vec2))
  (v! (x uv) (- 1.0 (y uv))))

(defun-g norm-from-map ((normal-map :sampler-2d) (uv :vec2))
  (let* ((norm-from-map (s~ (texture normal-map uv) :xyz))
         (norm-from-map (normalize
                         (- (* norm-from-map 2.0) 1.0))))
    (v! (x norm-from-map)
        (- (y norm-from-map))
        (z norm-from-map))))

(defun-g vert-with-tbdata
    ((vert g-pnt) (tb tb-data)
     &uniform
     (model-world :mat4)
     (world-view :mat4)
     (view-clip :mat4)
     (scale :float)
     ;; Parallax vars
     (light-pos :vec3)
     (cam-pos :vec3))
  (let* ((pos       (* scale (pos vert)))
         (norm      (norm vert))
         (uv        (treat-uvs (tex vert)))
         (norm      (* (m4:to-mat3 model-world) (norm vert)))
         (world-pos (* model-world (v! pos 1)))
         (view-pos  (* world-view  world-pos))
         (clip-pos  (* view-clip   view-pos))
         (t0 (normalize
              (s~ (* model-world
                     (v! (tb-data-tangent tb) 0))
                  :xyz)))
         (n0 (normalize
              (s~ (* model-world
                     (v! norm 0))
                  :xyz)))
         (t0 (normalize (- t0 (* (dot t0 n0) n0))))
         (b0 (cross n0 t0))
         (tbn (mat3 t0 b0 n0)))
    (values clip-pos
            uv
            norm
            (s~ world-pos :xyz)
            tbn
            (* tbn light-pos)
            (* tbn cam-pos)
            (* tbn (s~ world-pos :xyz)))))

(defun-g frag-tex ((uv :vec2)
                   (frag-norm :vec3)
                   (frag-pos :vec3)
                   (tbn :mat3)
                   (tan-light-pos :vec3)
                   (tan-cam-pos :vec3)
                   (tan-frag-pos :vec3)
                   &uniform
                   (cam-pos :vec3)
                   (albedo :sampler-2d)
                   (normap :sampler-2d)
                   (height-map :sampler-2d))
  (let* ((light-pos *pointlight-pos*)
         ;; Parallax
         (tan-cam-dir (- tan-cam-pos tan-frag-pos))
         (newuv (parallax-mapping uv tan-cam-dir height-map .1))
         ;; ---------
         (light-color (v! 1 1 1))
         (light-strength 1f0)         
         ;;--------------------
         (vec-to-light (- light-pos frag-pos))
         (dir-to-light (normalize vec-to-light))
         ;;--------------------

         (color (expt (s~ (texture albedo newuv) :xyz)
                      (vec3 2.2)))
         ;;(normal (normalize frag-norm))
         (nfm    (norm-from-map normap newuv))
         (normal (* tbn nfm))

         
         (direc-light-strength
          (* *dirlight-mul*
             light-color
             (saturate
              (dot normal
                   (normalize (- *dirlight-pos* frag-pos))))))
         (point-light-strength
          (* light-color
             light-strength
             ;; saturate?
             (saturate (dot normal dir-to-light))
             ;; attenuation - constant, linear, cuadratic
             (/ 1f0 (+ 1f0
                       (* .014 (length vec-to-light))
                       (* .07 (pow (length vec-to-light)
                                    2))))))
         (final-color (+  (* color
                             point-light-strength
                             )
                          (* color direc-light-strength)
                         )))
    (values
     (v! final-color 1)
     (v! 0 0 0 1))))

;;--------------------------------------------------

(defun-g vert
    ((vert g-pnt) &uniform
     (model-world :mat4) (world-view :mat4) (view-clip :mat4)
     (scale :float))
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
  (let* ((light-pos *pointlight-pos*)
         (light-color (v! 1 .7 .4))
         (light-strength 1f0)         
         ;;--------------------
         (vec-to-light (- light-pos frag-pos))
         (dir-to-light (normalize vec-to-light))
         ;;--------------------
         (direc-light-strength
          (* *dirlight-mul*
             light-color
             (saturate
              (dot frag-norm
                   (normalize (- *dirlight-pos* frag-pos))))))
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
    (values
     (v! final-color 1)
     (v! 0 0 0 1))))

;;--------------------------------------------------
;; 2D - Post Processing

(defun-g vert-2d ((vert :vec2))
  (let* ((uv  (+ .5 (* .5 vert))))
    (values (v! vert 0 1)
            uv)))

(defun-g frag-2d ((uv :vec2) &uniform (sam :sampler-2d))
  (let* ((color (s~ (texture sam uv) :xyz))
         ;; (color
         ;;  (s~ (nineveh.anti-aliasing:fxaa3 uv sam (v2! (/ 1 320f0))) :xyz))
         (ldr (nineveh.tonemapping:tone-map-reinhard color *exposure*))
         ;;(luma (rgb->luma-bt601 ldr))
         )
    (v! (pow ldr (vec3 2.2)) 1)
    
    ;;(v! ldr luma)
    ;;(v! color 1)
    ))

(defpipeline-g generic-2d-pipe (:points)
  :fragment (frag-2d :vec2))

;;--------------------------------------------------

;; 3D - g-pnt mesh with light shading
(defpipeline-g generic-pipe ()
  :vertex (vert g-pnt)
  :fragment (frag :vec2 :vec3 :vec3))

;; 3D - g-pnt+tb-data with light shading and textures
(defpipeline-g generic-tex-pipe ()
  :vertex (vert-with-tbdata g-pnt tb-data)
  :fragment (frag-tex :vec2 :vec3 :vec3 :mat3
                      ;; Parallax
                      :vec3 :vec3 :vec3))

;;--------------------------------------------------
;; 3D - Lamp solid color

(defun-g light-frag ((uv :vec2) (frag-norm :vec3) (frag-pos :vec3)
                     &uniform (color :vec3))
  (values (v! color 1)
          (v! color 1)))

(defpipeline-g light-pipe ()
  :vertex (vert g-pnt)
  :fragment (light-frag :vec2 :vec3 :vec3))

;;--------------------------------------------------
;; 2D - Blur

(defun-g sample-box
    ((uv :vec2) (delta :float) (sam :sampler-2d) (x :float) (y :float))
  (let* ((o (* (v! x y x y) (v! (- delta) (- delta) delta delta)))
         (s (+ (texture sam (+ uv (s~ o :xy)))
               (texture sam (+ uv (s~ o :zy)))
               (texture sam (+ uv (s~ o :xw)))
               (texture sam (+ uv (s~ o :zw))))))
    (* s .125)))

(defun-g some-frag
    ((uv :vec2) &uniform
     (sam :sampler-2d) (x :float) (y :float) (delta :float))
  (let ((color (sample-box uv delta sam x y)))
    color))

(defpipeline-g bloom-pipe (:points)
  :fragment (some-frag :vec2))

;;----------------------------------------
;; 2D - Bloom

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
