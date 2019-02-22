(in-package :shiny)

(defparameter *exposure* .5f0)

(defun-g treat-uvs ((uv :vec2))
  (v! (x uv) (- 1.0 (y uv))))

;;--------------------------------------------------
;; 3D - g-pnt with tangent info in tb-data AND textures

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
         (norm      (* (m4:to-mat3 model-world) norm))
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

(defun-g frag-tex-tbn ((uv :vec2)
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
         ;; (direc-light-strength
         ;;  (* *dirlight-mul*
         ;;     light-color
         ;;     (saturate
         ;;      (dot normal
         ;;           (normalize (- *dirlight-pos* frag-pos))))))
         ;; (point-light-strength
         ;;  (* light-color
         ;;     light-strength
         ;;     ;; saturate?
         ;;     (saturate (dot normal dir-to-light))
         ;;     ;; attenuation - constant, linear, cuadratic
         ;;     (/ 1f0 (+ 1f0
         ;;               (* .014 (length vec-to-light))
         ;;               (* .07 (pow (length vec-to-light)
         ;;                           2))))))
         ;; (final-color
         ;;  (+ (* color point-light-strength)
         ;;     (* color direc-light-strength)))
         )
    (values
     ;; (v! final-color 1)
     ;; (v! 1 1 1 1)
     ;;frag-pos
     (normalize frag-norm))))

(defpipeline-g generic-tex-pipe ()
  :vertex (vert-with-tbdata g-pnt tb-data)
  :fragment (frag-tex-tbn :vec2 :vec3 :vec3 :mat3
                          ;; Parallax
                          :vec3 :vec3 :vec3))

;;--------------------------------------------------
;; 3D - g-pnt mesh without tangents

(defun-g vert
    ((vert g-pnt) &uniform
     (model-world :mat4) (world-view :mat4) (view-clip :mat4)
     (scale :float))
  (let* ((pos        (* scale (pos vert)))
         (norm       (norm vert))
         (tex        (tex vert))
         (world-norm (* (m4:to-mat3 model-world) norm))
         (world-pos  (* model-world (v! pos 1)))
         (view-pos   (* world-view  world-pos))
         (clip-pos   (* view-clip   view-pos)))
    (values clip-pos
            tex
            world-norm
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
                         (* color direc-light-strength)
                         ))
         (final-color (apply-fog final-color
                                 .02
                                 frag-pos
                                 cam-pos
                                 (v! 0 -100 0))))
    (v! 1 1 1)
;;    (v! color 0)
    ))

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
         (luma (rgb->luma-bt601 ldr))
         )
    ;;(v! (pow ldr (vec3 2.2)) 1)    
    (v! ldr luma)
    ;;(v! color 1)
    ;;(v! ldr 1)
    ))

(defpipeline-g generic-2d-pipe (:points)
  :fragment (frag-2d :vec2))

;;--------------------------------------------------

;; 3D - g-pnt mesh with light shading
(defpipeline-g generic-pipe ()
  :vertex (vert g-pnt)
  :fragment (frag :vec2 :vec3 :vec3))

;;--------------------------------------------------
;; 3D - Lamp solid color

(defun-g light-frag
    ((uv :vec2) (frag-norm :vec3) (frag-pos :vec3)
     &uniform (color :vec3) (time :float))
  (nineveh.noise:perlin-noise
   (+ (* .1 time) frag-pos))
)

(defpipeline-g light-pipe ()
  :vertex (vert g-pnt)
  :fragment (light-frag :vec2 :vec3 :vec3))



(defun-g frag-tex ((uv :vec2)
                   (frag-norm :vec3)
                   (frag-pos :vec3)
                   &uniform
                   (cam-pos :vec3)
                   (albedo :sampler-2d))
  (let* ((light-pos *pointlight-pos*)
         ;; ---------
         (light-color (v! 1 1 1))
         (light-strength 1f0)         
         ;;--------------------
         (vec-to-light (- light-pos frag-pos))
         (dir-to-light (normalize vec-to-light))
         ;;--------------------
         (color (* 10 (s~ (texture albedo (* 20 uv)) :xyz)))

         ;;(color (expt color (vec3 2.2)))
         ;;--------------------
         ;; Fog
         ;;(fog-color (v! 0 1 1))
         (fog-color (* 1 (v! 0.14901961 0.3019608 0.69803923)))
         ;;(fog-factor (fog-linear frag-pos cam-pos .1 30))
         ;;(fog-factor (fog-exp2 frag-pos cam-pos .16))
         ;;(normal (normalize frag-norm))
         ;;(nfm    (norm-from-map normap uv))
         ;; (direc-light-strength
         ;;  (* *dirlight-mul*
         ;;     light-color
         ;;     (saturate
         ;;      (dot normal
         ;;           (normalize (- *dirlight-pos* frag-pos))))))
         ;; (point-light-strength
         ;;  (* light-color
         ;;     light-strength
         ;;     ;; saturate?
         ;;     (saturate (dot normal dir-to-light))
         ;;     ;; attenuation - constant, linear, cuadratic
         ;;     (/ 1f0 (+ 1f0
         ;;               (* .014 (length vec-to-light))
         ;;               (* .07 (pow (length vec-to-light)
         ;;                           2))))))
         ;; (final-color
         ;;  (+ (* color point-light-strength)
         ;;     (* color direc-light-strength)))
         (color (apply-fog color
                           .07
                           frag-pos
                           cam-pos
                           (v! 0 100 0))))    
    (v! color 1)))

(defpipeline-g tex-pipe ()
  :vertex (vert g-pnt)
  :fragment (frag-tex :vec2 :vec3 :vec3))

;;--------------------------------------------------
;; Billboard
;; https://www.youtube.com/watch?v=puOTwCrEm7Q
;; - basis vectors:
;; -- 3 vectors are true orthogonal
;; -- fill space (span)
;;
(defparameter *quad-3d*
  (list '(-1 -1 0) '( 1 -1 0)
        '( 1  1 0) '(-1  1 0)))

(defun-g billboard-vert ((pos :vec3)
                         &uniform
                         (time :float)
                         (world-view :mat4))
  (* world-view (v! .5 0 18.5 1)))

(defun-g billboard-geom (&uniform (camera-pos :vec3)
                                  (view-clip :mat4))
  (declare (output-primitive :kind :triangle-strip :max-vertices 4))
  (let* ((p (s~ (gl-position (aref gl-in 0)) :xyz))
         (to-camera (normalize (- camera-pos p)))
         ;;(to-camera (v! (x to-camera) 0 (z to-camera)))
         (up (v! 0 1 0))
         (right (cross to-camera up)))
    ;;
    (decf p (* .5 right))
    (emit ()
          (* view-clip (v! p 1))
          (v! 0 0))
    ;;
    (incf (y p) 1f0)
    (emit ()
          (* view-clip (v! p 1))
          (v! 0 1))
    ;;
    (decf (y p) 1f0)
    (incf p right)
    (emit ()
          (* view-clip (v! p 1))
          (v! 1 0))
    ;;
    (incf (y p) 1f0)
    (emit ()
          (* view-clip (v! p 1))
          (v! 1 1))
    (end-primitive)
    (values)))

(defun-g billboard-frag ((uv :vec2) &uniform (tex :sampler-2d) (time :float))
  (let* ((color (expt (texture tex (treat-uvs (* (m2:rotation-from-euler (radians (+ 0 (sin (* 5 time))))) uv))) (vec4 2.2) )))
    color))

(defpipeline-g billboard-pipe (:points)
  :vertex (billboard-vert :vec3)
  :geometry (billboard-geom)
  :fragment (billboard-frag :vec2))


;;--------------------------------------------------
(defun-g pbr-frag ((uv :vec2)
                   (frag-norm :vec3)
                   (frag-pos :vec3)
                   (tbn :mat3)
                   (tan-light-pos :vec3)
                   (tan-cam-pos :vec3)
                   (tan-frag-pos :vec3)
                   &uniform
                   (time :float)
                   (cam-pos :vec3)
                   (rough :sampler-2d)
                   (albedo :sampler-2d)
                   (height-map :sampler-2d)
                   (normal-map :sampler-2d)
                   (ao :sampler-2d))
  (let* (;;(n (normalize frag-norm))
         (tan-cam-dir (- tan-cam-pos tan-frag-pos))
         (uv (parallax-mapping uv tan-cam-dir height-map .001))
         (n (* tbn (norm-from-map normal-map uv)))
         (v (normalize (- cam-pos frag-pos)))
         (metallic .001)
         (f0 (vec3 .04))
         (color (pow (s~ (texture albedo uv) :xyz) (vec3  2.2)))
         (ao-color (x (texture ao uv)))
         (f0 (mix f0 color metallic))
         ;; reflectance
         (lo (vec3 0f0))
         ;; calculate per light radiance
         (light-pos (+ cam-pos
                       (v! (* 5 (sin time))
                           0
                           (* 5 (cos time)))))
         (l (normalize (- light-pos frag-pos)))
         (h (normalize (+ v l)))
         (distance (length (- light-pos frag-pos)))
         (attenuation (/ 1 (* distance distance)))
         (radiance (* (v! 1 1 1) attenuation))
         ;; cook-torrance brdf
         (roughness (x (texture rough uv)))
         (ndf (distribution-ggx n h roughness))
         (g   (geometry-smith n v l roughness))
         (f (fresnel-schlick (max (dot h v) 0) f0))
         ;;
         (ks f)
         (kd (* (- (vec3 1) ks)
                (- 1 metallic)))
         (numerator (* ndf g f))
         (denominator (* 4 (max (dot n v) 0) (max (dot n l) 0)))
         (specular (/ numerator (max denominator .001)))
         ;; add to outgoing radiance lo
         (n-dot-l (max (dot n l) 0))
         (lo (* (+ specular (/ (* kd color) 3.141516))
                radiance
                n-dot-l))
         (ambient (* (vec3 .03) color ao-color))
         (final-color (+ ambient lo))
         (final-color (/ final-color (+ color (vec3 1))))
         (final-color (pow final-color (vec3 (/ 1 2.2)))))
    final-color))

(defpipeline-g pbr-pipe ()
  :vertex (vert-with-tbdata g-pnt tb-data)
  :fragment (pbr-frag :vec2 :vec3 :vec3
                      :mat3 :vec3 :vec3 :vec3))
