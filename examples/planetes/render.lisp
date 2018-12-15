(in-package :shiny)

(defparameter *exposure* 1f0)
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
(defun-g frag ((uv :vec2)
               (frag-norm :vec3)
               (frag-pos :vec3)
               &uniform
               (time :float)
               (color :vec3)
               (cam-pos :vec3))
  (let* ((light-pos *pointlight-pos*)
         (light-color (v! 1 .7 .4))
         (light-strength 1f0)         
         ;;--------------------
         (vec-to-light (- light-pos frag-pos))
         (dir-to-light (normalize vec-to-light))
         ;;--------------------
         (final-color color)
         (final-color
          (dir-light-apply final-color
                           (v! 10 10 10)
                           (v! 3000 1000 -1000)
                           frag-pos
                           frag-norm))
         ;; (final-color (apply-fog final-color
         ;;                         (v! .5 .6 .7)
         ;;                         (length (- frag-pos cam-pos))
         ;;                         cam-pos
         ;;                         frag-pos))
         ;; AO FAKE
         (final-color (mix final-color
                           (v! 0 0 0)
                           (- 1 (vec3 (y (- frag-pos (v! 0 -2 0)))))))
         ;; (fog (get-exponential-height-fog
         ;;       frag-pos
         ;;       cam-pos
         ;;       (let ((fog-density .05)
         ;;             (fog-height-falloff .5)
         ;;             (fog-height 3)
         ;;             (cos-terminator-angle 10))
         ;;         (v! (* fog-density (exp2 (* (- fog-height-falloff)
         ;;                                     (- (z cam-pos) fog-height))))
         ;;             fog-height-falloff
         ;;             cos-terminator-angle))
         ;;       (v! 200 0 0)))
         ;;(final-color (mix final-color (s~ fog :xyz) (z fog)))
         ;; (final-color
         ;;  (fog-linear-apply
         ;;   final-color
         ;;   (v! .7 .6 .5)
         ;;   frag-pos
         ;;   cam-pos
         ;;   0 1000))
         )
    (values final-color
            (v! 0 1 0 1))))

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
    ;;(v! (- 1 (x color)) 0 0 1)
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

(defun-g light-frag ((uv :vec2)
                     (frag-norm :vec3)
                     (frag-pos :vec3)
                     &uniform
                     (cam-pos :vec3)
                     (color :vec3)
                     (time :float))
  (let* ((color (v! .2 .1 .8))
         ;; (color (smoothstep color
         ;;                    (v! 0 0 0)
         ;;                    (vec3 (+ .4 (* .6 (- 1 (y uv)))))))
         ;;(color (+ color (* .06 (nineveh.noise:perlin-noise frag-pos))))
         )
    (v! color 1)
    ;; (atmosphere (normalize frag-pos)
    ;;           (v! 0 6372000 0)
    ;;           (v! 0 1000 -1000)
    ;;           22f0
    ;;           6371000f0
    ;;           6471000f0
    ;;           (v! .0000055 .000013 .0000224)
    ;;           .000021
    ;;           8000f0
    ;;           1200f0
    ;;           .758)
    ))

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
         ;;(color (fog-exp2-apply color (v! .2 0 .2) frag-pos cam-pos .1))
         (color (apply-fog color
                           (v! .5 .6 .7)
                           (length (- frag-pos cam-pos))
                           cam-pos
                           (normalize (- frag-pos cam-pos)))))    
    (v! color 1)))

(defpipeline-g tex-pipe ()
  :vertex (vert g-pnt)
  :fragment (frag-tex :vec2 :vec3 :vec3))

;;--------------------------------------------------

;; (defun-g pbr-frag ((uv :vec2)
;;                    (frag-norm :vec3)
;;                    (frag-pos :vec3)
;;                    (tbn :mat3)
;;                    (tan-light-pos :vec3)
;;                    (tan-cam-pos :vec3)
;;                    (tan-frag-pos :vec3)                   
;;                    &uniform
;;                    (time :float)
;;                    (cam-pos :vec3)
;;                    (rough-map :sampler-2d)
;;                    (albedo :sampler-2d)
;;                    (normal-map :sampler-2d))
;;   (let* (
;;          ;;(uv (treat-uvs uv))
;;          (color (expt (s~ (texture albedo uv) :xyz) (vec3 2.2)))
;;          ;;(normal (normalize frag-norm))
;;          (normal (normalize (norm-from-map normal-map uv)))
;;          (normal (normalize (* tbn normal)))
;;          (light-pos (v! (* 10 (cos time)) 0 (* 10 (sin time))))
;;          (final-color color)
;;          (final-color
;;           (dir-light-apply final-color (v! 10 10 10) (v! 10 0 0)
;;                            frag-pos frag-norm))
;;          (final-color
;;           (point-light-apply final-color
;;                              (v! 1 1 1)
;;                              light-pos
;;                              frag-pos
;;                              normal
;;                              1 .014 .07))
;;          ;; (final-color
;;          ;;  (fog-exp2-apply final-color
;;          ;;                  (v! .5 .6 .7) frag-pos cam-pos .1))
;;          )
;;     (v! final-color 1)))



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
                   (ao-map :sampler-2d)
                   (rough-map :sampler-2d)
                   (albedo :sampler-2d)
                   (height-map :sampler-2d)
                   (normal-map :sampler-2d))
  (let* ((uv (treat-uvs uv))
         (uv (* uv 2))
         (roughness (x (texture rough-map uv)))
         (ao (x (texture ao-map uv)))         
         (color (expt (s~ (texture albedo uv) :xyz) (vec3 2.2)))
         (color (* color (v! 1 .2 .1)))
         (normal (normalize (norm-from-map normal-map uv)))
         (normal (normalize (* tbn normal)))
         ;; (light-pos (v! (+ -10 (* 20 (+ .5 (* .5 (sin time)))))
         ;;                1
         ;;                (+ -10 (* 20 (+ .5 (* .5 (cos time)))))))
         (light-pos (v! 0 1000 -1000))
         ;; metallic
         (metallic .1f0)
         (f0 (vec3 .04))
         ;;(f0 color)
         (f0 (mix f0 color metallic))
         ;; pbr - reflectance equation
         (lo (vec3 0f0))
         (n normal)
         (v (normalize (- cam-pos frag-pos)))
         (l (normalize (- light-pos frag-pos)))
         (h (normalize (+ v l)))
         (distance (length (- light-pos frag-pos)))
         ;;(attenuation (/ 1 (* distance distance)))
         (attenuation 1f0)
         (radiance (* (v! 5 5 5) attenuation))
         ;; pbr - cook-torrance brdf
         (ndf (distribution-ggx n h roughness))
         (g (geometry-smith n v l roughness))
         (f (fresnel-schlick (max (dot h v) 0) f0))
         ;;
         (ks f)
         (kd (- 1 ks))
         (kd (* kd (- 1 metallic)))
         ;;
         (numerator (* ndf g f))
         (denominator (+ .001
                         (* (max (dot n v) 0)
                            (max (dot n l) 0)
                            4)))
         (specular (/ numerator denominator))
         ;; add to outgoing radiance lo
         (n-dot-l (max (dot n l) 0))
         (lo (* (+ specular (/ (* kd color) 3.141516))
                radiance
                n-dot-l))
         ;;
         (ambient (* color ao (vec3 .03)))
         (final-color (+ ambient lo))
         ;; AO trickery
         ;;(final-color (vec3 (fract (length (- frag-pos (v! 0 0 -10))))))
         (pos (- frag-pos (v! 0 0 -10)))
         (size (/ (v! 3 10 1) 2))
         (d (- (abs pos) size))
         (final-color (mix final-color
                           (v! .001 .001 .001)
                           (- 1 (saturate (vec3 (length (max d 0)))))))
         ;; (final-color
         ;;  (fog-linear-apply
         ;;   final-color
         ;;   (v! .7 .6 .5)
         ;;   frag-pos
         ;;   cam-pos
         ;;   0 1000))
         )
    (values (v! final-color 1)
            (v! 0 1 0 1))))

(defpipeline-g pbr-pipe ()
  :vertex (vert-with-tbdata g-pnt tb-data)
  :fragment (pbr-frag :vec2 :vec3 :vec3
                      :mat3 :vec3 :vec3 :vec3))

;;--------------------------------------------------
(defun-g get-height-fxp ((in-position :vec3)
                         (in-cloud-min-max :vec2))
  (let ((height-fraction (/ (- (z in-position)
                               (x in-cloud-min-max))
                            (- (y in-cloud-min-max)
                               (x in-cloud-min-max)))))
    (saturate height-fraction)))

(defun-g clouds-frag ((uv :vec2)
                      (frag-norm :vec3)
                      (frag-pos :vec3)
                      &uniform
                      (time :float))
  (atmosphere (normalize frag-pos)
              (v! 0 6372000 0)
              (v! 0 -0 -100)
              22f0
              6371000f0
              6471000f0
              (v! .0000055 .000013 .0000224)
              .000021
              8000f0
              1200
              .758))

(defpipeline-g clouds-pipe ()
  :vertex (vert g-pnt)
  :fragment (clouds-frag :vec2 :vec3 :vec3))

;;--------------------------------------------------
;; God
(defpipeline-g god-rays-pipe (:points)
  :fragment (god-rays-frag :vec2))
