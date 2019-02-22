(in-package :shiny)

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
              (s~ (* model-world (v! (tb-data-tangent tb) 0))
                  :xyz)))
         (n0 (normalize
              (s~ (* model-world (v! norm 0))
                  :xyz)))
         (t0 (normalize (- t0 (* (dot t0 n0) n0))))
         (b0 (cross n0 t0))
         (tbn (mat3 t0 b0 n0)))
    (values clip-pos
            (treat-uvs uv)
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
         (normal (norm-from-map normap newuv))
         (normal (normalize (* tbn normal))))
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
(defparameter *mess* .0f0)
(defparameter *messx* .0f0)
(defparameter *messy* .0f0)
(defparameter *mul* 0f0)
(defun-g vert
    ((vert g-pnt) &uniform
     (model-world :mat4) (world-view :mat4) (view-clip :mat4)
     (scale :float) (time :float))
  (let* ((pos        (* scale (+ (v! (* *mul* (cos (* *messx* time gl-instance-id)))
                                     (* *mul* (sin (* *messy* time gl-instance-id)))
                                     (* *mul* (sin (* *mess* time gl-instance-id))))
                                 (pos vert))))
         (norm       (norm vert))
         (tex        (tex vert))
         (world-norm (* (m4:to-mat3 model-world) norm))
         (world-pos  (* model-world (v! pos 1)))
         (view-pos   (* world-view  world-pos))
         (clip-pos   (* view-clip   view-pos)))
    (values clip-pos
            tex
            (+ gl-instance-id world-norm)
            (s~ world-pos :xyz))))

;; http://wiki.ogre3d.org/tiki-index.php?page=-Point+Light+Attenuation
(defun-g frag ((uv :vec2)
               (frag-norm :vec3)
               (frag-pos :vec3)
               &uniform
               (time :float)
               (color :vec3)
               (cam-pos :vec3))
  (let* (
         ;;--------------------
         (final-color color)
	 ;; float3 dpdx = ddx(i.worldPos);
	 ;; float3 dpdy = ddy(i.worldPos);
	 ;; i.normal = normalize(cross(dpdy, dpdx));
         (dpdx (d-fdx frag-pos))
         (dpdy (d-fdy frag-pos))
         (frag-norm (normalize (cross dpdy dpdx)))
         (final-color
          (dir-light-apply final-color
                           (v! 20 20 20)
                           (v! 0 1000 1000)
                           frag-pos
                           frag-norm))
         (final-color
          (point-light-apply final-color
                             (v! 10 10 10)
                             *light-pos*
                             frag-pos
                             frag-norm
                             1f0
                             0.014 0.07)))
    (values (v! final-color 1)
            (v! 0 1 0 1))))

;;--------------------------------------------------
;; 2D - Post Processing

(defun-g frag-2d ((uv :vec2)
                  &uniform
                  (sam :sampler-2d)
                  (sam2 :sampler-2d)
                  (samd :sampler-2d))
  (let* (;; (color (defered-fog
         ;;            ;;(v! .5 .6 .7)
         ;;            (v! .18 .17 .15)
         ;;            uv
         ;;          sam
         ;;          samd))
         (final-color (s~ (texture sam uv) :xyz))
;;         (color2 (s~ (texture sam2 uv) :xyz))
         ;; (color
         ;;  (s~ (nineveh.anti-aliasing:fxaa3 uv sam (v2! (/ 1 320f0))) :xyz))
;;         (final-color (+ color color2))
         (ldr (tone-map-reinhard final-color *exposure*))
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
         (color (expt (s~ (texture albedo (* 20 uv)) :xyz)
                      (vec3 2.2)))
         ;;--------------------
         ;;(normal (normalize frag-norm))
         ;;(nfm    (norm-from-map normap uv))
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

(defun-g pbr-frag ((uv :vec2)
                   (frag-norm :vec3)
                   (frag-pos :vec3)
                   (tbn :mat3)
                   (tan-light-pos :vec3)
                   (tan-cam-pos :vec3)
                   (tan-frag-pos :vec3)
                   &uniform
                   (samd :sampler-2d)
                   (uv-repeat :float)
                   (uv-speed :float)
                   (time :float)
                   (color :vec3)
                   ;; Lighting
                   (light-pos :vec3)
                   (cam-pos :vec3)
                   ;; PBR
                   (metallic :float)
                   (albedo :sampler-2d)
                   (ao-map :sampler-2d)
                   (height-map :sampler-2d)
                   (normal-map :sampler-2d)
                   (rough-map :sampler-2d)
                   ;; IBL
                   (brdf-lut :sampler-2d)
                   (prefilter-map :sampler-cube)
                   (irradiance-map :sampler-cube))
  (let* (;; First change UV, then parallax!
         (uv (+ (* uv uv-repeat)
                (v! 0 (* uv-speed time))))
         (uv (parallax-mapping-offset-flipped
              uv
              (normalize (- tan-cam-pos tan-frag-pos))
              height-map
              .03))
         (roughness (x (texture rough-map uv)))
         (ao        (x (texture ao-map uv)))
         (color (* color (expt (s~ (texture albedo uv) :xyz)
                               (vec3 2.2))))
         ;; Normal Mapping
         ;;(normal (normalize frag-norm))
         (normal (norm-from-map normal-map uv))
         (normal (normalize (* tbn normal)))
         ;;----------------------------------------
         ;; PBR
         ;; metallic
         (n normal)
         (v (normalize (- cam-pos frag-pos)))
         (metallic .1)
         (f0 (vec3 .04))
         ;;(f0 color)
         (f0 (mix f0 color metallic))
         ;; pbr - reflectance equation
         (lo (vec3 0f0))
         ;; lights START
         (lo (+ lo (pbr-direct-lum light-pos
                                   frag-pos
                                   v
                                   n
                                   roughness
                                   f0
                                   metallic
                                   color)))
         ;; (lo (+ lo (pbr-point-lum light-pos
         ;;                          frag-pos
         ;;                          v
         ;;                          n
         ;;                          roughness
         ;;                          f0
         ;;                          metallic
         ;;                          color)))
         ;; ---------- END
         ;;(ambient (* color ao (vec3 .03)))
         ;; (ambient (pbr-ambient-map-r irradiance-map
         ;;                             color
         ;;                             ao n v f0
         ;;                             roughness))
         (r (reflect (- v) n))
         (f (fresnel-schlick-roughness (max (dot n v) 0)
                                       f0
                                       roughness))
         (ks f)
         (kd (* (- 1 ks) (- 1 metallic)))
         (irradiance (s~ (texture irradiance-map n) :xyz))
         (diffuse (* irradiance color))
         (prefiltered-color (s~ (texture-lod prefilter-map
                                             r
                                             (* roughness 4f0))
                                :xyz))
         (env-brdf (texture brdf-lut (v! (max (dot n v) 0) (* roughness 4f0))))
         (specular (* prefiltered-color (+ (* f (x env-brdf)) (y env-brdf))))
         (ambient (* (+ specular (* kd diffuse)) ao))

         (final-color (+ ambient lo))
         ;; Fog
         (final-color
          (fog-exp2-apply final-color
                          (v! .18 .17843138 .1552941)
                          ;;(v! .2 .3 .4)
                          frag-pos
                          cam-pos .03))
         )
    
    (v! final-color 1)
    ;;(v! uv 0 1)
    ;;(v! color 1)
    ))

(defpipeline-g pbr-pipe ()
  :vertex (vert-with-tbdata g-pnt tb-data)
  :fragment (pbr-frag :vec2 :vec3 :vec3
                      :mat3 :vec3 :vec3 :vec3))

;;--------------------------------------------------

(defun-g pbr-simple-frag ((uv :vec2)
                          (frag-norm :vec3)
                          (frag-pos :vec3)
                          &uniform
                          (light-pos :vec3)
                          (time :float)
                          (roughness :float)
                          (metallic :float)
                          (color :vec3)
                          (cam-pos :vec3)
                          ;; IBL
                          (irradiance-map :sampler-cube)
                          (prefilter-map :sampler-cube)
                          (brdf-lut :sampler-2d))
  (let* (;; First change UV, then parallax!
         (uv (treat-uvs uv))
         (normal (normalize frag-norm))
         (ao 1f0)
         (color color)
         ;;----------------------------------------
         ;; PBR
         ;; metallic
         ;;(f0 (vec3 .04))
         (f0 color)
         (f0 (mix f0 color metallic))
         ;; pbr - reflectance equation
         (n normal)
         (v (normalize (- cam-pos frag-pos)))
         (lo (vec3 0f0))
         ;; lights START
         (lo (+ lo (pbr-direct-lum light-pos
                                   frag-pos
                                   v
                                   n
                                   roughness
                                   f0
                                   metallic
                                   color)))
         ;; ---------- END
         ;; (r (reflect (- v) n))
         ;; (f (fresnel-schlick-roughness (max (dot n v) 0)
         ;;                               f0
         ;;                               roughness))
         ;; (ks f)
         ;; (kd (* (- 1 ks) (- 1 metallic)))
         ;; (irradiance (s~ (texture irradiance-map n) :xyz))
         ;; (diffuse (* irradiance color))
         ;; (prefiltered-color (s~ (texture-lod prefilter-map
         ;;                                     r
         ;;                                     (* roughness 4f0))
         ;;                        :xyz))
         ;; (env-brdf (texture brdf-lut (v! (max (dot n v) 0) (* roughness 4f0))))
         ;; (specular (* prefiltered-color (+ (* f (x env-brdf)) (y env-brdf))))
         ;; (ambient (* (+ specular (* kd diffuse)) ao))
         ;; (ambient (pbr-ambient-map-r irradiance-map
         ;;                             color
         ;;                             ao n v f0
         ;;                             roughness))
         (ambient (* color ao (vec3 .3)))
         (final-color (+ ambient lo))
         ;; Fog
         ;; (final-color
         ;;  (fog-exp2-apply final-color
         ;;                  (v! 0 0 0)
         ;;                  frag-pos
         ;;                  cam-pos .03))
         )
    (v! final-color 1)))


;;----------------------------------------
;; Functions to apply the Irradiance Map ONLY
(defun-g pbr-ambient-map ((irradiance-map :sampler-cube)
                          (albedo :vec3)
                          (ao :float)
                          (n :vec3)
                          (v :vec3)
                          (f0 :vec3))
  (let* ((ks (fresnel-schlick (max (dot n v) 0) f0))
        (kd (- 1 ks))
        (irradiance (s~ (texture irradiance-map n) :xyz))
        (diffuse (* irradiance albedo)))
    (* diffuse kd ao)))

(defun-g fresnel-schlick-roughness ((cos-theta :float)
                                    (f0 :vec3)
                                    (roughness :float))
  (+ f0
     (* (- (max (vec3 (- 1 roughness))
                f0)
           f0)
        (pow (- 1 cos-theta) 5f0))))

(defun-g pbr-ambient-map-r ((irradiance-map :sampler-cube)
                            (albedo :vec3)
                            (ao :float)
                            (n :vec3)
                            (v :vec3)
                            (f0 :vec3)
                            (roughness :float))
  (let* ((ks (fresnel-schlick-roughness (max (dot n v) 0)
                                        f0
                                        roughness))
        (kd (- 1 ks))
        (irradiance (s~ (texture irradiance-map n) :xyz))
        (diffuse (* irradiance albedo)))
    (* diffuse kd ao)))

(defpipeline-g pbr-simple-pipe ()
  :vertex (vert g-pnt)
  :fragment (pbr-simple-frag :vec2 :vec3 :vec3))



