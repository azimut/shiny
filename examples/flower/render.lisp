(in-package :shiny)

(defparameter *exposure* 1f0)

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
            uv
            norm
            (s~ world-pos :xyz)            
            tbn
            (* tbn light-pos)
            (* tbn cam-pos)
            (* tbn (s~ view-pos :xyz)))))

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
  (let* (
         ;;--------------------
         (final-color color)
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

(defparameter *parallax-scale* .07)
(defun-g pbr-frag ((uv :vec2)
                   (frag-norm :vec3)
                   (frag-pos :vec3)
                   (tbn :mat3)
                   (tan-light-pos :vec3)
                   (tan-cam-pos :vec3)
                   (tan-frag-pos :vec3)
                   &uniform
                   (light-pos :vec3)
                   (time :float)
                   (color-mult :float)
                   (cam-pos :vec3)
                   (ao-map :sampler-2d)
                   (rough-map :sampler-2d)
                   (albedo :sampler-2d)
                   (height-map :sampler-2d)
                   (normal-map :sampler-2d)
                   (uv-repeat :float)
                   (uv-speed :float)
                   (irradiance-map :sampler-cube))
  (let* (;; First change UV, then parallax!
         ;;(uv (treat-uvs uv))
         (uv (+ (* uv uv-repeat)
         ;;       (v! 0 (* uv-speed time))
                ))         
         (uv (parallax-mapping-offset
              uv
              (normalize (- tan-cam-pos tan-frag-pos))
              height-map
              *parallax-scale*))
         (roughness (x (texture rough-map uv)))
         (ao        (x (texture ao-map uv)))         
         (color (* color-mult (expt (s~ (texture albedo uv) :xyz)
                                    (vec3 2.2))))
         ;; Normal Mapping
         (normal (normalize (norm-from-map normal-map uv)))
         (normal (normalize (* tbn normal)))
         ;;----------------------------------------
         ;; PBR
         ;; metallic
         (metallic .1)
         (f0 (vec3 .04))
         ;;(f0 color)
         (f0 (mix f0 color metallic))
         ;; pbr - reflectance equation
         (lo (vec3 0f0))
         (n normal)
         (v (normalize (- cam-pos frag-pos)))
         ;; lights START
         (lo (+ lo (pbr-direct-lum (v! 0 200 -1000)
                                   frag-pos
                                   v
                                   n
                                   roughness
                                   f0
                                   metallic
                                   color)))
         ;; (lo (+ lo (pbr-point-lum (v! (* 5 (sin time))
         ;;                              0
         ;;                              (* 5 (cos time)))
                                  
         ;;                          frag-pos
         ;;                          v
         ;;                          n
         ;;                          roughness
         ;;                          f0
         ;;                          metallic
         ;;                          color)))
         ;; ---------- END
         ;;(ambient (* color ao (vec3 .03)))
         (ambient (pbr-ambient-map-r irradiance-map
                                     color
                                     ao n v f0
                                     roughness))
         (final-color (+ ambient lo
                         ))
         ;; Fog
         (final-color
          (fog-exp2-apply final-color
                          (v! .3 .2 .1)
                          frag-pos
                          cam-pos .02))
         )
    (v! final-color 1)))

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
                          (color-mult :float)
                          (color :vec3)
                          (cam-pos :vec3)
                          (irradiance-map :sampler-cube))
  (let* (;; First change UV, then parallax!
         (uv (treat-uvs uv))
         (normal (normalize frag-norm))
         (roughness 2f0)
         (ao        1f0)
         (color color)
         ;;----------------------------------------
         ;; PBR
         ;; metallic
         (metallic .1)
         (f0 (vec3 .04))
         (f0 color)
         ;;(f0 (mix f0 color metallic))
         ;; pbr - reflectance equation
         (n normal)
         (v (normalize (- cam-pos frag-pos)))
         (lo (vec3 0f0))
         ;; lights START
         (lo (+ lo (pbr-direct-lum (v! 100 200 500)
                                   frag-pos
                                   v
                                   n
                                   roughness
                                   f0
                                   metallic
                                   color)))
         ;; ---------- END
         (ambient (pbr-ambient-map-r irradiance-map
                                     color
                                     ao n v f0
                                     roughness))
         ;;(ambient (* color ao (vec3 .3)))
         (final-color (+ ambient lo))
         ;; Fog
         ;; (final-color
         ;;  (fog-exp2-apply final-color
         ;;                  (v! 0 0 0)
         ;;                  frag-pos
         ;;                  cam-pos .03))
         )
    (v! final-color 1)))

(defun-g fresnel-schlick-roughness ((cos-theta :float)
                                    (f0 :vec3)
                                    (roughness :float))
  (+ f0
     (* (- (max (vec3 (- 1 roughness))
                f0)
           f0)
        (pow (- 1 cos-theta) 5f0))))
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
;;--------------------------------------------------
;; Cubemap
;; Rendering order does not matter
;; Use it with a 1x1x1 box AND
;; depth-test-function #'<=
(defun-g cubemap-vert ((g-pnt g-pnt)
                       &uniform
                       (mod-clip :mat4))
  (let* ((pos3 (pos g-pnt))
         (pos4 (v! pos3 1))
         (cpos4 (* mod-clip pos4)))
    (values (s~ cpos4 :xyww)
            pos3)))

(defun-g cubemap-frag ((tc :vec3)
                       &uniform
                       (tex :sampler-cube))
  (v! (expt (s~ (texture tex tc) :xyz)
            (vec3 2.2))
      1f0))

(defpipeline-g cubemap-pipe ()
  (cubemap-vert g-pnt)
  (cubemap-frag :vec3))

;;----------------------------------------
(defun-g cube-down-frag ((frag-pos :vec3)
                         &uniform
                         (tex :sampler-cube))
  (let* ((normal (normalize frag-pos))
         (irradiance (v! 0 0 0))
         (up (v! 0 1 0))
         (right (cross up normal))
         (up (cross normal right))
         (sample-delta .025)
         (nr-samples 0f0))
    (for
     (phi 0f0) (< phi (* 2 +pi+)) (setf phi (+ phi sample-delta))
     (for
      (theta 0f0) (< theta (* .5 +pi+)) (setf theta (+ theta sample-delta))
      (let* (;; spherical to cartesian (in tangent space)
             (tangent-sample (v! (* (sin theta) (cos phi))
                                 (* (sin theta) (sin phi))
                                 (cos theta)))
             ;; Tangent space to world
             (sample-vec (+ (* right (x tangent-sample))
                            (* up (y tangent-sample))
                            (* normal (z tangent-sample)))))
        (incf irradiance (* (s~ (texture tex sample-vec) :xyz)
                            (cos theta)
                            (sin theta)))
        (incf nr-samples 1f0))))
    (setf irradiance (* +pi+ irradiance (/ 1 nr-samples)))
    (v! irradiance 1)))

(defpipeline-g cube-down-pipe ()
  :vertex (cubemap-vert g-pnt)
  :fragment (cube-down-frag :vec3))

;; This gives us a sample vector somewhat oriented around the expected
;; microsurface's halfway vector based on some input roughness and the
;; low-discrepancy sequence value Xi. Note that Epic Games uses the
;; squared roughness for better visual results as based on Disney's
;; original PBR research.
(defun-g importance-sample-ggx ((xi :vec2)
                                (n :vec3)
                                (roughness :float))
  (let* ((a (* roughness roughness))
         (phi (* 2 +pi+ (x xi)))
         (cos-theta (sqrt (/ (- 1 (y xi))
                             (+ 1 (* (1- (* a a)) (y xi))))))
         (sin-theta (sqrt (- 1 (* cos-theta cos-theta))))
         ;; from spherical coordinates to cartesian coordinates
         (h (v! (* (cos phi) sin-theta)
                (* (sin phi) sin-theta)
                cos-theta))
         ;; from tangent-space vector to world-space sample vector
         (up (if (< (abs (z n)) .999)
                 (v! 0 0 1)
                 (v! 1 0 0)))
         (tangent (normalize (cross up n)))
         (bitangent (cross n tangent))
         (sample-vec (+ (* (x h) tangent)
                        (* (y h) bitangent)
                        (* (z h) n))))
    (normalize sample-vec)))

;;  The GLSL Hammersley function gives us the low-discrepancy sample X
;;  of the total sample set of size Y.
(hammersley-nth-2d )

(nineveh.math-primitives:radical-inverse-vdc )
