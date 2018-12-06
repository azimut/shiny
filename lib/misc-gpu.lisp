(in-package :shiny)

;;--------------------------------------------------
;; https://learnopengl.com/Advanced-Lighting/Normal-Mapping
;; "Pushing pixels" code
(defun-g norm-from-map ((normal-map :sampler-2d)
                        (uv :vec2))
  (let* ((norm-from-map
          (s~ (texture normal-map uv) :xyz))
         (norm-from-map
          (normalize (- (* norm-from-map 2.0) 1.0))))
    (v! (x norm-from-map)
        (- (y norm-from-map))
        (z norm-from-map))))

;;--------------------------------------------------
;; https://learnopengl.com/Advanced-Lighting/Parallax-Mapping
;; vec3 viewDir   = normalize(fs_in.TangentViewPos - fs_in.TangentFragPos);
(defun-g parallax-mapping ((uv :vec2)
                           (view-dir :vec3)
                           (depth-map :sampler-2d)
                           (height-scale :float))
  (let* ((height (x (texture depth-map uv)))
         (p      (* (/ (s~ view-dir :xy) (z view-dir))
                    (* height height-scale))))
    (- uv p)))

;;--------------------------------------------------
;; https://catlikecoding.com/unity/tutorials/rendering/part-14/
;; These ones return a "fog-factor" number based on the DISTANCE

(defun-g fog-linear ((frag-pos :vec3)
                     (cam-pos :vec3)
                     (start :float)
                     (end :float))
  (let* ((view-distance (length (- frag-pos cam-pos)))
         (fog-factor
          (+ (* view-distance (/ -1 (- end start)))
             (/ end (- end start)))))
    fog-factor))

(defun-g fog-exp ((frag-pos :vec3)
                  (cam-pos :vec3)
                  (density :float))
  (let ((view-distance (length (- frag-pos cam-pos))))
    (exp2 (- (* view-distance (/ density (log 2)))))))

(defun-g fog-exp2 ((frag-pos :vec3)
                   (cam-pos :vec3)
                   (density :float))
  (let* ((view-distance (length (- frag-pos cam-pos)))
         (fog-density
          (* (/ density (sqrt (log 2))) view-distance))
         (fog-factor
          (exp2 (- (* fog-density fog-density)))))
    fog-factor))

;;--------------------------------------------------
;; Versions that apply the fog and returns the final color

(defun-g fog-linear-apply ((color :vec3)
                           (fog-color :vec3)
                           (frag-pos :vec3)
                           (cam-pos :vec3)
                           (start :float)
                           (end :float))
  (let* ((view-distance (length (- frag-pos cam-pos)))
         (fog-factor
          (+ (* view-distance (/ -1 (- end start)))
             (/ end (- end start)))))
    (mix fog-color color (saturate fog-factor))))

(defun-g fog-exp-apply ((color :vec3)
                        (fog-color :vec3)
                        (frag-pos :vec3)
                        (cam-pos :vec3)
                        (density :float))
  (let* ((view-distance (length (- frag-pos cam-pos)))
        (fog-factor
         (exp2 (- (* view-distance (/ density (log 2)))))))
    (mix fog-color color (saturate fog-factor))))

(defun-g fog-exp2-apply ((color :vec3)
                         (fog-color :vec3)
                         (frag-pos :vec3)
                         (cam-pos :vec3)
                         (density :float))
  (let* ((view-distance (length (- frag-pos cam-pos)))
         (fog-density
          (* (/ density (sqrt (log 2))) view-distance))
         (fog-factor
          (exp2 (- (* fog-density fog-density)))))
    (mix fog-color color (saturate fog-factor))))

;;--------------------------------------------------
;; http://iquilezles.org/www/articles/fog/fog.htm
;; "For example, the color of the fog can tell us about the strengh of the
;; sun. Even more, if we make the color of the fog not constant but
;; orientation dependant we can introduce an extra level of realism to
;; the image. For example, we can change the typical bluish fog color to
;; something yellowish when the view vector aligns with the sun
;; direction. This gives a very natural light dispersion effect. One
;; would argue that sucha an effect shouldn't be called fog but
;; scattering, and I agree, but in the end of the day one simply has to
;; modify a bit the fog equation to get the effect done."

(defun-g apply-fog ((color :vec3)
                    (density :float)
                    (distance :float) ;; camera to point distance
                    (ray-dir :vec3)   ;; camera to point vector
                    (sun-dir :vec3))  ;; sun light direction
  (let* ((fog-amount (- 1 (exp (* (- distance) density))))
         (sun-amount (max (dot ray-dir sun-dir) 0))
         (fog-color  (mix (v! .5 .6 .7) ;; blueish
                          (v!  1 .9 .7) ;; yellowish
                          (pow sun-amount 8))))
    (mix color fog-color fog-amount)))

;; Modified version, with more generic args (works?)
(defun-g apply-fog ((color :vec3)
                    (density :float)
                    (frag-pos :vec3)
                    (cam-pos :vec3)
                    (sun-pos :vec3))
  (let* ((distance (length (- cam-pos frag-pos)))
         (ray-dir  (normalize (- cam-pos frag-pos)))
         (sun-dir  (normalize (- sun-pos frag-pos)))
         (fog-amount (- 1 (exp (* (- distance) density))))
         (sun-amount (max (dot ray-dir sun-dir) 0))
         (fog-color  (mix (v! .5 .6 .7) ;; blueish
                          (v!  1 .9 .7) ;; yellowish
                          (pow sun-amount 8))))
    (mix color fog-color fog-amount)))
;;--------------------------------------------------
;; PBR - BRDF
;; https://learnopengl.com/PBR/Lighting
(defun-g fresnel-schlick ((cos-theta :float)
                          (f0 :vec3))
  (+ f0
     (* (- 1 f0)
        (pow (- 1 cos-theta) 5))))
(defun-g distribution-ggx ((n :vec3)
                           (h :vec3)
                           (roughness :float))
  (let* ((a  (* roughness roughness))
         (a2 (* a a))
         (n-dot-h (max (dot n h) 0))
         (n-dot-h2 (* n-dot-h n-dot-h))
         (num a2)
         (denom (+ 1 (* n-dot-h2 (- a2 1))))
         (denom (* 3.141516 denom denom)))
    (/ num denom)))
(defun-g geometry-schlick-ggx ((n-dot-v :float)
                               (roughness :float))
  (let* ((r (+ 1 roughness))
         (k (/ (* r r) 8))
         (num n-dot-v)
         (denom (+ (* n-dot-v (- 1 k))
                   k)))
    (/ num denom)))
(defun-g geometry-smith ((n :vec3)
                         (v :vec3)
                         (l :vec3)
                         (roughness :float))
  (let* ((n-dot-v (max (dot n v) 0))
         (n-dot-l (max (dot n l) 0))
         (ggx2 (geometry-schlick-ggx n-dot-v roughness))
         (ggx1 (geometry-schlick-ggx n-dot-l roughness)))
    (* ggx1 ggx2)))
;;--------------------------------------------------
(defun-g billboard-geom (&uniform (camera-pos :vec3)
                                  (view-clip :mat4))
  (declare (output-primitive :kind :triangle-strip :max-vertices 4))
  (let* ((p (s~ (gl-position (aref gl-in 0)) :xyz))
         (to-camera (normalize (- camera-pos p)))
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

