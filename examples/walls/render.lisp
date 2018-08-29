(in-package :shiny)

(defun-g g-rand ((x :float))
  (fract (* (sin x)
            10000f0)))

(defun-g cyn ((x :float))
  (+ .5 (* .5 (cos x))))

(defun-g syn ((x :float))
  (+ .5 (* .5 (sin x))))


(defun-g shadow-factor ((light-sampler :sampler-2d)
                        (pos-in-light-space :vec4))
  (let* ((proj-coords (/ (s~ pos-in-light-space :xyz)
                         (w pos-in-light-space)))
         (proj-coords (+ (* proj-coords 0.5) (vec3 0.5)))
         (our-depth (z proj-coords))
         (shadow 0f0)
         (bias 0.005)
         (texel-size (/ (vec2 1f0) (texture-size light-sampler 0)))
         (uv (s~ proj-coords :xy)))

    (for (x -1) (<= x 1) (++ x)
         (for (y -1) (<= y 1) (++ y)
              (let* ((uv+offset (+ uv (* (v! x y) texel-size)))
                     (pcf-depth (x (texture light-sampler uv+offset))))
                (incf shadow (if (> (- our-depth bias) pcf-depth) 0f0 1f0)))))

    (/ shadow 9f0)))

;; https://learnopengl.com/Advanced-OpenGL/Depth-testing
(defun-g linearize-depth ((depth :float) (near :float) (far :float))
  (let ((z (- (* depth 2f0) 1f0))) ;; to NDC
    (/ (* 2f0 near far)
       (+ far near
          (- (* z (- far near)))))))

(defun-g circle-g ((uv :vec2) (size :float))
  (v3! (* size
          (distance (v! .5 .5)
                    uv))))
;;--------------------------------------------------

(defun-g vert
    ((vert g-pnt) &uniform
     (model-world :mat4) (world-view :mat4) (view-clip :mat4)
     (time :float))
  (let* ((pos       (pos vert))
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

;;--------------------------------------------------

(defun-g sphere-frag
    ((uv :vec2) (frag-norm :vec3) (frag-pos :vec3) &uniform
     (time :float) (light-factor :float))
  (let* (
         ;; (vec-to-light (- (v! 0 0 0)
         ;;                    frag-pos))
         ;;         (dir-to-light (normalize vec-to-light))
         (light-color (v! .2 .9 .09)) ;; .9 .1 0 ;; .2 .9 .1
         (light-size (+ ;;(/ 90 light-factor)
                        (+ 30 (* .01 (syn time)))))
         (circle (circle-g uv light-size))
         (sky (- 1 (* circle light-color)))
         (noise (+  -3 (perlin-noise (* 40 (+ (v! (* .009 time) 0) uv)))))
         (landmass (* (v! .2 .2 .2) ;; 0 .2 .9 ;; .2 .2 .2
                      (v3! noise)
                      (clamp (v3! (- (y frag-pos))) 0 1.2)))
         (starry (let ((r (g-rand (* 100 (y uv)))))
                   (if (> r .9991)
                       (v3! r)
                       (v3! 0))))
         (starry (clamp (* starry (y frag-pos)) 0 1))
         (final (+ (* .9 landmass)
                   (clamp sky 0 1)
                   (* 2 (+ .2 (* .05 (sin (* 10 time)))) starry)))
;;         (final (+ (* 1 (dot dir-to-light frag-norm)) (s~ final :xyz)))
         )
    final))
;;--------------------------------------------------

(defun-g voz-vert
    ((vert g-pnt) &uniform
     (model-world :mat4) (world-view :mat4) (view-clip :mat4)
     (time :float) (lead-pos :vec3))
  (let* ((pos       (pos vert))
         (time      (+ time gl-instance-id))
         (norm      (norm vert))
         (tex       (tex  vert))
         (norm      (* (m4:to-mat3 model-world) (norm vert)))

         (world-pos (* model-world (v! pos 1)))
         (vec-to-lead   (- lead-pos (s~ world-pos :xyz)))
         (dic-to-lead (normalize vec-to-lead))
         (new-pos (+ lead-pos
                     (v! (* 70  (cos time) (sin time))
                         (* 20  (cos time))
                         (+ -50 (* 20 (cos time))))))
         (world-pos (+ world-pos
                       (v! new-pos 0)))
;;         (world-pos (* world-pos (m3:point-at (v! 1 1 1) new-pos lead-pos)))
         
         (view-pos  (* world-view  world-pos))
         (clip-pos  (* view-clip   view-pos)))
    (values clip-pos
            tex
            norm
            (s~ world-pos :xyz))))

(defun-g voz-frag
    ((uv :vec2) (frag-norm :vec3) (frag-pos :vec3) &uniform
     (time :float) (light-factor :float) (cam-pos :vec3) (light-tex :sampler-2d))
  (let* ((light-pos (v! 0 70 -150))
         (obj-color (v! .2 .9 1));; .2 .9 1  ;; celeste
         ;; Ambient
         (light-ambient .1)
         ;; Diffuse
         (vec-to-light  (- light-pos frag-pos))
         (dir-to-light  (normalize vec-to-light))
         (light-diffuse (saturate (dot frag-norm dir-to-light)))
         (shadow (shadow-factor light-tex (v! light-pos 1)))
         (lights (+ light-ambient (* shadow (+ light-diffuse))))
         (result (* lights obj-color))
         (depth (v3! (/ (linearize-depth (z gl-frag-coord) .1f0 100f0) 100f0)))
         (result (* ;;depth
                    result)))
    (v! result 0)))

;;--------------------------------------------------

(defun-g ground-vert
    ((vert g-pnt) &uniform
     (model-world :mat4) (world-view :mat4) (view-clip :mat4)
     (time :float) (tex-noise :sampler-2d))
  (let* ((pos       (pos vert))
         (norm      (norm vert))
         (tex       (tex vert))
         (dis (* 20
                 (x
                  (texel-fetch
                   tex-noise
                   (ivec2 (v! (int (floor (* 200 (/ (+ 50 (x pos)) 100))))
                              (int (floor (* 200 (/ (+ 50 (z pos)) 100))))))
                   0))))
         (pos (+ pos (v! 0 dis 0)))
;;         (pos (- pos (v! 0 10 0)))
         (pos (* pos (v! 3 3 3)))
         (norm      (* (m4:to-mat3 model-world) (norm vert)))
         (world-pos (* model-world (v! pos 1)))
         (view-pos  (* world-view  world-pos))
         (clip-pos  (* view-clip   view-pos)))
    (values clip-pos
            tex
            norm
            (s~ world-pos :xyz))))

(defun-g ground-frag
    ((uv :vec2) (frag-norm :vec3) (frag-pos :vec3) &uniform
     (time :float))
  (let* ((color (v3! 1)))
    color))

;;--------------------------------------------------

(defun-g pass-vert ((pos :vec2))
  (values (v! pos 0 1)
          (+ .5 (* .5 pos))))
(defun-g pass-frag ((uv :vec2) &uniform (time :float))
  (let ((color
         (v3! (nineveh.noise:cellular-noise
               (+ (- (v! 0 (+ (- (clamp (sin time) .1 .16)) time)))
                  (* 5 uv))))))
    (v! color 0)))
(defpipeline-g pass-pipe ()
  (pass-vert :vec2)
  (pass-frag :vec2))

;;--------------------------------------------------
;; Deform vertex logic, voz-shading
(defpipeline-g ground-pipe ()
  :vertex (ground-vert g-pnt)
  :fragment (voz-frag :vec2 :vec3 :vec3))

;; No verted logic, voz shading
(defpipeline-g lead ()
  :vertex (vert g-pnt)
  :fragment (voz-frag :vec2 :vec3 :vec3))

;; Vertex movement, voz shading
(defpipeline-g pipe ()
  :vertex (voz-vert g-pnt)
  :fragment (voz-frag :vec2 :vec3 :vec3))

;; No vextex logic, sphere shading
(defpipeline-g white ()
  :vertex (vert g-pnt)
  :fragment (sphere-frag :vec2 :vec3 :vec3))
