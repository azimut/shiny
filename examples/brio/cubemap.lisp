(in-package #:shiny)

;; Reference: Cubemap generation mostly from
;; https://learnopengl.com/PBR/IBL/Diffuse-irradiance
(defvar *cubemap-sides* '("left" "right" "bottom" "front" "top" "back"))
(defvar *cubemap-rotations*
  (list
   (list (v! 0 -1  0) (v! 0 0 0) (v!  1  0  0))
   (list (v! 0 -1  0) (v! 0 0 0) (v! -1  0  0))
   (list (v! 0  0 -1) (v! 0 0 0) (v!  0 -1  0))
   (list (v! 0  0  1) (v! 0 0 0) (v!  0  1  0))          
   (list (v! 0 -1  0) (v! 0 0 0) (v!  0  0  1))
   (list (v! 0 -1  0) (v! 0 0 0) (v!  0  0 -1))))

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
  (let* ((color (expt (s~ (texture tex tc) :xyz)
                     (vec3 2.2))))
    (v! color 1f0)))

(defpipeline-g cubemap-pipe ()
  (cubemap-vert g-pnt)
  (cubemap-frag :vec3))

;;--------------------------------------------------
(defun make-render-cubemap (camera &optional (pos (v! 0 0 0)))
  (let ((dst-cubemap (make-texture
                      nil
                      :dimensions '(2048 2048)
                      :cubes t
                      :element-type :rgb16f)))
    (cubemap-render-to-cubemap camera dst-cubemap pos)
    dst-cubemap))
(defun cubemap-render-to-cubemap (camera dst-cubemap &optional (pos (v! 0 0 0)))
  "Fills the provided DST-CUBEMAP texture with render of
   current scene. Dimensions of the DST-CUBEMAP adviced as 2048x2048"
  (assert (eq :RGB16F (element-type (texref dst-cubemap))))
  (assert (texture-cubes-p dst-cubemap))
  (let ((dimensions (dimensions dst-cubemap)))
    (assert (apply #'= dimensions))
    (setf (resolution (current-viewport)) (v! dimensions))
    (setf (fov camera) 90f0)
    (setf (pos camera) pos)
    (with-free*
        ((bs (make-buffer-stream nil :primitive :points))
         (external-fbo
          (make-fbo
           (list 0 :dimensions dimensions :element-type :rgb16f)
           (list :d :dimensions dimensions)))
         (external-sample (cepl:sample (attachment-tex external-fbo 0)))
         (fbo
          (make-fbo
           (list 0 :dimensions dimensions :element-type :rgb16f)))
         (pipeline
          (pipeline-g (:points)
            :fragment
            (lambda-g ((uv :vec2) &uniform (sam :sampler-2d))
              (let ((color (s~ (texture sam uv) :xyz)))
                (v! color 1))))))
      (loop
         :for side :in *cubemap-sides*
         :for rotation :in *cubemap-rotations*
         :for face :from 0
         :do
         ;; Rotate camera
           (destructuring-bind (up from to) rotation
             (setf (rot camera)
                   (q:look-at up from to)))
         ;; Normal draw - preferably a 16bit fbo to avoid dithering
           (with-fbo-bound (external-fbo)
             (clear external-fbo)
             (loop :for actor :in *actors* :do
                  (draw actor camera 1f0)))
         ;; Switch FBO texture for one of the cubemap
           (setf (attachment fbo 0)
                 (texref dst-cubemap :cube-face face))
         ;; Final draw to LDR (? the colors
           (map-g-into fbo pipeline bs
                       :sam external-sample)))))

;;--------------------------------------------------
(defun cubemap-save-to-disk (camera &optional (dim '(250 250)) (pos (v! 0 0 0)) (image-format "bmp"))
  "Save the current scene in *ACTORS* into 6 bmp images.
   Sadly the images are saved in a 8bit LDR images.
   > (cubemap-save-to-disk '(500 500) (v! 0 0 0) \"bmp\""
  (declare (type string image-format))
  (assert (apply #'= dim))  
  (assert (sequence-of-length-p dim 2))
  (assert (sequence-of-length-p pos 3))
  (setf (resolution (current-viewport)) (v! dim))
  (setf (pos camera) pos)
  (setf (fov camera) 90f0)
  (with-free*
      ((bs  (make-buffer-stream nil :primitive :points))
       (external-fbo
        (make-fbo
         (list 0 :dimensions dim :element-type :rgb16f)
         (list :d :dimensions dim)))
       (external-sample
        (cepl:sample (attachment-tex external-fbo 0)))
       (fbo (make-fbo (list 0 :dimensions dim)))
       (pipeline
        (pipeline-g (:points)
          :fragment
          (lambda-g ((uv :vec2) &uniform (sam :sampler-2d))
            (v! (pow (s~ (texture sam uv) :xyz)
                     (vec3 (/ 1f0 2.2)))
                1)))))
    (loop
       :for side :in *cubemap-sides*
       :for rotation :in *cubemap-rotations*
       :do
       ;; Rotate camera
         (destructuring-bind (up from to) rotation
           (setf (rot camera)
                 (q:look-at up from to)))
       ;; Normal draw - preferably a 16bit fbo to avoid dithering
         (with-fbo-bound (external-fbo)
           (clear external-fbo)
           (loop :for actor :in *actors* :do
                (draw actor camera 1f0)))
       ;; Final draw to LDR (? the colors
         (map-g-into fbo pipeline bs
                     :sam external-sample)
       ;; Read from our created fbo texture
         (dirt:save-as-image
          (attachment-tex fbo 0)
          (asdf:system-relative-pathname
           :shiny
           (concatenate 'string
                        "static/cubemap_" side "." image-format))))))

;;----------------------------------------
;; GI - IBL - Irradiance map generator
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

;;--------------------------------------------------
;; GI - IBL - Irradiance light
(defun cubemap-render-to-irradiance-cubemap (camera
                                             src-cubemap
                                             src-cubemap-sample
                                             dst-cubemap)
  "Adviced dimensions of DST-CUBEMAP of 32x32"
  (declare (type texture src-cubemap dst-cubemap))
  (let ((dst-dimensions (dimensions dst-cubemap))
        (src-dimensions (dimensions src-cubemap)))
    (assert (eq :RGB16F (element-type (texref dst-cubemap :cube-face 0))))
    (assert (apply #'= src-dimensions))
    (assert (apply #'= dst-dimensions))
    (setf (resolution (current-viewport)) (v! dst-dimensions))
    (setf (fov camera) 90f0)
    (with-free*
        ((fbo
          (make-fbo
           (list 0 :dimensions dst-dimensions :element-type :rgb16f)
           (list :d :dimensions dst-dimensions))))
      (loop
         :for side :in *cubemap-sides*
         :for rotation :in *cubemap-rotations*
         :for face :from 0
         :do
         ;; Rotate camera
           (destructuring-bind (up from to) rotation
             (setf (rot camera) (q:look-at up from to)))
         ;; Switch FBO texture for one of the cubemap
           (setf (attachment fbo 0)
                 (texref dst-cubemap :cube-face face))
           (with-setf* ((cull-face) :front
                        (depth-test-function) #'<=)
             (with-fbo-bound (fbo)
               (clear fbo)
               (map-g #'cube-down-pipe (box)
                      :tex src-cubemap-sample
                      :mod-clip (m4:* (projection camera)
                                      (world->view camera)))))))))

;;--------------------------------------------------
;; GI - IBL - Prefilter cubemap
;;
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

(defun-g prefilter-frag ((frag-pos :vec3)
                         &uniform
                         (environment-map :sampler-cube)
                         (roughness :float))
  (let* ((n (normalize frag-pos))
         ;; make the simplyfying assumption that V equals R equals the normal 
         (r n)
         (v r)
         (total-weight 0f0)
         (prefiltered-color (vec3 0f0)))
    (dotimes (i 1024)
      (let* ((xi (hammersley-nth-2d 1024 i))
             (h (importance-sample-ggx xi n roughness))
             (l (normalize (- (* 2 h (dot v h)) v)))
             (n-dot-l (max (dot n l) 0f0)))
        (when (> n-dot-l 0)
          (let* ((d (distribution-ggx n h roughness))
                 (n-dot-h (max (dot n h) 0))
                 (h-dot-v (max (dot h v) 0))
                 (pdf (+ .0001 (/ (* d n-dot-h)
                                  (* 4 h-dot-v))))
                 (resolution 512)
                 (sa-texel (/ (* 4 +pi+)
                              (* 6 resolution resolution)))
                 (sa-sample (/ (+ .0001 (* pdf 1024))))
                 (mip-level (if (= 0 roughness)
                                0f0
                                (* .5 (log2 (/ sa-sample sa-texel))))))
            (incf prefiltered-color
                  (* (s~ (texture-lod environment-map l mip-level) :xyz)
                     n-dot-l))
            (incf total-weight n-dot-l)))))
    (divf prefiltered-color (vec3 total-weight))
    (v! prefiltered-color 1)))

(defpipeline-g prefilter-pipe ()
  :vertex (cubemap-vert g-pnt)
  :fragment (prefilter-frag :vec3))

;;--------------------------------------------------
;; GI - IBL - Specular prefilter cubemap
;; https://learnopengl.com/PBR/IBL/Specular-IBL
(defun cubemap-render-to-prefilter-cubemap (camera
                                            src-cubemap
                                            src-cubemap-sample
                                            dst-cubemap)
  "Adviced DST-CUBEMAP dimensions 128x128"
  (let ((dst-dimensions (dimensions dst-cubemap))
        (src-dimensions (dimensions src-cubemap)))
    (assert (= 5 (texture-mipmap-levels dst-cubemap)))
    (assert (texture-cubes-p dst-cubemap))
    (assert (texture-cubes-p src-cubemap))
    (assert (eq :RGB16F (texture-element-type dst-cubemap)))
    (assert (apply #'= dst-dimensions))
    (setf (fov camera) 90f0)    
    (dotimes (mip 5)
      (let* ((mip-width  (floor (* 128 (expt .5 mip))))
             (mip-height mip-width)
             (dimensions (list mip-width mip-height))
             (roughness  (coerce (/ mip (- 5 1)) 'single-float)))
        (setf (resolution (current-viewport)) (v! dimensions))        
        (with-free*
            ((fbo
              (make-fbo
               (list 0 :dimensions dimensions :element-type :rgb16f)
               (list :d :dimensions dimensions))))
          (loop
             :for side :in *cubemap-sides*
             :for rotation :in *cubemap-rotations*
             :for face :from 0
             :do
             ;; Rotate camera
               (destructuring-bind (up from to) rotation
                 (setf (rot camera) (q:look-at up from to)))
             ;; Switch FBO texture for one of the cubemap
               (setf (attachment fbo 0)
                     (texref dst-cubemap :cube-face face :mipmap-level mip))
               (with-setf* ((cull-face) :front
                            (depth-test-function) #'<=)
                 (with-fbo-bound (fbo)
                   (clear-fbo fbo)
                   (map-g #'prefilter-pipe (box)
                          :roughness roughness
                          :environment-map src-cubemap-sample
                          :mod-clip (m4:* (projection camera)
                                          (world->view camera)))))))))))

