(in-package #:shiny)

;; Reference: Cubemap generation mostly from
;; https://learnopengl.com/PBR/IBL/Diffuse-irradiance
(defparameter *saved* nil)
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
;; GI - IBL - Specular prefilter cubemap
;; https://learnopengl.com/PBR/IBL/Specular-IBL
(defgeneric render-to-prefilter-cubemap (camera
                                         src-cubemap
                                         src-cubemap-sample
                                         dst-cubemap))

(defmethod render-to-prefilter-cubemap ((camera pers)
                                        src-cubemap
                                        src-cubemap-sample
                                        dst-cubemap)
  (let ((dst-dimensions (dimensions dst-cubemap))
        (src-dimensions (dimensions src-cubemap)))
    (assert (= 5 (texture-mipmap-levels dst-cubemap)))
    (assert (texture-cubes-p dst-cubemap))
    (assert (texture-cubes-p src-cubemap))
    (assert (eq :RGB16F (texture-element-type dst-cubemap)))
    (assert (= (car dst-dimensions) (cadr dst-dimensions)))
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
             :finally (setf *saved* T)
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
;;--------------------------------------------------
(defgeneric render-to-light-cubemap (camera
                                     src-cubemap
                                     src-cubemap-sample
                                     dst-cubemap))

(defmethod render-to-light-cubemap ((camera pers)
                                    src-cubemap
                                    src-cubemap-sample
                                    dst-cubemap)
  (let ((dst-dimensions (dimensions dst-cubemap))
        (src-dimensions (dimensions src-cubemap)))
    ;; (assert (eq :RGB16F (element-type (texref src-cubemap :cube-face 0)))
    ;;         (src-cubemap)
    ;;         "Provided src-cubemap must be HDR/RGB16F")
    (assert (eq :RGB16F (element-type (texref dst-cubemap :cube-face 0)))
            (dst-cubemap)
            "Provided dst-cubemap must be HDR/RGB16F")
    (assert (= (car src-dimensions) (cadr src-dimensions))
            (src-dimensions)
            "src DIMENSIONS should have an aspect ratio of 1:1")
    (assert (= (car dst-dimensions) (cadr dst-dimensions))
            (dst-dimensions)
            "dst DIMENSIONS should have an aspect ratio of 1:1")
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
         :for i :from 0
         :finally (setf *saved* T)
         :do
         ;; Rotate camera
           (destructuring-bind (up from to) rotation
             (setf (rot camera) (q:look-at up from to)))
         ;; Switch FBO texture for one of the cubemap
           (setf (attachment fbo 0)
                 (texref dst-cubemap :cube-face i))
           (with-setf* ((cull-face) :front
                        (depth-test-function) #'<=)
             (with-fbo-bound (fbo)
               (clear-fbo fbo)
               (map-g #'cube-down-pipe (box)
                      :tex src-cubemap-sample
                      :mod-clip (m4:* (projection camera)
                                      (world->view camera)))))))))
;;--------------------------------------------------
(defgeneric render-to-cubemap (camera dimensions cubemap))
(defmethod render-to-cubemap ((camera pers) dimensions cubemap)
  "Fills the provided CUBEMAP texture with proper images"
  (assert (eq :RGB16F (element-type (texref cubemap :cube-face 0)))
          (cubemap)
          "Provided cubemap must be HDR/RGB16F")
  (assert (and (list dimensions) (= 2 (length dimensions)))
          (dimensions)
          "DIMENSIONS should be a list of 2 numbers")
  (assert (= (car dimensions) (cadr dimensions))
          (dimensions)
          "DIMENSIONS should have an aspect ratio of 1:1")
  (setf (resolution (current-viewport)) (v! dimensions))
  (setf (fov camera) 90f0)
  (with-free*
      ((external-fbo
        (make-fbo
         (list 0 :dimensions dimensions :element-type :rgb16f)
         (list :d :dimensions dimensions)))
       (external-sample (cepl:sample (attachment-tex external-fbo 0)))
       (bs (make-buffer-stream nil :primitive :points))
       (fbo
        (make-fbo
         (list 0 :dimensions dimensions :element-type :rgb16f)))
       (pipeline
        (pipeline-g (:points)
          :fragment
          (lambda-g ((uv :vec2) &uniform (sam :sampler-2d))
            (let* ((color (s~ (texture sam uv) :xyz))
                   ;;(color (v! (x color) .1 (z color)))
                   )
              (v! (pow color
                       (vec3 (/ 1f0 2.2)))
                  1))))))
    (loop
       :for side :in *cubemap-sides*
       :for rotation :in *cubemap-rotations*
       :for i :from 0
       :finally (setf *saved* T)
       :do
       ;; Rotate camera
         (destructuring-bind (up from to) rotation
           (setf (rot camera)
                 (q:look-at up from to)))
       ;; Normal draw - preferably a 16bit fbo to avoid dithering
         (with-fbo-bound (external-fbo)
           (clear-fbo external-fbo)
           (loop :for actor :in *actors* :do
                (draw actor camera)))
       ;; Switch FBO texture for one of the cubemap
         (setf (attachment fbo 0) (texref cubemap :cube-face i))
       ;; Final draw to LDR (? the colors
         (with-fbo-bound (fbo)
           (clear-fbo fbo)
           (map-g pipeline bs
                  :sam external-sample)))))
;;--------------------------------------------------
(defgeneric save-to-cubemap (camera dimensions))
(defmethod save-to-cubemap ((camera pers) dimensions)
  "Save the current scene in *ACTORS* into 6 bmp images
   Sadly the images are saved in a 8bit LDR image"
  (assert (and (list dimensions) (= 2 (length dimensions)))
          (dimensions)
          "DIMENSIONS should be a list of 2 numbers")
  (assert (= (car dimensions) (cadr dimensions))
          (dimensions)
          "DIMENSIONS should have an aspect ratio of 1:1")
  (setf (resolution (current-viewport)) (v! dimensions))
  (setf (fov camera) 90f0)
  (with-free*
      ((external-fbo
        (make-fbo
         (list 0 :dimensions dimensions :element-type :rgb16f)
         (list :d :dimensions dimensions)))
       (external-sample (cepl:sample (attachment-tex external-fbo 0)))
       (bs (make-buffer-stream nil :primitive :points))
       (fbo (make-fbo (list 0 :dimensions dimensions)))
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
       :finally (setf *saved* T)
       :do
       ;; Rotate camera
         (destructuring-bind (up from to) rotation
           (setf (rot camera)
                 (q:look-at up from to)))
       ;; Normal draw - preferably a 16bit fbo to avoid dithering
         (with-fbo-bound (external-fbo)
           (clear-fbo external-fbo)
           (loop :for actor :in *actors* :do
                (draw actor camera)))
       ;; Final draw to LDR (? the colors
         (with-fbo-bound (fbo)
           (clear-fbo fbo)
           (map-g pipeline bs
                  :sam external-sample))
       ;; Read from our created fbo texture
         (dirt:save-as-image
          (attachment-tex fbo 0)
          (asdf:system-relative-pathname
           :shiny
           (concatenate 'string "static/cubemap_" side ".dds"))))))
