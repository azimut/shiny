(in-package :shiny)

(defvar *fbo* nil)
(defvar *sam* nil)
(defvar *bs* nil)

(defvar *t-cubemap* nil)
(defvar *s-cubemap* nil)

(defvar *t-cubemap-live* nil)
(defvar *s-cubemap-live* nil)

(defparameter *saved* nil)
(defparameter *dimensions* '(1024 1024))

(defun initialize ()
  (unless *t-cubemap-live*
    (setf *t-cubemap-live*
          (make-texture
           nil
           :element-type :rgb8
           :dimensions *dimensions*
           :cubes t))
    (setf *s-cubemap-live*
          (cepl:sample *t-cubemap-live*)))
  (unless *t-cubemap*
    (setf *t-cubemap*
          (make-cubemap-tex
           "static/ThickCloudsWater/left.png"
           "static/ThickCloudsWater/right.png"
           "static/ThickCloudsWater/up.png"
           "static/ThickCloudsWater/down.png"
           "static/ThickCloudsWater/front.png"
           "static/ThickCloudsWater/back.png"))
    (setf *s-cubemap*
          (cepl:sample *t-cubemap*)))
  ;;--------------------------------------------------
  ;; Buffer stream for single stage pipelines  
  (unless *bs* (setf *bs* (make-buffer-stream nil :primitive :points)))
  ;;--------------------------------------------------
  ;; HDR fbo(s)
  (when *fbo* (free *fbo*))
  (setf *fbo*
        (make-fbo
         (list 0 :element-type :rgb16f :dimensions *dimensions*)
         (list :d :dimensions *dimensions*)))
  (setf *sam* (cepl:sample (attachment-tex *fbo* 0)
                           :wrap :clamp-to-edge))
  ;;--------------------------------------------------
  (setf (clear-color) (v! 0 0 0 1))
  ;;--------------------------------------------------
  (setf *actors* nil)
  ;;(make-pbr (v! 0 -2 0))
  (make-cubemap)
  ;;(make-pbr-simple)
  NIL)

(defgeneric render-to-cubemap (camera dimensions cubemap))
(defmethod render-to-cubemap ((camera pers) dimensions cubemap)
  (let ((sides '("left" "right" "bottom" "front" "top" "back"))
        (rotations
         (list
          (list (v! 0 -1  0) (v! 0 0 0) (v!  1  0  0))
          (list (v! 0 -1  0) (v! 0 0 0) (v! -1  0  0))
          (list (v! 0  0 -1) (v! 0 0 0) (v!  0 -1  0))
          (list (v! 0  0  1) (v! 0 0 0) (v!  0  1  0))          
          (list (v! 0 -1  0) (v! 0 0 0) (v!  0  0  1))
          (list (v! 0 -1  0) (v! 0 0 0) (v!  0  0 -1)))))
    (assert (and (list dimensions) (= 2 (length dimensions)))
            (dimensions)
            "DIMENSIONS should be a list of 2 numbers")
    (assert (= (car dimensions) (cadr dimensions))
            (dimensions)
            "DIMENSIONS should have an aspect ratio of 1:1")
    (setf (resolution (current-viewport)) (v! dimensions))
    (setf (fov camera) 90f0)
    (with-free*
        ((external-fbo (make-fbo (list 0 :dimensions dimensions :element-type :rgb16f)
                                 (list :d :dimensions dimensions)))
         (external-sample (cepl:sample (attachment-tex external-fbo 0)))
         (bs (make-buffer-stream nil :primitive :points))
         (fbo (make-fbo (list 0 :dimensions dimensions)))
         (pipeline
          (pipeline-g (:points)
            :fragment
            (lambda-g ((uv :vec2) &uniform (sam :sampler-2d))
              (let* ((color (s~ (texture sam uv) :xyz))
                     (color (v! (x color) .1 (z color))))
                (v! (pow color
                         (vec3 (/ 1f0 2.2)))
                    1))))))
      (loop
         :for side :in sides
         :for rotation :in rotations
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
           ;; Set FBO texture 
           (setf (attachment fbo 0) (texref cubemap :cube-face i))
         ;; Final draw to LDR (? the colors
           (with-fbo-bound (fbo)
             (clear-fbo fbo)
             (map-g pipeline bs
                    :sam external-sample))))))

;; Reference: Cubemap generation mostly from
;; https://learnopengl.com/PBR/IBL/Diffuse-irradiance
(defgeneric save-to-cubemap (camera dimensions))
(defmethod save-to-cubemap ((camera pers) dimensions)
  (let ((sides '("left" "right" "top" "bottom" "front" "back"))
        (rotations
         (list
          (list (v! 0 -1  0) (v! 0 0 0) (v!  1  0  0))
          (list (v! 0 -1  0) (v! 0 0 0) (v! -1  0  0))          
          (list (v! 0  0 -1) (v! 0 0 0) (v!  0 -1  0))
          (list (v! 0  0  1) (v! 0 0 0) (v!  0  1  0))          
          (list (v! 0 -1  0) (v! 0 0 0) (v!  0  0  1))
          (list (v! 0 -1  0) (v! 0 0 0) (v!  0  0 -1)))))
    (assert (and (list dimensions) (= 2 (length dimensions)))
            (dimensions)
            "DIMENSIONS should be a list of 2 numbers")
    (assert (= (car dimensions) (cadr dimensions))
            (dimensions)
            "DIMENSIONS should have an aspect ratio of 1:1")
    (setf (resolution (current-viewport)) (v! dimensions))
    (setf (fov camera) 90f0)
    (with-free*
        ((external-fbo (make-fbo (list 0 :dimensions dimensions :element-type :rgb16f)
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
         :for side :in sides
         :for rotation :in rotations
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
             (concatenate 'string "static/cubemap_" side ".bmp")))))))

(defun draw! ()
  (let* ((res (surface-resolution (current-surface)))
         (time (mynow)))
    
    (setf (resolution (current-viewport)) res)
    
    (update *currentcamera*)
    (update-all-the-things *actors*)
    
    ;; (unless *saved*
    ;;   (save-to-cubemap *camera-cubemap* *dimensions*))

    ;; (unless *saved*
    ;;   (render-to-cubemap *camera-cubemap* *dimensions* *t-cubemap-live*))
    
    (with-fbo-bound (*fbo*)
      (clear-fbo *fbo*)
      (loop :for actor :in *actors* :do
           (draw actor *currentcamera*)))
    
    (as-frame
      (with-setf* ((depth-mask) nil
                   (cull-face) nil
                   (clear-color) (v! 0 0 0 1))        
        (map-g #'generic-2d-pipe *bs*
               :sam *sam*)))))

(def-simple-main-loop runplay
    (:on-start #'initialize)
  (draw!))

