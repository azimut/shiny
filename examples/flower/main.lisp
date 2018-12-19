(in-package :shiny)

(defvar *fbo* nil)
(defvar *sam* nil)
(defvar *bs* nil)

(defvar *t-cubemap* nil)
(defvar *s-cubemap* nil)

(defparameter *saved* nil)
(defparameter *dimensions* '(300 300))

(defun initialize ()
  (unless *t-cubemap*
    (setf *t-cubemap*
          (make-cubemap-tex
           "static/ThickCloudsWater/left.png"
           "static/ThickCloudsWater/right.png"
           "static/ThickCloudsWater/up.png"
           "static/ThickCloudsWater/down.png"
           "static/ThickCloudsWater/front.png"
           "static/ThickCloudsWater/back.png"))
    (setf *s-cubemap* (cepl:sample *t-cubemap*)))
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
  (make-pbr (v! 0 -2 0))
  (make-cubemap)
;;  (make-pbr-simple)
  NIL)


;; Reference: Cubemap generation mostly from
;; https://learnopengl.com/PBR/IBL/Diffuse-irradiance
(defgeneric save-to-cubemap (camera fbo sample))
(defmethod save-to-cubemap ((camera pers) (external-fbo fbo) sample)
  (let ((sides '("left" "right" "bottom" "top" "front" "back"))
        (rotations
         (list (list (v! 0 -1  0) (v! 0 0 0) (v!  1  0  0))
               (list (v! 0 -1  0) (v! 0 0 0) (v! -1  0  0))
               (list (v! 0  0  1) (v! 0 0 0) (v!  0  1  0))
               (list (v! 0  0 -1) (v! 0 0 0) (v!  0 -1  0))
               (list (v! 0 -1  0) (v! 0 0 0) (v!  0  0  1))
               (list (v! 0 -1  0) (v! 0 0 0) (v!  0  0 -1))))
        (dimensions (dimensions (attachment-tex external-fbo 0))))
    (assert (= 1 (/ (car dimensions) (cadr dimensions)))
            (dimensions)
            "Dimensions of the FBO should be so the
             aspect ratio is 1:1")
    (setf (resolution (current-viewport)) (v! dimensions))
    (with-free*
        ((bs (make-buffer-stream nil :primitive :points))
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
         :initially (setf (fov camera) 90f0)
         :finally   (setf *saved* T)
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
                    :sam sample))
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
    
    (unless *saved*
      (save-to-cubemap *camera-cubemap* *fbo* *sam*))
    
    ;; (with-fbo-bound (*fbo*)
    ;;   (clear-fbo *fbo*)
    ;;   (loop :for actor :in *actors* :do
    ;;        (draw actor *currentcamera*)))
    
    (as-frame
      (with-setf* ((depth-mask) nil
                   (cull-face) nil
                   (clear-color) (v! 0 0 0 1))        
        (map-g #'generic-2d-pipe *bs*
               :sam *sam*)))))

(def-simple-main-loop runplay
    (:on-start #'initialize)
  (draw!))

