(in-package :shiny)

(defvar *particle-fbo* nil)
(defvar *sam-particle-fbo* nil)
(defvar *samd-particle-fbo* nil)

(defvar *fbo* nil)
(defvar *sam* nil)
(defvar *samd* nil)
(defvar *bs* nil)

;; Cubemap with clouds
(defvar *t-cubemap* nil)
(defvar *s-cubemap* nil)

;; Cubemap for the luminance
(defparameter *saved* nil)
(defvar *t-cubemap-live* nil)
(defvar *s-cubemap-live* nil)

(defvar *cloud-tex* nil)

;; Cubemap to store the mix between the clouds and own floor geometry
;; ???

;; Prefilter cubemap - for specular
(defparameter *prefilter* nil)
(defvar *t-cubemap-prefilter* nil)
(defvar *s-cubemap-prefilter* nil)

(defparameter *brdf* nil)
(defvar *f-brdf* nil)
(defvar *t-brdf* nil)
(defvar *s-brdf* nil)

(defparameter *dimensions* '(400 300))
(defparameter *dimensions* '(800 600))
(defparameter *dimensions* '(532 400))

(defun free-cubes ()
  (when *t-cubemap-prefilter*    
    (free *t-cubemap-prefilter*)
    (setf *t-cubemap-prefilter* NIL))  
  (when *t-cubemap*
    (free *t-cubemap*)
    (setf *t-cubemap* NIL))
  (when *t-cubemap-live*
    (free *t-cubemap-live*)
    (setf *t-cubemap-live* NIL)))

(defun initialize ()
  ;; (unless *cloud-tex*
  ;;   (setf *cloud-tex*
  ;;         (get-tex "static/Cloud04_8x8.tga")))
  ;;(init-particles 100)
  (unless *t-cubemap*
    ;; (setf *t-cubemap*
    ;;       (make-cubemap-tex
    ;;        "static/ThickCloudsWater/left.png"
    ;;        "static/ThickCloudsWater/right.png"
    ;;        "static/ThickCloudsWater/up.png"
    ;;        "static/ThickCloudsWater/down.png"
    ;;        "static/ThickCloudsWater/front.png"
    ;;        "static/ThickCloudsWater/back.png"))
    ;; (setf *t-cubemap*
    ;;       (make-cubemap-tex
    ;;        "static/cubemap_left.bmp"
    ;;        "static/cubemap_right.bmp"
    ;;        "static/cubemap_bottom.bmp"
    ;;        "static/cubemap_front.bmp"
    ;;        "static/cubemap_top.bmp"
    ;;        "static/cubemap_back.bmp"))
    ;; (setf *s-cubemap*
    ;;       (cepl:sample *t-cubemap*
    ;;                    :wrap :clamp-to-edge
    ;;                    :magnify-filter :linear))
    )
  ;;--------------------------------------------------
  ;; IBL - Diffuse ambient
  ;; (unless *t-cubemap-live*
  ;;   (setf *t-cubemap-live*
  ;;         (make-texture
  ;;          nil
  ;;          :element-type :rgb16f
  ;;          :dimensions '(32 32)
  ;;          :cubes t))
  ;;   (setf *s-cubemap-live*
  ;;         (cepl:sample *t-cubemap-live*
  ;;                      :minify-filter :linear
  ;;                      :magnify-filter :linear
  ;;                      :wrap :clamp-to-edge)))
  ;; ;;--------------------------------------------------
  ;; ;; IBL - Specular ambient
  ;; ;; 1) prefilter
  ;; (unless *t-cubemap-prefilter*
  ;;   (setf *t-cubemap-prefilter*
  ;;         (make-texture
  ;;          nil
  ;;          :element-type :rgb16f
  ;;          :dimensions '(128 128)
  ;;          :mipmap 5
  ;;          :cubes t))
  ;;   (setf *s-cubemap-prefilter*
  ;;         (cepl:sample *t-cubemap-prefilter*
  ;;                      :magnify-filter :linear
  ;;                      :wrap :clamp-to-edge)))
  ;; ;; 2) BRDF
  ;; (unless *f-brdf*
  ;;   (setf *f-brdf*
  ;;         (make-fbo
  ;;          (list 0 :element-type :rg16f :dimensions '(512 512))))
  ;;   (setf *t-brdf* (attachment-tex *f-brdf* 0))
  ;;   (setf *s-brdf*
  ;;         (cepl:sample *t-brdf*
  ;;                      :wrap :clamp-to-edge
  ;;                      :magnify-filter :linear
  ;;                      :minify-filter :linear)))
  
  ;;--------------------------------------------------
  ;; Buffer stream for single stage pipelines  
  (unless *bs*
    (setf *bs* (make-buffer-stream nil :primitive :points)))
  ;;--------------------------------------------------
  ;; HDR fbo(s)
  (when *fbo* (free *fbo*))
  (setf *fbo*
        (make-fbo
         (list 0 :element-type :rgb16f :dimensions *dimensions*)
         (list :d :dimensions *dimensions*)))
  (setf *sam* (cepl:sample (attachment-tex *fbo* 0)
                           :wrap :clamp-to-edge))
  (setf *samd* (cepl:sample (attachment-tex *fbo* :d)
                            :wrap :clamp-to-edge))
  ;;--------------------------------------------------
  (setf (clear-color) (v! 0 0 0 1))
  ;;--------------------------------------------------
  (setf *actors* nil)
  ;;(make-pbr (v! 0 -2 0))
  ;;(make-pbr)
  ;;(make-piso (v! 0 -2 0))
  (make-thing)
  ;;(make-cubemap)
  ;;(make-pbr-simple (v! 0 0 -10))
  NIL)

(defun draw! ()
  (let* ((res (surface-resolution (current-surface)))
         (time (mynow)))
    
    (setf (resolution (current-viewport)) res)
    ;;(setf (resolution (current-viewport)) (v! *dimensions*))
    
    (update *currentcamera*)
    ;;(setf (pos *camera1*) *light-pos*)
    (update-all-the-things *actors*)

    ;; (unless *prefilter*
    ;;   (cubemap-render-to-prefilter-cubemap *camera-cubemap*
    ;;                                        *t-cubemap*
    ;;                                        *s-cubemap*
    ;;                                        *t-cubemap-prefilter*)
    ;;   (setf *prefilter* T))
    ;; (unless *brdf*
    ;;   (setf (resolution (current-viewport)) (v! 512 512))
    ;;   (map-g-into *f-brdf* #'brdf-pipe *bs*)
    ;;   (setf *brdf* T))

    ;; (unless *saved*
    ;;   (cubemap-render-to-irradiance-cubemap *camera-cubemap*
    ;;                                         *t-cubemap*
    ;;                                         *s-cubemap*
    ;;                                         *t-cubemap-live*)
    ;;   (setf *saved* T))

    (with-fbo-bound (*fbo*)
      (clear *fbo*)
      (loop :for actor :in *actors* :do
           (draw actor *currentcamera* time)))
    
    (as-frame
      (with-setf* ((depth-mask) nil
                   (cull-face) nil
                   (clear-color) (v! 0 0 0 1))        
        (map-g #'generic-2d-pipe *bs*
               :sam *sam*)))))

(def-simple-main-loop runplay (:on-start #'initialize)
  (draw!))
