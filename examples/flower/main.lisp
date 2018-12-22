(in-package :shiny)

(defvar *fbo* nil)
(defvar *sam* nil)
(defvar *bs* nil)

;; Cubemap with clouds
(defvar *t-cubemap* nil)
(defvar *s-cubemap* nil)

;; Cubemap for the luminance
(defvar *t-cubemap-live* nil)
(defvar *s-cubemap-live* nil)

;; Cubemap to store the mix between the clouds and own floor geometry
;; ???

;; Prefilter cubemap - for specular
(defvar *t-cubemap-prefilter* nil)
(defvar *s-cubemap-prefilter* nil)

(defparameter *saved* nil)
(defparameter *dimensions* '(512 512))

(defun free-cubes ()
  ;;(setf *saved* NIL)
  (when *t-cubemap-prefilter*
    (free *t-cubemap-prefilter*)
    (setf *t-cubemap-prefilter* NIL))  
  ;; (when *t-cubemap*
  ;;   (free *t-cubemap*)
  ;;   (setf *t-cubemap* NIL))
  ;; (when *t-cubemap-live*
  ;;   (free *t-cubemap-live*)
  ;;   (setf *t-cubemap-live* NIL))
  )

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
    (setf *s-cubemap*
          (cepl:sample *t-cubemap*
                       :wrap :clamp-to-edge
                       :minify-filter :linear)))
  (unless *t-cubemap-live*
    (setf *t-cubemap-live*
          (make-texture
           nil
           :element-type :rgb16f
           :dimensions '(32 32)
           :cubes t))
    (setf *s-cubemap-live*
          (cepl:sample *t-cubemap-live*
                       :minify-filter :linear
                       :wrap :clamp-to-edge)))
  ;;--------------------------------------------------
  (unless *t-cubemap-prefilter*
    (setf *t-cubemap-prefilter*
          (make-texture
           nil
           :element-type :rgb16f
           :dimensions '(128 128)
           :mipmap 5
           :cubes t))
    (setf *s-cubemap-prefilter*
          (cepl:sample *t-cubemap-prefilter*
                       :wrap :clamp-to-edge)))
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
  ;;--------------------------------------------------
  (setf (clear-color) (v! 0 0 0 1))
  ;;--------------------------------------------------
  (setf *actors* nil)
  ;;(make-pbr (v! 0 -2 -10))
  ;;(make-cubemap)
  ;;(make-light-cubemap)
  (make-pbr-simple (v! 0 0 -10))
  NIL)

(defun draw! ()
  (let* ((res (surface-resolution (current-surface)))
         (time (mynow)))
    
    (setf (resolution (current-viewport)) res)
    ;;(setf (resolution (current-viewport)) (v! *dimensions*))
    
    (update *currentcamera*)
    (update-all-the-things *actors*)
    
    ;; (unless *saved*
    ;;   (save-to-cubemap *camera-cubemap* *dimensions*))
    ;; (unless *saved*
    ;;   (render-to-cubemap *camera-cubemap*
    ;;                      *dimensions*
    ;;                      *t-cubemap-live*)
    ;;   (delete-actor-class "CUBEMAP")
    ;;   (make-light-cubemap))

    (unless *saved*
      (render-to-light-cubemap
       *camera-cubemap*
       *t-cubemap*
       *s-cubemap*
       *t-cubemap-live*)
      ;;(delete-actor-class "CUBEMAP")
      ;;(make-light-cubemap)
      )
    
    (with-fbo-bound (*fbo*)
      (clear-fbo *fbo*)
      (loop :for actor :in *actors* :do
           (draw actor *currentcamera*)))
    
    (as-frame
      (with-setf* ((depth-mask) nil
                   (cull-face) nil
                   (clear-color) (v! 0 0 0 1))        
        (map-g #'generic-2d-pipe *bs*
               :sam *sam*)))
    ))

(def-simple-main-loop runplay
    (:on-start #'initialize)
  (draw!))
