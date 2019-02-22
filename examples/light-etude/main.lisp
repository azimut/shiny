(in-package :shiny)

(defvar *dimensions-fbo* nil)
(defvar *fbo* nil)
(defvar *sam* nil)

;; Assimp
(defvar *mesh* nil)
(defvar *mesh-light* nil)
(defvar *ass* nil)
(defvar *ass-light* nil)

(defvar *blend* nil)
(defvar *bs* nil)

;; Merge

(defvar *sam-depth* nil)

(defvar *fbo-secondary* nil)
(defvar *sam-secondary* nil)
(defvar *fbo-terciary* nil)
(defvar *sam-terciary* nil)

;; SSAO
(defvar *sam-pos* nil)
(defvar *sam-nor* nil)
(defvar *sam-alb* nil)
(defvar *fbo-ao* nil)
(defvar *sam-ao* nil)
(defvar *noise-tex* nil)
(defvar *noise-sam* nil)
(defvar *fbo-ao-blur* nil)
(defvar *sam-ao-blur* nil)
(defvar *ubo-kernel* nil)

;; Blur
(defvar *sam1* nil)
(defvar *half-fbo* nil)
(defvar *fourth-fbo* nil)
(defvar *eighth-fbo* nil)
(defvar *sixteen-fbo* nil)
(defvar *sam-half-fbo* nil)
(defvar *sam-fourth-fbo* nil)
(defvar *sam-eighth-fbo* nil)
(defvar *sam-sixteen-fbo* nil)

(defun generate-rotation-kernel ()
  (loop :repeat 4 :collect
     (loop :repeat 4 :collect
        (v! (1- (* 2 (random 1f0)))
            (1- (* 2 (random 1f0)))
            0f0))))

(defun generate-sample-kernel ()
  (loop :for i :below 64 :collect
     (let* ((sample (v! (1- (* 2 (random 1f0)))
                        (1- (* 2 (random 1f0)))
                        (random 1f0)))
            (sample (v3:normalize sample))
            (sample (v3:*s sample (random 1f0)))
            (scale (/ i 64f0))
            (scale (lerp .1 1f0 (* scale scale))))
       (v3:*s sample scale))))

(defun initialize ()
  ;;(unless *mesh*)
  (setf *mesh*
        (elt
         (slot-value
          (ai:import-into-lisp
           "/home/sendai/untitled.3ds"
           :processing-flags '(:ai-process-triangulate
                               :ai-process-calc-tangent-space))
          'ai:meshes)
         0))
  (setf *mesh-light*
        (elt
         (slot-value
          (ai:import-into-lisp
           "/home/sendai/untitled-light.3ds"
           :processing-flags '(:ai-process-triangulate
                               :ai-process-calc-tangent-space))
          'ai:meshes)
         0))
  (when *ass*       (cepl:free (slot-value *ass* 'buf)))
  (when *ass-light* (cepl:free (slot-value *ass-light* 'buf)))
  (setf *ass*
        (make-instance 'assimp-thing-with-maps
                       :rot (q:from-axis-angle (v! 1 0 0)
                                               (radians -90))
                       :buf (assimp-mesh-to-stream *mesh*)))
  (setf *ass-light*
        (make-instance 'assimp-bloom
                       :rot (q:from-axis-angle (v! 1 0 0)
                                               (radians -90))
                       :buf (assimp-mesh-to-stream *mesh-light*)))
  ;;--------------------------------------------------
  ;; Buffer stream for single stage pipelines
  (unless *bs* (setf *bs* (make-buffer-stream nil :primitive :points)))
  ;;--------------------------------------------------
  ;; HDR fbo(s)
  (when *fbo*
    (free *fbo*)
    ;; (free *fbo-secondary*)
    ;; (free *fbo-terciary*)
    ;; (free *fbo-ao*)
    )
  
  ;; (setf *fbo* (make-fbo (list 0 :element-type :rgba16f)
  ;;                       ;; BLOOM LIGHT
  ;;                       (list 1 :element-type :rgba16f)
  ;;                       ;; SSAO
  ;;                       (list 2 :element-type :rgb16f) ;; pos
  ;;                       (list 3 :element-type :rgb16f) ;; nor
  ;;                       :d))
  (setf *fbo* (make-fbo ;;(list 0 :element-type :rgb16f) ;; pos
               (list 0 :element-type :rgb16f) ;; nor
               :d))
  (setf *dimensions-fbo* (dimensions (attachment-tex *fbo* 0)))
  ;; (setf *sam* (cepl:sample (attachment-tex *fbo* 0) :wrap :clamp-to-edge))
  ;; (setf *sam1* (cepl:sample (attachment-tex *fbo* 1) :wrap :clamp-to-edge))
  ;;--------------------------------------------------
  ;; SSAO - Normal and position textures
  (setf *sam-depth* (cepl:sample (attachment-tex *fbo* :d)
                                 :minify-filter :nearest
                                 :magnify-filter :nearest
                                 :wrap :clamp-to-edge))
  ;; (setf *sam-pos* (cepl:sample (attachment-tex *fbo* 0)
  ;;                              :minify-filter :nearest
  ;;                              :magnify-filter :nearest
  ;;                              :wrap :clamp-to-edge))
  (setf *sam-nor* (cepl:sample (attachment-tex *fbo* 0)
                               :minify-filter :nearest
                               :magnify-filter :nearest))
  ;; SSAO - Random kernel rotations - generate noise texture
  (unless *noise-tex*
    (setf *noise-tex* (make-texture (generate-rotation-kernel)
                                    :element-type :rgb32f))
    (setf *noise-sam* (cepl:sample *noise-tex*
                                   :minify-filter :nearest
                                   :magnify-filter :nearest)))
  ;; SSAO - samples
  (unless *ubo-kernel*
    (setf *ubo-kernel* (make-ubo (list (generate-sample-kernel))
                                 'random-kernel)))
  ;; (unless *g-kernel*
  ;;   (setf *g-kernel* (make-gpu-array (generate-sample-kernel)
  ;;                                    :dimensions 64
  ;;                                    :element-type :vec3)))
  ;; SSAO - ?
  ;; (setf *fbo-ao* (make-fbo (list 0 :element-type :r16)))
  ;; (setf *sam-ao* (cepl:sample (attachment-tex *fbo-ao* 0)
  ;;                             :minify-filter :nearest
  ;;                             :magnify-filter :nearest
  ;;                             :wrap :clamp-to-edge))
  ;; (setf *fbo-ao-blur* (make-fbo (list 0 :element-type :r8)))
  ;; (setf *sam-ao-blur* (cepl:sample (attachment-tex *fbo-ao-blur* 0)
  ;;                             :minify-filter :nearest
  ;;                             :magnify-filter :nearest
  ;;                             :wrap :clamp-to-edge))
  ;;--------------------------------------------------
  ;; Sec FBO
  ;; (setf *fbo-secondary* (make-fbo (list 0 :element-type :rgba16f)))
  ;; (setf *sam-secondary*
  ;;       (cepl:sample (attachment-tex *fbo-secondary* 0)
  ;;                    :wrap :clamp-to-edge))
  ;; Ter FBO
  ;; (setf *fbo-terciary* (make-fbo (list 0 :element-type :rgba16f)))
  ;; (setf *sam-terciary*
  ;;       (cepl:sample (attachment-tex *fbo-terciary* 0)
  ;;                    :wrap :clamp-to-edge))
  ;;--------------------------------------------------
  ;; BLUR

  ;; Used for the upsampling part of the blur
  (unless *blend*
    (setf *blend* (make-blending-params :source-rgb :one
                                        :destination-rgb :one)))
  (when *half-fbo*
    (free *half-fbo*)
    (free *fourth-fbo*)
    (free *eighth-fbo*)
    (free *sixteen-fbo*))
  
  ;; (flet ((f (d) (mapcar (lambda (x) (round (/ x d)))
  ;;                       *dimensions-fbo*)))
  ;;   (setf *half-fbo*
  ;;         (make-fbo (list 0 :element-type :rgba16f :dimensions (f 2))))
  ;;   (setf *fourth-fbo*
  ;;         (make-fbo (list 0 :element-type :rgba16f :dimensions (f 4))))
  ;;   (setf *eighth-fbo*
  ;;         (make-fbo (list 0 :element-type :rgba16f :dimensions (f 8))))
  ;;   (setf *sixteen-fbo*
  ;;         (make-fbo (list 0 :element-type :rgba16f :dimensions (f 16))))
  ;;   (setf *sam-half-fbo* (cepl:sample (attachment-tex *half-fbo* 0) 
  ;;                                     :wrap :clamp-to-edge))
  ;;   (setf *sam-fourth-fbo* (cepl:sample (attachment-tex *fourth-fbo* 0)
  ;;                                       :wrap :clamp-to-edge))
  ;;   (setf *sam-eighth-fbo* (cepl:sample (attachment-tex *eighth-fbo* 0)
  ;;                                       :wrap :clamp-to-edge))
  ;;   (setf *sam-sixteen-fbo* (cepl:sample (attachment-tex *sixteen-fbo* 0)
  ;;                                        :wrap :clamp-to-edge)))
  ;;--------------------------------------------------
  (setf (clear-color) (v! 0 0 0 1))
  ;;(setf (clear-color) (v! 0 0 0 1))
  (setf *actors* nil)
  ;;;(make-box)
  (make-cement)
  (make-celestial-sphere)
  ;;(push *ass* *actors*)
  ;;(push *ass-light* *actors*)
  ;;(make-sphere)
  ;;(make-piso)
  NIL)

(defun draw! ()  
  (let ((res (surface-resolution (current-surface))))
    (setf (resolution (current-viewport)) res)
    
    (update *currentcamera*)
    (update-all-the-things *actors*)
    
    (with-fbo-bound (*fbo*)
      (clear-fbo *fbo*)
      (loop :for actor :in *actors* :do
           (draw actor *currentcamera*)))

    ;; Really poor implementation of the awesome and more effective bloom from:
    ;; https://catlikecoding.com/unity/tutorials/advanced-rendering/bloom/
    ;; (let ((width (car *dimensions-fbo*))
    ;;       (height (car (last *dimensions-fbo*))))
    ;;   (with-setf* ((depth-mask) nil
    ;;                (cull-face) nil
    ;;                (clear-color) (v! 0 0 0 1)
    ;;                (depth-test-function) #'always)
    ;;;;--------------------------------------------------
    ;;     (with-fbo-bound (*half-fbo*)
    ;;       (clear-fbo *half-fbo*)
    ;;       (map-g #'bloom-pipe *bs*
    ;;              :sam *sam1*
    ;;              :x (/ 1f0 width)
    ;;              :y (/ 1f0 height)
    ;;              :delta 1f0))
    ;;     (with-fbo-bound (*fourth-fbo*)
    ;;       (clear-fbo *fourth-fbo*)
    ;;       (map-g #'bloom-pipe *bs*
    ;;              :sam *sam-half-fbo*
    ;;              :x (/ 1f0 (round (* .5 width)))
    ;;              :y (/ 1f0 (round (* .5 height)))))
    ;;     (with-fbo-bound (*eighth-fbo*)
    ;;       (clear-fbo *eighth-fbo*)
    ;;       (map-g #'bloom-pipe *bs*
    ;;              :sam *sam-fourth-fbo*
    ;;              :x (/ 1f0 (round (* .5 (* .5 width))))
    ;;              :y (/ 1f0 (round (* .5 (* .5 height))))))
    ;;     (with-fbo-bound (*sixteen-fbo*)
    ;;       (clear-fbo *sixteen-fbo*)
    ;;       (map-g #'bloom-pipe *bs*
    ;;              :sam *sam-eighth-fbo*
    ;;              :x (/ 1f0 (round (* .5 (* .5 (* .5 width)))))
    ;;              :y (/ 1f0 (round (* .5 (* .5 (* .5 height)))))))
    ;;;;--------------------------------------------------
    ;;     ;; END downscale BEGIN upscale
    ;;     (with-blending *blend*
    ;;       (with-fbo-bound (*eighth-fbo*)
    ;;         (clear-fbo *eighth-fbo*)
    ;;         (map-g #'bloom-pipe *bs*
    ;;                :sam *sam-sixteen-fbo*
    ;;                :x (/ 1f0 (round (* .5 (* .5 (* .5 width)))))
    ;;                :y (/ 1f0 (round (* .5 (* .5 (* .5 height)))))
    ;;                :delta .5)))
    ;;     (with-blending *blend*
    ;;       (with-fbo-bound (*fourth-fbo*)
    ;;         (clear-fbo *fourth-fbo*)
    ;;         (map-g #'bloom-pipe *bs*
    ;;                :sam *sam-eighth-fbo*
    ;;                :x (/ 1f0 (round (* .5 (* .5 width))))
    ;;                :y (/ 1f0 (round (* .5 (* .5 height)))))))
    ;;     (with-blending *blend*
    ;;       (with-fbo-bound (*half-fbo*)
    ;;         (clear-fbo *half-fbo*)
    ;;         (map-g #'bloom-pipe *bs*
    ;;                :sam *sam-fourth-fbo*
    ;;                :x (/ 1f0 (round (* .5 width)))
    ;;                :y (/ 1f0 (round (* .5 height))))))
    ;;     (with-blending *blend*
    ;;       (with-fbo-bound (*fbo-secondary*)
    ;;         (clear-fbo *fbo-secondary*)
    ;;         (map-g #'bloom-pipe *bs*
    ;;                :sam *sam-half-fbo*
    ;;                :x (/ 1f0 width)
    ;;                :y (/ 1f0 height))))
    ;;--------------------------------------------------
    ;;     ;; END upscale BEGIN merge
    ;;     (with-fbo-bound (*fbo-terciary*)
    ;;       (clear-fbo *fbo-terciary*)
    ;;       (map-g #'dobloom-pipe *bs*
    ;;              :sam *sam*
    ;;              :light-sam *sam-secondary*
    ;;              :delta .5
    ;;              :x (/ 1f0 width)
    ;;              :y (/ 1f0 height)))))
    (as-frame
      (with-setf* ((depth-mask) nil
                   (cull-face) nil
                   (clear-color) (v! 0 0 0 1)
                   (depth-test-function) #'always)
        (map-g #'ssao-pipe *bs*
               ;;                 :g-position *sam-pos*
               :g-normal *sam-nor*
               :g-depth *sam-depth*
               :kernel *kernel*
               :kernel-effect *kernel-effect*
               :radius *radius*
               ;;                 :world-view (world->view *currentcamera*)
               :tex-noise *noise-sam*
               :random-kernel *ubo-kernel*
               :res (v! *dimensions-fbo*)
               :view-clip (projection *currentcamera*))))
    ;; (as-frame
    ;;   (map-g #'generic-2d-pipe *bs*
    ;;          ;;:sam *sam-terciary*
    ;;          ;;:sam *sam*
    ;;          :sam *sam-nor*
    ;;          ))
    ))

(def-simple-main-loop runplay
    (:on-start #'initialize)
  (draw!))

