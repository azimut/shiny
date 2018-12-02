(in-package :shiny)

(defvar *fbo* nil)
(defvar *sam* nil)
(defvar *bs* nil)
(defvar *blend* (make-blending-params))

(defvar *gar-src* nil)
(defvar *gar-dst* nil)
(defvar *tfs-src* nil)
(defvar *tfs-dst* nil)
(defvar *str-src* nil)
(defvar *str-dst* nil)



(defun initialize ()
  ;; Different type of streams over the SAME gpu-arrays...
  ;; This might be a CEPL limitation, but is not that bad...
  (unless *gar-src*
    (setf *gar-src* (make-gpu-array nil :dimensions 1000 :element-type 'pdata)
          *gar-dst* (make-gpu-array nil :dimensions 1000 :element-type 'pdata)
          *str-src* (make-buffer-stream *gar-src* :primitive :points)
          *str-dst* (make-buffer-stream *gar-dst* :primitive :points)
          *tfs-src* (make-transform-feedback-stream *gar-src*)
          *tfs-dst* (make-transform-feedback-stream *gar-dst*)))
  (with-transform-feedback (*tfs-src*)
    (map-g #'pinit-pipe *bs*))
  (with-transform-feedback (*tfs-dst*)
    (map-g #'pinit-pipe *bs*))
  ;;--------------------------------------------------
  ;; Buffer stream for single stage pipelines
  (unless *bs* (setf *bs* (make-buffer-stream nil :primitive :points)))
  ;;--------------------------------------------------
  ;; HDR fbo(s)
  (when *fbo* (free *fbo*))
  (setf *fbo* (make-fbo (list 0 :element-type :rgb16f)
                        :d))
  (setf *sam* (cepl:sample (attachment-tex *fbo* 0)))
  (setf (clear-color) (v! 0 0 0 1))
  ;;--------------------------------------------------
  ;; Actores
  (setf *actors* nil)
  (make-celestial-sphere)
  ;;(make-box)
  ;;(make-piso (v! 0 -2 0))
  ;;(make-piso (v! 0  2 0) (q:from-axis-angle (v! 1 0 0) (radians 180)))
  ;; (%gl::sampler-parameter-f
  ;;  (%cepl.types::%sampler-id (get-tex "static/checker.dds"))
  ;;  :texture-max-anisotropy-ext 1f0)
  NIL)

(defun draw! ()  
  (let ((res (surface-resolution (current-surface))))
    (setf (resolution (current-viewport)) res)
    
    (update *currentcamera*)
    (update-all-the-things *actors*)
    
    (with-fbo-bound (*fbo*)
      (clear-fbo *fbo*)
      (loop :for actor :in *actors* :do
         (draw actor *currentcamera*))
      ;; Update particles
      (with-transform-feedback (*tfs-dst*)
        (map-g #'pupdate-pipe *str-src*
               :time (mynow)))
      ;; Draw particles
      (map-g #'prender-pipe *str-src*
             :world-view (world->view *currentcamera*)
             :view-clip (projection *currentcamera*))
      (rotatef *tfs-src* *tfs-dst*)
      (rotatef *str-src* *str-dst*)
      (rotatef *gar-src* *gar-dst*)
      ;; (with-blending *blend*
      ;;   (map-g #'billboard-pipe *bs*
      ;;          :camera-pos (pos *currentcamera*)
      ;;          :tex (get-tex "static/float-alpha.png")
      ;;          :time (mynow)
      ;;          :world-view (world->view *currentcamera*)
      ;;          :view-clip (projection *currentcamera*)))
      )
    (as-frame
      (with-setf* ((depth-mask) nil
                   (cull-face) nil
                   (clear-color) (v! 0 0 0 1)
                   (depth-test-function) #'always)
        (map-g #'generic-2d-pipe *bs*
               :sam *sam*)))))
(def-simple-main-loop runplay
    (:on-start #'initialize)
  (draw!))

