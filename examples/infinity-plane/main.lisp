(in-package :shiny)

(defvar *fbo* nil)
(defvar *sam* nil)
(defvar *bs* nil)
(defvar *sam-depth* nil)
(defvar *tmp* nil)
(defvar *tmp2* nil)
(defvar *blend* (make-blending-params))


(defun initialize ()
  ;;--------------------------------------------------
  ;; Buffer stream for single stage pipelines
  (unless *bs* (setf *bs* (make-buffer-stream nil :primitive :points)))
  ;;--------------------------------------------------
  ;; HDR fbo(s)
  (when *fbo*
    (free *fbo*))
  (setf *fbo* (make-fbo (list 0 :element-type :rgb16f)
                        :d))
  (setf *sam* (cepl:sample (attachment-tex *fbo* 0)))
  (setf (clear-color) (v! 0 0 0 1))
  (setf *actors* nil)
  (make-celestial-sphere)
  ;;(make-box)
  (make-piso (v! 0 -2 0))
  (make-piso (v! 0  2 0) (q:from-axis-angle (v! 1 0 0) (radians 180)))
  (%gl::sampler-parameter-f
   (%cepl.types::%sampler-id (get-tex "static/checker.dds"))
   :texture-max-anisotropy-ext 1f0)
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
      (with-blending *blend*
        (map-g #'billboard-pipe *bs*
               :camera-pos (pos *currentcamera*)
               :tex (get-tex "static/float-alpha.png")
               :time (mynow)
               :world-view (world->view *currentcamera*)
               :view-clip (projection *currentcamera*))))

    (as-frame
      (with-setf* ((depth-mask) nil
                   (cull-face) nil
                   (clear-color) (v! 0 0 0 1)
                   (depth-test-function) #'always)
        (map-g #'generic-2d-pipe *bs*
           :sam *sam*)))
    ))
(def-simple-main-loop runplay
    (:on-start #'initialize)
  (draw!))

