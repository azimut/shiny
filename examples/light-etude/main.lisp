(in-package :shiny)

(defvar *fbo* nil)
(defvar *sam* nil)
(defvar *mesh* nil)
(defvar *mesh-light* nil)
(defvar *ass* nil)
(defvar *ass-light* nil)
(defvar *bs* nil)

(defun initialize ()
  ;; Buffer stream for single stage pipelines
  (unless *bs* (setf *bs* (make-buffer-stream nil :primitive :points)))
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
  (when *ass* (cepl:free (slot-value *ass* 'buf)))
  (when *ass-light* (cepl:free (slot-value *ass-light* 'buf)))
  (setf *ass*
        (make-instance 'assimp-thing
;;                       :pos (v! 4 0 4)
                       :rot (q:from-axis-angle (v! 1 0 0)
                                       (radians -90))
                       :buf (assimp-mesh-to-stream *mesh*)))
  (setf *ass-light*
        (make-instance 'assimp-bloom
;;                       :pos (v! 4 0 4)
                       :rot (q:from-axis-angle (v! 1 0 0)
                                       (radians -90))
                       :buf (assimp-mesh-to-stream *mesh-light*)))
  ;; HDR fbo
  (when *fbo*
    (free *fbo*))
  (setf *fbo* (make-fbo (list 0 :element-type :rgba16f) :d))
  (setf *sam* (cepl:sample (attachment-tex *fbo* 0)))

  ;;--------------------------------------------------
  ;;(setf (clear-color) (v! .2 .2 .9 0))
  (setf (clear-color) (v! 0 0 0 1))
  (setf *actors* nil)
  ;;(make-box)
  (push *ass* *actors*)
  (push *ass-light* *actors*)
  ;;(make-sphere)
  (make-piso)
  NIL)

(defun draw! ()  
  (let ((res (surface-resolution (current-surface))))
    (setf (resolution (current-viewport)) res)
    (update *currentcamera*)
    ;;(update-all-the-things *actors*)
    (with-fbo-bound (*fbo*)
      (clear-fbo *fbo*)
      (loop :for actor :in *actors* :do
         (draw actor *currentcamera*)))
    ;; Draw fbo/postprocess
    (as-frame
      (map-g #'generic-2d-pipe *bs* :sam *sam*))))

(def-simple-main-loop runplay
    (:on-start #'initialize)
  (draw!))

