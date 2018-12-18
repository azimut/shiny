(in-package :shiny)

(defvar *fbo* nil)
(defvar *sam* nil)
(defvar *bs* nil)

(defun initialize ()
  ;;--------------------------------------------------
  ;; Buffer stream for single stage pipelines
  (unless *bs* (setf *bs* (make-buffer-stream nil :primitive :points)))
  ;;--------------------------------------------------
  ;; HDR fbo(s)
  (when *fbo* (free *fbo*))
  (setf *fbo* (make-fbo (list 0 :element-type :rgb16f)
                        :d))
  (setf *sam* (cepl:sample (attachment-tex *fbo* 0)
                           :wrap :clamp-to-edge))
  ;;--------------------------------------------------
  (setf (clear-color) (v! 0 0 0 1))
  ;;--------------------------------------------------
  (setf *actors* nil)
  (make-pbr (v! 0 -2 0))
  (make-pbr-simple (v! 0 -2 -30))
  NIL)

(defun draw! ()
  (let* ((res (surface-resolution (current-surface)))
         (time (mynow)))
    
    (setf (resolution (current-viewport)) res)

    (update *currentcamera*)
    (update-all-the-things *actors*)

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

