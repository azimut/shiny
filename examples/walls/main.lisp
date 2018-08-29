(in-package :somecepl)

(defvar *fbo* nil)
(defvar *sam* nil)
(defvar *lfbo* nil)
(defvar *sfbo* nil)
(defparameter *light-camera*
  (make-instance
   'orth
   :pos (v! 0 5 0)
   :rot (q:from-axis-angle (v! 1 0 0)
                           (radians -55))
   :frame-size (v! 20 20)))

(defun initialize ()
  (unless *fbo*
    (setf *fbo* (make-fbo 0))
    (setf *sam* (cepl:sample (attachment-tex *fbo* 0))))
  (unless *lfbo*
    (setf *lfbo* (make-fbo '(:d :dimensions (1024 1024))))
    (setf *sfbo* (cepl:sample (attachment-tex *lfbo* :d))))
  (setf (clear-color) (v! .2 .2 .2 0))
  (setf *actors* nil)
  (setf *lead*   nil)
  ;;(make-sphere)
  (setf *lead* (make-lead))
  (make-voz)
  (make-ground)
  nil)

(defun draw! ()  
  (let ((res (surface-resolution (current-surface))))
    (setf (resolution (current-viewport))
          res)
    (as-frame
      ;; Noise texture
      (map-g-into *fbo* #'pass-pipe
                  (get-quad-stream-v2)
                  :time (mynow))
      ;; Shadow
      (progn;;with-fbo-bound (*lfbo* :attachment-for-size :d)
        (loop :for actor :in *actors* :do
           (draw actor *light-camera*)))
      ;;(draw-tex-tl *sam*)
      ;;; Render
      ;; (loop :for actor :in *actors* :do
      ;;    (update actor)
      ;;    (draw actor *currentcamera*))
      )))

(def-simple-main-loop runplay
    (:on-start #'initialize)
  (draw!))
