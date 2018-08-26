(in-package :somecepl)

(defvar *fbo* nil)
(defvar *sam* nil)

(defun initialize ()
  (unless *fbo*
    (setf *fbo* (make-fbo 0))
    (setf *sam* (cepl:sample (attachment-tex *fbo* 0))))
  (setf (clear-color) (v! .2 .2 .2 0))
  (setf *actors* nil *lead* nil)
  (make-sphere)
  (make-voz)
  (make-ground)
  (setf *lead* (make-lead)))

(defun draw! ()  
  (let ((res (surface-resolution (current-surface))))
    (setf (resolution (current-viewport))
          res)
    (as-frame
      (map-g-into *fbo* #'pass-pipe (get-quad-stream-v2)
                  :time (mynow))
      (draw-tex-tr *sam*)
      (update *currentcamera*)
      (loop :for actor :in *actors*
         :do
         (update actor)
         (draw actor res)))))

(def-simple-main-loop runplay (:on-start #'initialize)
  (draw!))
