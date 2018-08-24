(in-package :somecepl)

(defun initialize ()
  (setf (clear-color) (v! .2 .2 .2 0))
  (setf *actors* nil *lead* nil)
  (make-sphere)
  (make-voz)
  (setf *lead* (make-lead)))

(defun draw! ()  
  (let ((res (surface-resolution (current-surface))))
    (setf (resolution (current-viewport))
          res)
    (as-frame
      (update *currentcamera*)
      (loop :for actor :in *actors*
         :do
         (update actor)
         (draw actor res)))))

(def-simple-main-loop runplay (:on-start #'initialize)
  (draw!))
