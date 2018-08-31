(in-package :shiny)

(defun initialize ()
  (unless *fbo*
    (setf *fbo* (make-fbo 0))
    (setf *sam* (cepl:sample (attachment-tex *fbo* 0))))
  (unless *lfbo*
    (setf *lfbo* (make-fbo '(:d :dimensions (1024 1024))))
    (setf *sfbo* (cepl:sample (attachment-tex *lfbo* :d))))
  (setf (clear-color) (v! .2 .2 .2 0))
  (setf *actors* nil)
  (setf *outsiders* nil)
  (setf *lead* nil)
  ;;--------------------------------------------------

  ;; Make!!!
  (make-planet)
  (make-sphere)
  ;; (setf *lead* (make-lead))
  ;; (make-voz)
  ;; (make-portal)
  ;;(make-ground)
  nil)

(defun draw! ()  
  (let ((res (surface-resolution (current-surface))))
    (setf (resolution (current-viewport))
          res)
    (as-frame
      (update *currentcamera*)
      ;; Noise texture
      (map-g-into *fbo* #'pass-pipe
                  (get-quad-stream-v2)
                  :time (mynow))
      ;; Shadow
      ;; (with-fbo-bound (*lfbo* :attachment-for-size :d)
      ;;   (loop :for actor :in *actors* :do
      ;;      (draw actor *light-camera*)))
      ;;(draw-tex *sam*)
      ;;; Render
      (update-all-the-things *outsiders*)
      ;;(update *currentcamera*)
      (with-fbo-bound (*fbo*)
        (clear-fbo *fbo*)
        (loop :for actor :in *actors* :do
           (draw actor *portal-camera*)))
      (loop :for actor :in *outsiders* :do
         (draw actor *currentcamera*))
      )))


(defun draw! ()  
  (let ((res (surface-resolution (current-surface))))
    (setf (resolution (current-viewport))
          res)
    (as-frame
      (update *currentcamera*)
      ;; Noise texture
      (map-g-into *fbo* #'pass-pipe
                  (get-quad-stream-v2)
                  :time (mynow))
      ;; Shadow
      ;; (with-fbo-bound (*lfbo* :attachment-for-size :d)
      ;;   (loop :for actor :in *actors* :do
      ;;      (draw actor *light-camera*)))
      ;;(draw-tex-br *sam*)
      ;;; Render
      ;;(update-all-the-things *outsiders*)
      ;;(update *currentcamera*)
      ;; (with-fbo-bound (*fbo*)
      ;;   (clear-fbo *fbo*)
      ;;   (loop :for actor :in *actors* :do
      ;;      (draw actor *portal-camera*)))
      (loop :for actor :in *actors* :do
         (draw actor *currentcamera*)))))

(def-simple-main-loop runplay
    (:on-start #'initialize)
  (draw!))
