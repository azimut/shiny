(in-package :somecepl)

;; CV> (query-video "/home/sendai/videos/sally.mp4")
;; size:7600 n-size:144 w:50 h:50
(defun show-video (filename &optional (sec 30))
  (with-captured-file (capture filename)
    (skip-to capture sec)
    (let ((frame (cv:query-frame capture)))
      (cv:with-ipl-images ((img (cv:size 50 50) cv:+ipl-depth-8u+ 3))
        (cv:resize frame img)
        (cepl.types::%memcpy
         (cepl:pointer *car*)
         (cffi:foreign-slot-value
          img
          '(:struct cv::ipl-image)
          'cv::image-data)
         1800)))))

(defparameter *step*
  (make-stepper (seconds .5) (seconds 1)))

(defvar *car* nil) (defvar *tex* nil) (defvar *sam* nil)

(defun initialize ()
  ;;(setf (clear-color) (v! .2 .2 .2 0))
  (unless *car*
    (setf *car* (make-c-array nil
                              :dimensions '(30 30)
                              :element-type :short))
    (setf *tex* (make-texture *car* :element-type :rgb))
    (setf *sam* (cepl:sample *tex*))))

(defun-g vert ((vert g-pnt))
  (let ((pos (pos vert)))
    (values (v! pos 1)
            (+ .5 (* .5 pos)))))

(defun-g frag ((uv :vec3) &uniform (tarr :sampler-2d) (resolution :vec2))
  (let ((color (texture tarr (s~ uv :xy))))
    color))

(defun-g frag ((uv :vec3) &uniform (tarr :sampler-2d) (resolution :vec2))
  (v! 1 1 1 0))

(defpipeline-g pipe ()
  (vert g-pnt)
  (frag :vec3))

(defun draw! ()  
;;   (when (funcall *step*)
;;     (push-g *car* (texref *tex*))
;; ;;    (show-video "/home/sendai/videos/sally.mp4" 20)
;;     )
  
  (let ((res (surface-resolution (current-surface))))
    (setf (resolution (current-viewport))
          res)
    (as-frame
      (map-g #'pipe (get-quad-stream-v2)
             :tarr *sam*
             :resolution res))))

(def-simple-main-loop runplay (:on-start #'initialize)
  (draw!))
