(in-package :shiny)

(defclass camera ()
  ((pos  :initarg :pos  :accessor pos)
   (rot  :initarg :rot  :accessor rot)
   (near :initarg :near :accessor near)
   (far  :initarg :far  :accessor far)
   (frame-size :initarg :frame-size
               :accessor frame-size))
  (:default-initargs
   :pos (v! 0 0 0)
   :rot (q:identity)
   :near .1
   :far 400f0
   :frame-size nil))

(defclass orth (camera) ())
(defclass pers (camera)
  ((fov :initarg :fov :accessor fov))
  (:default-initargs
   :fov 60f0))

(defparameter *camera* (make-instance 'pers))
(defparameter *camera2* (make-instance 'pers))
(defparameter *camera1* (make-instance 'orth))
(defparameter *cameras* (list *camera* *camera2*))
(defparameter *camera-cubemap*
  (make-instance 'pers :fov 90f0))
(defparameter *currentcamera* *camera*)

(defun next-camera ()
  "rotates current value on *CURRRENTCAMERA*
   for each on *CAMERAS*"
  (setf *cameras* (alexandria:rotate *cameras*))
  (setf *currentcamera* (first-elt *cameras*))
  (values))

(defun world->view (camera)
  (m4:* (m4:translation (v3:negate (pos camera)))
        (q:to-mat4      (q:inverse (rot camera)))))

(defgeneric projection (camera)
  (:method ((camera pers))
    (let ((fs (or (frame-size camera)
                  (viewport-resolution (current-viewport)))))
      (rtg-math.projection:perspective-v2
       fs
       (near camera)
       (far camera)
       (fov camera))))
  (:method ((camera orth))
    (let ((fs (or (frame-size camera)
                  (viewport-resolution (current-viewport)))))
      (rtg-math.projection:orthographic
       (x fs)
       (y fs)
       (near camera)
       (far camera)))))

(defun world->clip (camera)
  (m4:* (projection camera)
        (world->view camera)))
;;--------------------------------------------------
;; UPDATE
;;--------------------------------------------------
(defmethod update ((camera orth)))
(defmethod update ((camera pers))
  (setf (pos camera) (v! 0 0 3))
;;  (setf (rot camera) (q:identity))
  (setf (rot camera)
        (q:from-axis-angle (v! 0 1 0)
                           (radians (* 360 (* .1 (mynow))))))
  )
