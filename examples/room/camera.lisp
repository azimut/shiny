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
(defparameter *camera1* (make-instance 'orth))
(defparameter *cameras* (list *camera* *camera1*))
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
(defmethod update ((camera orth))
  (setf (pos camera) (v! 0 0 0))
  ;;(setf (frame-size camera) (v2! 10))
  (setf (rot camera)
        ;; TOP
        (q:from-axis-angle (v! 1 0 0)
                           (radians -90))
        ;; FRONT
        ;; (q:from-axis-angle (v! 1 0 0)
        ;;                    (radians -45))
        ))
}
(defmethod update ((camera pers))
  (let ((time (mynow)))
    (with-slots (pos rot) camera
      ;;(setf rot (q:identity))
      (setf pos (v! 0 0 0))
      (setf rot (q:*
                 (q:from-axis-angle (v! 1 0 0)
                                    (radians -10))
                 ;;(q:identity)
                 (q:from-axis-angle (v! .2 .9 0)
                                    (radians (* 20
                                                (sin (* .1 time)))))
                 ))
      ;; (setf rot (q:from-axis-angle
      ;;            (v! 0 1 0)
      ;;            (radians (* 90 0))))
      ;; (setf pos (v! (+ 0 (* 0 (cos (* .5 time))))
      ;;               (+ 0 (* .1 (sin (* .5 time))))
      ;;               (+ 0 (* 1 (cos (* .5 time))))))
      ;;(setf rot (q:look-at (v! 0 1 0) pos (v! 0 -1 0)))
      )))
