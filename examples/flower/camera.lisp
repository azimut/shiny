(in-package :shiny)

(defclass camera ()
  ((pos  :initarg :pos  :accessor pos)
   (rot  :initarg :rot  :accessor rot)
   (near :initarg :near :accessor near)
   (far  :initarg :far  :accessor far)
   (frame-size :initarg :frame-size
               :accessor frame-size)
   (buf :initarg :buf))
  (:default-initargs
   :pos (v! 0 0 0)
   :rot (q:identity)
   :near .1
   :far 400f0
   :frame-size nil
   :buf (box)))

(defclass orth (camera) ())
(defclass pers (camera)
  ((fov :initarg :fov :accessor fov))
  (:default-initargs
   :fov 60f0))

(defparameter *camera* (make-instance 'pers))
(defparameter *camera1* (make-instance 'orth))
(defparameter *camera-cubemap*
  (make-instance 'pers :fov 90f0))

(defparameter *currentcamera* *camera*)

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

(defmethod update ((camera orth))
  (setf (pos camera) (v! 0 0 0))
  (setf (frame-size camera) (v2! 30))
;;  (setf (rot camera) (v! 0 0 0))
  (setf (rot camera)
        ;; TOP
        (q:from-axis-angle (v! 1 0 0)
                           (radians -90))
        ;; FRONT
        ;; (q:from-axis-angle (v! 1 0 0)
        ;;                    (radians -45))
        ))

(defmethod update ((camera pers))
  (let ((time (mynow)))
    (with-slots (pos rot) camera
      ;;(setf rot (q:identity))
      ;; (setf rot (q:*
      ;;            (q:from-axis-angle (v! 1 0 0) (radians -15))
      ;;            (q:from-axis-angle (v! 0 1 0)
      ;;                               (radians (* 20 (sin (* .2 time)))))))
      ;; (setf rot (q:from-axis-angle
      ;;            (v! 0 1 0)
      ;;            (radians (* 90 0))))
      ;; (setf pos (v! (+ 0 (* 0 (cos (* .5 time))))
      ;;               (+ 0 (* .1 (sin (* .5 time))))
      ;;               (+ 0 (* 1 (cos (* .5 time))))))
      ;;(setf pos (v! 0 0 0))
      ;;(setf rot (q:look-at (v! 0 1 0) pos (v! 0 -1 0)))
      )))
