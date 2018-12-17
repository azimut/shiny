(in-package :shiny)

(defclass camera ()
  ((pos :initarg :pos :initform (v! 0 0 0)   :accessor pos)
   (rot :initarg :rot :initform (q:identity) :accessor rot)
   (near :initarg :near :initform .1 :accessor near)
   (far :initarg :far :initform 400f0 :accessor far)
   (frame-size
    :initarg :frame-size :initform nil :accessor frame-size)
   (buf :initform (box))))

(defclass orth (camera) ())
(defclass pers (camera)
  ((fov :initform 60f0 :accessor fov)))

(defparameter *camera* (make-instance 'pers))
(defparameter *camera1* (make-instance 'orth))
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
                           (radians -40))
        ;; FRONT
        ;; (q:from-axis-angle (v! 1 0 0)
        ;;                    (radians -45))
        ))

(defparameter *crotate* nil)
(defparameter *head* nil)
(defparameter *wave* 1f0)

(defparameter *cam-vector* (v! 1 0 0))
(defmethod update ((camera pers))
  (let ((time (mynow))) (with-slots (pos rot) camera
     ;;(setf rot (q:identity))
     (setf rot (q:*
                (q:from-axis-angle (v! 1 0 0) (radians 10))
                (q:from-axis-angle (v! 0 1 0)
                                   (radians (* 20 (sin (* .2 time)))))))
     ;;(setf rot (q:from-axis-angle (v! 0 0 1) (radians 80)))
     (setf pos (v! (+ -10 (* 0 (cos (* .5 time))))
                   (+ 22 (* .1 (sin (* .5 time))))
                   (+ 100 (* 1 (cos (* .5 time))))
                   ))
     ;;(setf pos (v! 0 0 0))
     ;;(setf rot (q:look-at (v! 0 1 0) pos (v! 0 -1 0)))
     )))
