(in-package :shiny)

(defclass camera ()
  ((pos :initarg :pos :initform (v! 0 0 0)   :accessor rot)
   (rot :initarg :rot :initform (q:identity) :accessor pos)
   (near :initarg :near :initform .1 :accessor near)
   (far :initarg :far :initform 400f0 :accessor far)
   (frame-size
    :initarg :frame-size :initform nil :accessor frame-size)
   (buf :initform (box))))

(defclass orth (camera)
  ((tex :initform (get-tex "static/LetterO.jpg"))))
(defclass pers (camera)
  ((fov :initform 60f0 :accessor fov)
   (tex :initform (get-tex "static/LetterP.jpg"))))

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
      (rtg-math.projection:perspective
       (x fs)
       (y fs)
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
  (setf (frame-size camera) (v2! 5))
;;  (setf (rot camera) (v! 0 0 0))
  (setf (rot camera)
        ;; TOP
        (q:from-axis-angle (v! 1 0 0)
                           (radians -90))
        ;; FRONT
;        (q:identity)
        ;; (q:from-axis-angle (v! 1 0 0)
        ;;                    (radians -45))
        ))

(defparameter *crotate* nil)
(defparameter *head* nil)
(defparameter *wave* 1f0)

(defparameter *cam-vector* (v! 1 0 0))
(defmethod update ((camera pers))
  ;;(setf (pos camera) (v! 0 2 9))
  ;;(setf (far camera) 100f0)
  (setf (pos camera) (v! 0 0 3))
  ;;n(setf (rot camera) (q:from-axis-angle *cam-vector* (radians 90)))
  (setf (rot camera)
        (q:from-axis-angle *cam-vector*
                           ;;(radians 90)
                           (radians (mod (* 20 (mynow)) 360))
                           ;;(radians (+ 130 (mod (* 20 (mynow)) 90)))
                           )
        )
  )



