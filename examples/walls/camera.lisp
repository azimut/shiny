(defclass camera ()
  ((pos  :initarg :pos  :initform (v! 0 0 0) :accessor rot)
   (rot  :initarg :rot  :initform (q:identity) :accessor pos)
   (near :initarg :near :initform .1 :accessor near)
   (far  :initarg :far  :initform 400f0 :accessor far)))

(defclass orth (camera) ())
(defclass pers (camera)
  ((fov :initform 60f0 :accessor fov)))

(defparameter *camera* (make-instance 'pers))
(defparameter *camera1* (make-instance 'orth))
(defparameter *currentcamera* *camera*)

(defun world->view (camera)
  (m4:* (m4:translation (v3:negate (pos camera)))
        (q:to-mat4      (q:inverse (rot camera)))))

(defgeneric projection (camera width height)
  (:method ((camera pers) width height)
    (rtg-math.projection:perspective
     width
     height
     (near camera)
     (far camera)
     (fov camera)))
  (:method ((camera orth) width height)
    (rtg-math.projection:orthographic
     (* .2 width)
     (* .2 height)
     (near camera)
     (far camera))))

(defmethod update ((camera orth))
  (setf (pos camera)
        (v! 0 0 300))
  (setf (rot camera)
        (q:from-axis-angle (v! 1 0 0) (radians -90)))
  )

(defmethod update ((camera pers))
  ;;  (setf (pos camera) (v! 30 10 70))
  (setf (pos camera) (v! 0 2 10))
  (setf (rot camera) (v! 0 0 0))
  ;; (setf (rot camera)
  ;;       (q:from-axis-angle
  ;;        (v! 0 1 1)
  ;;        (radians 40)))
  
  ;; (setf (rot camera)
  ;;        (q:from-axis-angle
  ;;         (v! 1 1 1)
  ;;         (radians (mod (* 20
  ;;                          (mynow))
  ;;                       360))))
   )
