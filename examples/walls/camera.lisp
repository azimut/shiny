(defclass camera ()
  ((pos :initarg :pos :initform (v! 0 0 0)   :accessor rot)
   (rot :initarg :rot :initform (q:identity) :accessor pos)
   (near :initarg :near :initform .1 :accessor near)
   (far :initarg :far :initform 400f0 :accessor far)
   (frame-size
    :initarg :frame-size :initform nil :accessor frame-size)))

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
  (setf (pos camera)
        (v! 0 30 0))
  (setf (frame-size camera) (v! 300 300))
;;  (setf (rot camera) (v! 0 0 0))
  (setf (rot camera)
        ;; (q:*
        ;;  (q:from-axis-angle (v! 0 1 0)
        ;;                     (radians 180)))
        (q:from-axis-angle (v! 1 0 0)
                           (radians -45))))

(defmethod update ((camera pers))
  (setf (pos camera) (v! 0 0 20))
  ;;  (setf (rot camera) (v! 0 0 0))
  (setf (rot camera) (q:from-axis-angle (v! 1 0 0) (radians -10)))
  ;; (setf (rot camera)
  ;;       ;; (q:*
  ;;       ;;  (q:from-axis-angle (v! 0 1 0)
  ;;       ;;                     (radians 180)))
  ;;       (q:from-axis-angle (v! 1 0 1)
  ;;                          (radians 30)))
  ;;  (setf (pos camera) (v! 30 10 70))
  ;; (setf (pos camera) (v! 0 2 10))
  ;; (setf (rot camera) (v! 0 0 0))  
  ;; (setf (rot camera)
  ;;        (q:* (q:from-axis-angle
  ;;              (v! 0 1 0)
  ;;              (radians 180))
  ;;             (q:from-axis-angle
  ;;              (v! 1 -1 0)
  ;;              (radians -45))))
  )


(defvar *light-camera* (make-instance 'orth))
(setf (pos *light-camera*) (v! 0 5 0))
(setf (rot *light-camera*) (q:from-axis-angle (v! 1 0 0)
                                              (radians -55)))
(setf (frame-size *light-camera* (v! 20 20)))

(defvar *portal-camera* (make-instance 'pers))
(setf (pos *portal-camera*) (v! 0 100 200))
(setf (rot *portal-camera*) (q:from-axis-angle (v! 1 0 0) (radians -10)))
(setf (frame-size *portal-camera*) (v! 30 30))
