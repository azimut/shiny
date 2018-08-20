(in-package :somecepl)

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

(defun model->world (actor)
  (with-slots (pos rot) actor
      (m4:* (m4:translation pos)
            (q:to-mat4 rot))))

(defun world->view (camera)
  (m4:* (m4:translation (v3:negate (pos camera)))
        (q:to-mat4 (q:inverse (rot camera)))))

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
     (* .1 width)
     (* .1 height)
     (near camera)
     (far camera))))

(defvar *actors* nil)
(defclass actor ()
  ((pos :initarg :pos :initform (v! 0 0 0))
   (rot :initarg :rot :initform (v! 0 0 0))
   (buf :initarg :buf :initform (box))))

;;--------------------------------------------------

(defclass voz (actor) ())
(defclass piso (actor) ())
(defclass dolly (actor) ())

(defparameter *trigger* (make-trigger 20))
(defgeneric update (actor))

;; y - altura


(defmethod update ((camera orth))
  ;; (setf (pos camera)
  ;;       (v! 0 5 0))
  ;; (setf (rot camera)
  ;;       (q:from-axis-angle (v! 1 0 0) (radians -90)))
  )

(defmethod sync (x)
  (+ .5 (* .5 (sin x))))
(defmethod cync (x)
  (+ .5 (* .5 (cos x))))

(defmethod update ((camera pers))
  (setf (pos camera) (v! 0 2 10))
  (setf (rot camera) (v! 0 0 0))
   ;; (setf (rot camera) (q:from-axis-angle (v! 1 0 0)
   ;;                                       (radians -35)))
   (setf (rot camera)
         (q:from-axis-angle
          (v! 1 1 1)
          (radians (mod (* (+ 20  (funcall *trigger* 'shoot))
                           (mynow))
                        360))))
  )

(defmethod update ((actor dolly))
  ;; (let* ((time (* .001 (get-internal-real-time)))
  ;;        (st (sync time))
  ;;        (newpos (v! (+ -10  (* 20 (cync time)))
  ;;                    2
  ;;                    (+ -10  (* 20 st)))))
  ;;   (with-slots (pos) actor
  ;;     (setf pos newpos)))
  )

(defmethod update ((actor piso))
  (with-slots (pos) actor
    (setf pos (v! 0 -5 0))))

(defmethod update ((actor voz))
  (with-slots (pos rot) actor
    (setf pos (v! 0
                  (+ .5 (* .5 (sin (* .001 (get-internal-real-time)))))
                  0))
    (setf rot (v! 0 0 0))
    (setf rot (q:from-axis-angle
               (v! -1 1 1)
               (radians (* 360 (+ .5 (* .5 (cos (* .001 (get-internal-real-time)))))))))
    ))

(defgeneric draw (actor res))

(defmethod draw ((actor piso) res)
  (with-setf:with-setf (cull-face) :front
    (with-slots (buf) actor
      (map-g #'white buf
             :time (mynow)
             :model-world (model->world actor)
             :world-view (world->view *currentcamera*)
             :view-clip  (projection *currentcamera*
                                     (x res)
                                     (y res))))))

(defvar *instances* 185)
(defmethod draw ((actor dolly) res)
  (with-slots (buf) actor
    (with-instances *instances*
      (map-g #'dollypipe buf
             :time (mynow)
             :sound *ubo*
             :model-world (model->world actor)
             :world-view (world->view *currentcamera*)
             :view-clip  (projection *currentcamera*
                                     (x res)
                                     (y res))))))

(defmethod draw ((actor voz) res)
  (with-slots (buf) actor
    (map-g #'pipe buf
           :time (mynow)
           :sound *ubo*
           :model-world (model->world actor)
           :world-view (world->view *currentcamera*)
           :view-clip  (projection *currentcamera*
                                   (x res)
                                   (y res)))))
