(in-package :shiny)

(defvar *actors* nil)

(defun update-all-the-things (l)
  (declare (list l))
  (loop :for actor :in l :do
     (update actor)))

(defun model->world (actor)
  (with-slots (pos rot) actor
      (m4:* (m4:translation pos)
            (q:to-mat4 rot))))

(defun delete-actor-class (class-name)
  (declare (string class-name))
  (setf *actors*
        (delete-if (lambda (x) (string= class-name (class-name (class-of x))))
                   *actors*)))

(defmethod sync (x) (+ .5 (* .5 (sin x))))
(defmethod cync (x) (+ .5 (* .5 (cos x))))

(defclass actor ()
  ((pos :initarg :pos :initform (v! 0 0 0) :accessor pos)
   (rot :initarg :rot :initform (v! 0 0 0) :accessor rot)
   (buf :initarg :buf :initform (box))))

(defclass piso (actor)
  ((buf :initform (box 15 .1 15))))
(defclass box (actor)
  ((buf :initform (box 2 2 2))))

(defclass assimp-thing (actor)
  ((stream :initarg :stream)))

(defun make-box ()
  (let ((box (make-instance 'box)))
    (push box *actors*)
    box))

(defun make-piso ()
  (let ((piso (make-instance 'piso)))
    (push piso *actors*)
    piso))

(defgeneric update (actor))
(defmethod update (actor))
(defmethod update ((actor box))
  (setf (rot actor) (q:from-axis-angle (v! 1 0 0)
                                       (radians (* 360 (sync (* .1 (mynow))))))))

(defmethod update ((actor piso))
  (with-slots (pos rot) actor
    (setf pos (v! 0 0 0))))

(defmethod update ((actor assimp-thing))
  (setf (pos actor) (v! 0 0 0))
  (setf (rot actor) (q:from-axis-angle
                     (v! 1 0 0)
                     (radians 90)))
  )
