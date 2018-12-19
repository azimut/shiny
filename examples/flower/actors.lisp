(in-package :shiny)

(defvar *actors* nil)
(defparameter *light-pos* (v! -60 100 -50)
  ;;(v! 0 10 -40)
  )
(defparameter *light-color* (v! .9 .9 .9))
(defparameter *exposure* .5f0)

(defun update-all-the-things (l)
  (declare (list l))
  (loop :for actor :in l :do
     (update actor)))

(defun model->world (actor)
  (with-slots (pos rot) actor
      (m4:* (m4:translation pos)
            (q:to-mat4 rot))))

(defun delete-actor-name (actor-name)
  (declare (symbol actor-name))
  (setf *actors*
        (delete-if
         (lambda (x) (eq actor-name (slot-value x 'name)))
         *actors*))
  NIL)

(defun delete-actor-class (class-name)
  (declare (string class-name))
  (setf *actors*
        (delete-if
         (lambda (x) (string= class-name (class-name (class-of x))))
         *actors*))
  NIL)

(defmethod sync (x) (+ .5 (* .5 (sin x))))
(defmethod cync (x) (+ .5 (* .5 (cos x))))

(defclass actor ()
  ((name  :initarg :name  :initform (gensym))
   (pos   :initarg :pos   :initform (v! 0 0 0) :accessor pos)
   (rot   :initarg :rot   :initform (v! 0 0 0) :accessor rot)
   (buf   :initarg :buf   :initform (box))
   (color :initarg :color :initform (v! 1 0 0))
   (scale :initarg :scale :initform 1f0)))

(defclass cubemap (actor) ())
(defun make-cubemap ()
  (let ((obj (make-instance 'cubemap)))
    (push obj *actors*)
    obj))

(defclass pbr-simple (actor) ())

(defclass pbr (actor)
  ((albedo :initarg :albedo)
   (ao     :initarg :ao)
   (height :initarg :height)
   (normal :initarg :normal)
   (roughness :initarg :roughness))
  (:default-initargs
   :albedo (get-tex "static/16.Plasterwall02-1k/plasterwall02_albedo.jpg" nil t :rgb8)
   :ao (get-tex "static/16.Plasterwall02-1k/plasterwall02_ao.jpg" nil t :r8)
   :height (get-tex "static/16.Plasterwall02-1k/plasterwall02_height.jpg" nil t :r8)
   :normal (get-tex "static/16.Plasterwall02-1k/plasterwall02_normal.jpg" nil t :rgb8)
   :roughness (get-tex "static/16.Plasterwall02-1k/plasterwall02_roughness.jpg" nil t :r8)))

(defclass box (actor)
  ((buf :initform (box 2 2 2))))

(defun make-pbr (&optional (pos (v! 0 0 0)) (rot (q:identity)))
  (let ((obj
         (make-instance
          'pbr
          :buf (lattice 100 100 2 2 t)
          :pos pos
          :rot rot)))
    (push obj *actors*)
    obj))

(defun make-pbr-simple (&optional (pos (v! 0 0 0)))
  (let ((obj
         (make-instance
          'pbr-simple
          :buf (box 2 4 .5)
          :pos pos)))
    (push obj *actors*)
    obj))

(defun make-box (&optional (pos (v! 0 0 0)) (scale 1f0))
  (let ((obj (make-instance 'box
                            :pos pos
                            :scale scale
                            :buf (box) ;;(box 3 10 1)
                            )))
    (appendf *actors* (list obj))
    obj))


(defgeneric update (actor))
(defmethod update (actor))
(defmethod update ((actor pbr)))
(defmethod update ((actor pbr-simple))  
  (with-slots (pos rot color) actor
    ;;(setf color (v! .01 .01 .01))
    (setf rot (q:from-axis-angle
               (v! 1 .2 .8)
               (radians (mod (* 10 (mynow)) 360))))
    ;;(setf pos (v! 0 0 -8))
    ))
(defmethod update ((actor box)))


