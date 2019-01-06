(in-package :shiny)
(defparameter *exposure* .9f0)
(defvar *scale* 1f0)
(defvar *color* (v! .3 .3 .3))
(defvar *rough* 1f0)
(defvar *pointlight-pos* (v! 0 0 0))

(defvar *actors* nil)
(defparameter *light-pos* (v! 0 1000 3000))
(defparameter *light-color* (v! .9 .9 .9))
(defparameter *exposure* 1f0)

(defparameter *parallax-scale* .01f0)

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

;;--------------------------------------------------

(defclass actor ()
  ((name  :initarg :name)
   (pos   :initarg :pos :accessor pos)
   (rot   :initarg :rot :accessor rot)
   (buf   :initarg :buf)
   (color :initarg :color)
   (scale :initarg :scale))
  (:default-initargs
   :name (gensym)
   :pos (v! 0 0 0)
   :rot (q:identity)
   :buf (box)
   :color (v! 1 1 1)
   :scale 1f0))

(defclass cubemap (actor) ())
(defun make-cubemap ()
  (let ((obj (make-instance 'cubemap)))
    (push obj *actors*)
    obj))

(defclass pbr (actor)
  ((albedo    :initarg :albedo)
   (ao        :initarg :ao)
   (height    :initarg :height)
   (normal    :initarg :normal)
   (roughness :initarg :roughness)
   (uv-repeat :initarg :uv-repeat)
   (uv-speed  :initarg :uv-speed)
   (metallic  :initarg :metallic))
  (:default-initargs
   :uv-repeat 1f0
   :uv-speed .1
   :metallic .1
   :albedo (get-tex "static/2048/pjEmy_4K_Albedo.jpg" nil t :rgb8)
   :ao (get-tex "static/2048/pjEmy_4K_AO.jpg" nil t :r8)
   :height (get-tex "static/2048/pjEmy_4K_Displacement.jpg" nil t :r8)
   :normal (get-tex "static/2048/pjEmy_4K_Normal.jpg" nil t :rgb8)
   :roughness (get-tex "static/2048/pjEmy_4K_Roughness.jpg" nil t :r8)))

(defclass piso (pbr) ())
(defclass thing (pbr) ())

(defun make-piso (&optional (pos (v! 0 0 0)) (rot (q:identity))
                    (uv-speed 0f0))
  (let ((obj
         (make-instance
          'piso
          :buf (lattice 100 100 2 2 t)
          :pos pos
          :uv-speed uv-speed
          :uv-repeat 10f0
          :rot rot)))
    (push obj *actors*)
    obj))

(defun make-thing (&optional (pos (v! 0 0 0)) (rot (q:identity)))
  (let ((obj
         (make-instance
          'pbr-simple
          :buf (box 1 2 .3 t)
          :pos pos
          :rot rot)))
    (push obj *actors*)
    obj))

(defclass pbr-simple (actor)
  ((roughness :initarg :roughness)
   (metallic  :initarg :metallic))
  (:default-initargs
   :roughness .1
   :metallic  .1))
(defun make-pbr-simple (&optional (pos (v! 0 0 0)))
  (let ((obj
         (make-instance
          'pbr-simple
          :buf (sphere)
          :pos pos)))
    (push obj *actors*)
    obj))

(defclass box (actor)
  ((buf :initform (box 2 2 2))))
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
(defparameter *uvs* -.2)
(defmethod update ((actor pbr))
  (with-slots (pos uv-repeat uv-speed) actor
    ;;(setf pos (v! 0 -2 0))
    (setf uv-speed *uvs*)
    ;;(setf uv-repeat 1f0)
    )
  )
(defvar *rotcube* 1f0)
(defmethod update ((actor pbr-simple))
  (with-slots (pos rot color roughness metallic) actor
    (setf pos (v! 0 0 -4))
    (setf metallic .9)
    (setf color (v! .03 .03 .03))
    (setf roughness .8)
    ;;(setf color (v! .01 .01 .01))
    (setf rot (q:from-axis-angle
               (v! 1 .2 .8)
               (radians (* *rotcube* (mod (* 10 (mynow)) 360)))))
    ;; (setf pos (v! 0 0 (+ -10  (- (* 5 (sin (mynow)))))))
    ))
(defmethod update ((actor box)))


