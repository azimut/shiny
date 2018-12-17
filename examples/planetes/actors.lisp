(in-package :shiny)

(defvar *actors* nil)
(defparameter *light-pos* (v! -60 100 -50)
  ;;(v! 0 10 -40)
  )
(defparameter *light-color* (v! .9 .9 .9))
(defparameter *sun-intensity* 10f0)
(defparameter *exposure* .5f0)
(defparameter *radius-planet* 16000000f0)

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
   (scale :initarg :scale :initform 1f0)))

(defclass pbr (actor)
  ((albedo    :initarg :albedo)
   (ao        :initarg :ao)
   (height    :initarg :height)
   (normal    :initarg :normal)
   (roughness :initarg :roughness)))

(defclass celestial-sphere (actor)
  ((buf :initform (sphere 20 20 20))))
(defclass piso (actor)
  ((buf :initform (lattice 50 50 2 2))
   (tex :initform (get-tex "static/checker.dds"))))


(defclass box (actor)
  ((buf :initform (box 2 2 2))))



(defclass sphere (actor)
  ((buf :initform (sphere 1))))


(defun make-pbr (&optional (pos (v! 0 0 0)) (rot (q:identity)))
  (let ((obj
         (make-instance
          'pbr
          :buf (lattice 200 200 2 2 t)
          :pos pos
          :rot rot
          :albedo (get-tex "static/32.Rock01-1k/rock01_albedo.jpg" nil t :rgb8)
          :ao (get-tex "static/32.Rock01-1k/rock01_ao.jpg" nil t :r8)
          :height (get-tex "static/32.Rock01-1k/rock01_height.jpg" nil t :r8)
          :normal (get-tex "static/32.Rock01-1k/rock01_normal.jpg" nil t :rgb8)
          :roughness (get-tex "static/32.Rock01-1k/rock01_roughness.jpg" nil t :r8))))
    (push obj *actors*)
    obj))

(defclass planet (actor)
  ((buf :initform (sphere))
   (albedo :initform (get-tex "static/16.Plasterwall02-1k/plasterwall02_albedo.jpg" nil t :rgb8))
   (ao :initform (get-tex "static/16.Plasterwall02-1k/plasterwall02_ao.jpg" nil t :r8))
   (height :initform (get-tex "static/16.Plasterwall02-1k/plasterwall02_height.jpg" nil t :r8))
   (normal :initform (get-tex "static/16.Plasterwall02-1k/plasterwall02_normal.jpg" nil t :rgb8))
   (roughness :initform (get-tex "static/16.Plasterwall02-1k/plasterwall02_roughness.jpg" nil t :r8))))

(defun make-planet (&optional (pos (v! 0 0 0))
                      (scale 1f0)
                      (rot (q:identity)))
  (let ((obj
         (make-instance
          'planet
          :pos pos
          :rot rot
          :scale scale)))
    (push obj *actors*)
    obj))

(defun make-celestial-sphere ()
  (let ((obj (make-instance 'celestial-sphere)))
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

(defun make-sphere (&optional
                      (pos (v! 0 0 0))
                      (scale 1f0))
  (let ((obj (make-instance 'sphere
                            :pos pos
                            :scale scale)))
    (push obj *actors*)
    obj))
(defun make-piso (&optional (pos (v! 0 0 0)) (rot (q:identity)))
  (let ((obj (make-instance 'piso :pos pos :rot rot)))
    (push obj *actors*)
    obj))

(defgeneric update (actor))
(defmethod update (actor))
(defmethod update ((actor pbr))
  ;;(setf (pos actor) (v! 0 -2 0))

  )
(defmethod update ((actor sphere))
  (with-slots (pos) actor
    (let* ((old-pos pos)
           (time (* 1f0 (mynow)))
           (new-pos (v! (+ -4  (* 4 (sin time)))
                        0
                        (+ 60 (- (* 10 (cos time))))))
           ;;(new-pos (v! 0 0 0))
           )
      (setf pos new-pos)
      (setf *light-pos* new-pos))))

(defmethod update ((actor celestial-sphere))
  ;;(setf (pos actor) (v! 0 0 0))
  )
(defmethod update ((actor planet))
  (with-slots (pos name) actor
    (v3:incf pos (v! 0 0 .5))
    (setf *light-pos* pos)
    (if (or (> (z pos) 100)
            (> (y pos) 30))
        (progn
          (v3:decf pos (v! 0 .5 0))
          ;;(delete-actor-name name)
          )))
  ;; (with-slots (pos scale) actor
  ;;   (setf pos (v! (* -60 (cos (mynow))) (* 100 (sin (mynow))) -100))
  ;;   ;;(setf pos (v! 0 0 10))
  ;;   (setf *light-pos* pos)
  ;;   (setf scale 10f0))
  )

(defmethod update ((actor box))
  (with-slots (pos name) actor
    (v3:incf pos (v! 0 0 .5))
    (if (or (> (z pos) 100)
            (> (y pos) 30))
        (progn
          (v3:decf pos (v! 0 .5 0))
          ;;(delete-actor-name name)
          ))))


