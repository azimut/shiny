(in-package :shiny)

(defvar *outsiders* nil)
(defvar *actors* nil)
(defvar *lead* nil)

(defun delete-actor-class (class-name)
  (declare (string class-name))
  (setf *actors*
        (delete-if (lambda (x) (string= class-name (class-name (class-of x))))
                   *actors*)))

(defmethod sync (x) (+ .5 (* .5 (sin x))))
(defmethod cync (x) (+ .5 (* .5 (cos x))))

(defgeneric update (actor))
(defmethod update (actor))

(defun update-all-the-things (l)
  (declare (list l))
  (loop :for actor :in l :do
     (update actor)))

(defun model->world (actor)
  (with-slots (pos rot) actor
      (m4:* (m4:translation pos)
            (q:to-mat4 rot))))

(defclass actor ()
  ((pos :initarg :pos :initform (v! 0 0 0) :accessor pos)
   (rot :initarg :rot :initform (v! 0 0 0) :accessor rot)
   (buf :initarg :buf :initform (box))))

(defclass lead   (actor) ())
(defclass voz    (actor) ())
(defclass sphere (actor) ())
(defclass wall   (actor) ())
(defclass ground (actor) ())
(defclass portal (actor)
  ((buf :initform (box))))
(defclass planet (actor)
  ((buf :initform (sphere 5))
   (pos :initform (v! 0 50 0))))

(defun make-planet ()
  (let ((planet (make-instance 'planet)))
    (push planet *actors*)
    planet))

(defun make-portal ()
  (let ((portal (make-instance 'portal)))
    (push portal *outsiders*)
    portal))

(defun make-ground ()
  (let ((ground
         (make-instance
          'ground
          :pos (v! 0 -50 0)
          :buf (lattice))))
    (push ground *actors*)
    ground))

(defun make-lead ()
  (let ((lead
         (make-instance
          'lead
          :buf (cone 1f0 3f0) :pos (v! 0 30 -50)
          :rot (q:from-axis-angle (v! 0 0 1) (radians 90)))))
    (push lead *actors*)
    lead))

(defun make-sphere ()
  (push (make-instance 'sphere :buf (sphere 200))
        *actors*))

(defun make-voz ()
  (push (make-instance
         'voz
         :buf (cone 1f0 3f0)
         :pos (v! 0 30 0)
         :rot (q:from-axis-angle (v! 0 0 1)
                                 (radians 90)))
        *actors*))

(defun make-wall ()
  (push (make-instance 'wall :buf (box 5 5 .2))
        *actors*))

(defmethod update ((actor lead))
  (setf (rot actor) (q:from-axis-angle (v! 1 0 0) (radians 90)))
  (setf (pos actor) (v! 0 100 200))
  ;; (setf (pos actor) (v! (* 20 (sync (* 2 (mynow))))
  ;;                       (* 10 (cync (mynow)))
  ;;                       (* 100 (sync (* .5 (mynow))))))
  )

(defmethod update ((actor voz))
  ;; (setf (pos actor) (v! 0 0 0))
  ;; (setf (rot actor) (q:from-axis-angle (v! 0 1 0)
  ;;                                      (radians 90)))
  )
(defmethod update ((actor sphere))
  ;; (setf (rot actor) (v! 0 0 0))
  ;; (setf (pos actor) (v! 0 0 0))
  ;;(setf (rot actor) (v! 0 0 0))
  ;; (setf (rot actor)
  ;;       (q:from-axis-angle (v! 0 1 0)
  ;;                          (radians 90)))
  )
(defmethod update ((actor wall))
  ;; (setf (pos actor) (v! 0 0 0))
  ;; (setf (rot actor) (v! 0 0 0))
  ;;(setf (pos actor) (v! 0 (* 2 (sin (mynow))) 0))
  ;; (setf (rot actor) (q:from-axis-angle
  ;;                    (v! 0 1 0)
  ;;                    (radians (* 360 (sync (mynow))))))
  )

(defmethod update ((actor portal))
  ;;(setf (rot actor) (v! 0 0 0))
  (setf (rot actor)
        (q:from-axis-angle
         (v! 1 1 1)
         (radians (mod (* .01 (get-internal-real-time)) 360))))

  ;; (setf (rot actor)
  ;;       (q:from-axis-angle (v! 1 1 0)
  ;;                          (radians (mod (* .01 (get-internal-real-time)) 360))))
  )
