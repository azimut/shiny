(in-package :somecepl)


;;;
;; at_all.clj
;;;

(defun zipmap (keys vals)
  (let ((a (loop :for k :in keys :for v :in vals
              :collect (cons k v))))
    (alexandria:alist-hash-table a)))

(defun triad (scale root)
  (zipmap '(:i :iii :v)
          (list (funcall scale root)
                (funcall scale (+ root 2))
                (funcall scale (+ root 4)))))

;; NOTE: quot (!
(defun ionian (degree)
  (let* ((interval (mod degree 7))
         (note     (nth interval '(0 2 4 5 7 9 11)))
         (octave   (floor (/ (- degree interval) 7))))
    (+ (* 12 octave) note)))

(defun lower (note) (- note 12))
(defun raise (note) (+ note 12))

(defun with-base (chord)
  (setf (gethash :base chord) (lower (gethash :i chord)))
  chord)

(defvar I (with-base (triad #'ionian 0)))
(defvar II (with-base (triad #'ionian 1)))
(defvar V (with-base (triad #'ionian 4)))
(defparameter progression (list I I II II II V I))

(defvar *base* 60)

(defun even-melody (time notes)
  (play-midi-note time (+ *base* (car notes)) 30 1 1)
  (aat (+ time #[1 b]) #'even-melody it (cdr notes)))

(even-melody (now) (repeat 32 (mapcar #'ionian '(5 4))))
(even-melody (now) (mapcar #'ionian '(2 4 5 4 4 2 4)))
(even-melody (now) (mapcar #'ionian '(-2 1 2 2 1 1 -2 1)))
