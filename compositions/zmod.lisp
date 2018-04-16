(in-package :somecepl)

;; Note: change repeat beat

(defun f (time notes)
  (when (zmodt 1)
    (p time (car notes) 80 1 1)
    (setf notes (rotate notes -1)))
  (when (zmodt 2)
    (p time (cadr notes) 50 2 2))
  (aat (+ time #[1 b]) #'f it notes))

(defun f (time notes)
  (when (zmodt 1)
    (p time (car notes) 80 1 1)
    (setf notes (cdr notes)))
  (when (zmodt 2)
    (p time (cadr notes) 50 2 0)
    (p time (cadr notes) 50 2 2))
  (unless notes
    (setf notes (make-chord 60 90 3 *phrygian*)))
  (when notes
    (aat (+ time #[1 b]) #'f it notes)))


(defun f (time notes)
  (when (zmodt 1)
    (p time (car notes) 80 1 1)
    (setf notes (cdr notes)))
  (when (zmodt 2)
    (p time (cadr notes) 50 2 0)
    (p time (cadr notes) 50 2 2))
  (when (zmodt 4)
    (p time (- 12 (cadr notes)) 60 2 9))
  (unless notes
    (setf notes (make-chord 60 90 3 *phrygian*)))
  (when notes
    (aat (+ time #[1 b]) #'f it notes)))

(f (now) (make-chord 60 70 3 *phrygian*))

(flush-pending)
(off-with-the-notes *synth*)
;; violin
(fluidsynth:program-change *synth* 2 40)

(defvar *r* nil)

(defun f (time notes)
  (when (zmodt 1)
    (p time (car notes) 80 1 1)
    (setf notes (cdr notes)))
  (when (zmodt 2)
    (when (cadr notes) (setf *r* (cadr notes)))
    (p time (cadr notes) 50 (pick 2 3) 2))
  (unless notes
    (setf notes (make-chord 60 90 3 *phrygian*)))
  (when notes
    (aat (+ time #[1 b]) #'f it notes)))

(defun g (time)
  (when (and *r* (zmodt 1))
    (p time (qcosr *phrygian* *r* 7 1/2) 80 .5 3))
    (aat (+ time #[.5 b]) #'g it))

(g (now))

(flush-pending)
(off-with-the-notes *synth*)

(fluidsynth:program-change *synth* 3 24)
