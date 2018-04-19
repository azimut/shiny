(in-package :somecepl)

(setf (bpm *tempo*) 200)

;; Note: change repeat beat

(setf (fluidsynth:setting *fluid-settings* "synth.gain") .2)

(flush-pending)
(off-with-the-notes *synth*)

(loop :for x :from 10 :to 30
   :do (fluidsynth:program-change *synth* x (pick 52 53)))

(defun f (time notes)
  (when (zmodt 1)
    (p time (car notes) 80 1 1)
    (setf notes (rotate notes -1)))
  (when (zmodt 2)
    (p time (cadr notes) 50 10 (+ 10 (random 20))))
  (aat (+ time #[1 b]) #'f it notes))

(defun f (time notes)
  (when (zmodt 1)
    (p time (car notes) 80 1 1)
    (setf notes (rotate notes -1)))
  (when (zmodt 2)
    (p time (cadr notes) 50 2 3))
  (aat (+ time #[1 b]) #'f it notes))

;; violin
(fluidsynth:program-change *synth* 2 40)

(defun f (time notes)
  (when (zmodt 1)
    (p time (car notes) 80 1 1)
    (setf notes (cdr notes)))
  (when (zmodt 2)
    (p time (cadr notes) 50 2 3)
    (p time (cadr notes) 50 5 2)
    )
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
    (p time (cadr notes) 50 5 2))
  (unless notes
    (setf notes (make-chord 60 90 3 *phrygian*)))
  (when (zmodt 6)
    (p time (- (car notes) 24) 60 2 4))
  (when notes
    (aat (+ time #[1 b]) #'f it notes)))

(f (now) (make-chord 60 70 3 *phrygian*))

(flush-pending)
(off-with-the-notes *synth*)

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
    (p time (qcosr *phrygian* *r* 7 3/2) 80 .5 3))
    (aat (+ time #[.5 b]) #'g it))

(g (now))

(flush-pending)
(off-with-the-notes *synth*)

(fluidsynth:program-change *synth* 3 24)
