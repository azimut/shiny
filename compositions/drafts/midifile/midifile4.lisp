(in-package #:shiny)

(progn
  (defparameter *mf* "/home/sendai/Downloads/Minecraft_Theme_Sweden_Calm.mid")
  (defparameter *bass*  (get-measures-pair *mf* 0 4 2))
  (defparameter *notes* (get-measures-pair *mf* 1 4 2)))

(setf (bpm *tempo*) 42)

(let ((measures (make-cycle *notes*))
      (bass     (make-cycle *bass*)))
  (defun f (time)
    (let* ((measure   (next measures))
           (notes     (first measure))
           (durations (second measure)))
      (pa time notes 50 durations 0 (d2o durations)))
    (let* ((measure   (next bass))
           (notes     (first measure))
           (durations (second measure)))
      (pa time notes 50 durations 0 (d2o durations)))
    (aat (+ time #[2 b]) #'f it)))

(defun f ())
(f (now))


;;--------------------------------------------------

(progn
  (defparameter *mf* "/home/sendai/Downloads/Resurrections_-_Celeste.mid")
  (defparameter *notes*   (subseq (get-measures-pair *mf* 0 4) 2))
  (defparameter *sing*    (get-measures-pair *mf* 1 4))
  (defparameter *quarter* (first (get-midi-tempo *mf*))))

(let ((notes (make-cycle  *notes*))
      (sing  (make-cycle  *sing*)))
  (defun f (time)
    (let* ((measure   (next notes))
           (notes     (first measure))
           (durations (second measure)))
      (pa time notes 40 durations 0 (d2o durations)))
    (let* ((measure   (next sing))
           (notes     (first measure))
           (durations (second measure)))
      (pa time notes 60 durations 1 (d2o durations)))
    (aat (+ time #[ (* *quarter* 4) s]) #'f it)))

(defun f ())

(f (now))
(fg 2f0)
(fp 1 0)
(fp 0 1)
(fp 0 49)
