(in-package #:shiny)

(defparameter *mf*
  "/home/sendai/Downloads/Minecraft_Theme_Sweden_Calm.mid")

(defvar *notes* nil)
(defvar *sing* nil)
(setf *notes* (subseq (get-measures-pair *mf* 10 2 1) 6))
(let ((measures (make-heap *notes*)))
  (defun f (time)
    (let* ((measure (next measures))
           (notes (first measure))
           (durations (second measure)))
      (play-midi-arp time notes 50 durations 0 (d2o durations)))
    (aat (+ time #[2 b]) #'f it)))

(defun f ())
(f (now))


;;--------------------------------------------------

(defparameter *mf*
  "/home/sendai/Downloads/Resurrections_-_Celeste.mid")

(setf *notes* (subseq (get-measures-pair *mf* 4 2 3) 0))
(setf *sing*  (subseq (get-measures-pair *mf* 4 2 0) 0))

(let ((measures (make-cycle  *notes*))
      (sing     (make-cycle  *sing*)))
  (defun f (time)
    (let* ((measure   (next measures))
           (notes     (first measure))
           (durations (second measure)))
      (play-midi-arp time notes 50 durations 0 (d2o durations)))
    (let* ((measure (next sing))
           (notes   (first measure))
           (durations (second measure)))
      (play-midi-arp time notes 20 durations 0 (d2o durations)))
    (aat (+ time #[2 b]) #'f it)))
(defun f ())
(f (now))
(fg 2f0)
(fp 0 1)
