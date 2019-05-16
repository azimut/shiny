(in-package :shiny)

(defparameter *mf*
  "/home/sendai/Downloads/Radiohead_-_Daydreaming.mid")

(defparameter *notes*  (subseq (get-notes *mf*) 0 29))
(defparameter *notes*  (subseq (get-notes-durations *mf*) 30 60))
(defparameter *mnotes* (cm:markov-analyze (mapcar #'car *notes*) :order 2))

(defparameter *notes*  (subseq (get-notes *mf*) 0 29))
(defparameter *mnotes*
  (cm:markov-analyze
   (get-notes-durations-chords-silences *mf* 1) :order 2))

(defun f ())

(defparameter *metre* (make-metre '(4) 1))
(defparameter *notes*  (subseq (get-notes *mf*) 3 30))

(let ((notes (make-cycle *notes*)))
  (defun f (time &optional (beat 0))
    (let ((n (next notes))
          (ps (odds .2))
          (r .5))
      (p time (+ -12 n) (rcosr 60 5 5) (+ 1 r) 0)
      (when (and (not ps) (= 1 (funcall *metre* beat)))
        (destructuring-bind (n d) (next *mnotes*)
          (p time (cm:transpose n +24) 70 d 1)))
      ;;(and ps (play-sand (+ 12 n) 2 :amp (rcosr 100 50 5)))
      (aat (+ time #[r b]) #'f it (+ .5 beat)))))

(f (now))
(fp 0 0)
(fp 0 33)
(fp 1 1)
(fg 1f0)
(defun f ())
(defparameter *notes* (subseq (get-notes *mf*) 1020 1100))
(defparameter *metre* (make-metre '(8) 1))
(let ((notes (make-cycle *notes*)))
  (defun f (time &optional (beat 0))
    (let ((n (next notes))
          (ps (odds .2))
          (r .3))
      (p time (+ -12 n) (rcosr 60 5 5) (+ 1 r) 0)
      (when (and (not ps) (= 1 (funcall *metre* beat)))
        (p time (cm:transpose (car (next *mnotes*)) +24) 70 4 1))
      ;;(and ps (play-sand (+ 12 n) 1 :amp (rcosr 350 50 5)))
      (aat (+ time #[r b]) #'f it (+ .5 beat)))))

(aat (tempo-sync #[1 b]) #'f it)
(defun f ())
