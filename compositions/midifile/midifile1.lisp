(in-package :shiny)

(defparameter *mf*
  "/home/sendai/Downloads/Radiohead_-_Daydreaming.mid")

(defparameter *notes*  (subseq (get-notes *mf*) 3 30))

(defparameter *mnotes* (cm:markov-analyze (mapcar #'car *notes*) :order 2))
(defparameter *notes*  (subseq (get-notes-duration *mf*) 30 60))



(defparameter *notes*  (subseq (get-notes *mf*) 1000 1160))
(defparameter *mnotes*
  (cm:markov-analyze (get-notes-list *mf* 1) :order 2))

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
        (p time (cm:transpose (print (next *mnotes*)) +24) 50 2 1))
      (and ps (play-sand (+ 12 n) 2 :amp (rcosr 100 50 5)))
      (aat (+ time #[r b]) #'f it (+ .5 beat)))))

(f (now))
(fp 0 0)
(fp 0 33)
(fp 1 69)
(fg 1f0)

(defparameter *notes*  (subseq (get-notes *mf*) 1020 1100))
(defparameter *metre* (make-metre '(8) 1))
(let ((notes (make-cycle *notes*)))
  (defun f (time &optional (beat 0))
    (let ((n (next notes))
          (ps (odds .2))
          (r .3))
      (p time (+ -12 n) (rcosr 60 5 5) (+ 1 r) 0)
      (when (and (not ps) (= 1 (funcall *metre* beat)))
        (p time (cm:transpose (print (next *mnotes*)) +24) 40 4 1))
      (and ps (play-sand (+ 12 n) 1 :amp (rcosr 350 50 5)))
      (aat (+ time #[r b]) #'f it (+ .5 beat)))))
