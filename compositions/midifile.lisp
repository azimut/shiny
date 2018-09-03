(in-package :shiny)
(defparameter *mf*
  "/home/sendai/Downloads/Radiohead_-_Daydreaming.mid")

(defparameter *notes* (get-notes *mf*))

(defparameter *notes* (subseq (get-notes-and-velocity *mf*) 30 60))
(defparameter *mnotes*
    (cm:markov-analyze (mapcar #'car *notes*) :order 3))

(defun f ())
(let ((notes (make-cycle *notes*))
      ;;(notes (make-cycle (mapcar #'car *notes*)))
      ;; (rhythm (make-cycle
      ;;          (mapcar (lambda (x) (coerce x 'single-float))
      ;;                  (mapcar #'cadr *notes*))))
      )
  (defun f (time)
    (let ((n (next notes))
          ;;(n (next *mnotes*))
;;          (r (next rhythm))
          (r .5))
      (p time n 60 r 0)
;;      (p time )
;;      (play-newfm (+ 12 n) 2)
      (aat (+ time #[r b]) #'f it))))

(f (now))

(play-pluck 90 1)
