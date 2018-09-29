(in-package :shiny)
(fp 2 1)
(let ((notes (make-cycle (subseq (palin (make-chord-fixed 60 6 (scale 0 'ryukyu))) 2)))
      (rhythm (make-weighting '(.4 (1 .1)))))
  (defun f (time)
    (let ((r (print (next rhythm))))
      (p time (next notes) 60 r 2)
      (aat (+ time #[r b]) #'f it))))

(f (now))
(fg .2)
(defun f ())

(make-chord )
