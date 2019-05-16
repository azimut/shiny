(in-package :shiny)

(fg 2f0)

(progn
  (defparameter *mf*      "/home/sendai/Downloads/Wet_Hands_Minecraft.mid")
  (defparameter *bass*    (subseq (get-measures-pair *mf* 0 4) 0))
  (defparameter *notes*   (subseq (get-measures-pair *mf* 1 4) 0))
  (defparameter *quarter* (first (get-midi-tempo *mf*))))

(let ((notes (make-cycle *notes*))
      (bass  (make-cycle *bass*)))
  (defun f (time)
    (destructuring-bind (n d) (next notes)
      (pa time n 40 d 0 (d2o d)))
    (destructuring-bind (n d) (next bass)
      (pa time n 40 d 1 (d2o d)))
    (aat (+ time #[(* 4 *quarter*) b]) #'f it)))

(aat (tempo-sync #[1 b]) #'f it)
(defun f ())
