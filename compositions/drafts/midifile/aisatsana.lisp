(in-package :shiny)

(defvar *notes*   NIL)
(defvar *sing*    NIL)
(defvar *quarter* NIL)
(defvar *mf*      NIL)

(setf (bpm *tempo*) 64)
(setf *quarter* .23)
(setf *mf* "/home/sendai/Downloads/aisatsana_by_Aphex_Twin.mscz.mid")
(setf *notes*
      (subseq (get-measures-pair *mf* 0 12 (* *quarter* 4) :seconds nil) 4))
(setf *sing*
      (subseq (get-measures-pair *mf* 1 24 (* *quarter* 4) :seconds nil) 12))

(let* ((c  (make-cycle *notes*))
       (cc (make-cycle *sing*)))
  (defun f (time)
    (let* ((nn (next c))
           (n (first  nn))
           (d (second nn)))
      (pa time n 40 d 1 (d2o d)))
    (let* ((nn (next cc))
           (n (first  nn))
           (d (second nn)))
      (pa time n 40 d 1 (d2o d)))
    (aat (+ time #[(* 4 *quarter*) b]) #'f it)))

(aat (tempo-sync #[1 b]) #'f it)
(fp 1 40)
(defun f ())
