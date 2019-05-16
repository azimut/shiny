(in-package :shiny)

(fg 2f0)

(progn
  (defparameter *mf*      "/home/sendai/Downloads/April_14th.mid")
  (defparameter *bass*    (subseq (get-measures-pair *mf* 0 3) 0))
  (defparameter *notes*   (subseq (get-measures-pair *mf* 1) 0))
  (defparameter *quarter* (first (get-midi-tempo *mf*))))

(setf (bpm *tempo*) 70)

(let ((p 46))
  (fp 0 p)
  (fp 6 p))

(fp 1 40)
(fp 2 42)
(fp 2 74)

(defmethod p :around (time pitch velocity duration channel &key pan)
  (call-next-method))

(defmethod p :around (time pitch velocity duration channel &key pan)
  ;;  (call-next-method)
  (when (numberp pitch)
    (if (and (= channel 0) (< 1 pitch 50))
        (progn
          (call-next-method time pitch velocity duration channel)
          (call-next-method time (+ 12 pitch) (+ -10 velocity) (* .3333 4) 2))
        (call-next-method)))
  )

(let ((bass   (make-cycle *bass*))
      (notes  (make-cycle *notes*)))
  (defun f (time)
    ;; background + phasing
    (destructuring-bind (n d) (next notes)
      (pa time n 55 .3333 0 .3333)
      (pa (+ #[(cosr 1 .5 1) b] time)
          (butlast (transpose (reverse n) +12))
          55 .1111 0 .3333)
      )
    ;; notes
    (destructuring-bind (n d) (next bass)
      (pa time (rn n) 60 d 1 d))
    (aat (+ time #[4 b]) #'f it)))

(aat (tempo-sync #[*quarter* b]) #'f it)
(defun f ())
