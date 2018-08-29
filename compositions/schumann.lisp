(in-package :somecepl)

(defparameter *scale*  (ov-scale :G4 :minor))
(defparameter *ncrypt* (decrypt-melody "carla"))
(defparameter *theme*  (nths *ncrypt* *scale*))

(defun div (num div)
  "/ with zero divisor support-ish"
  (when (> div 0)
    (/ num div)))

(fg 2f0)
(p (now) 50 80 1 0)

(defun f ())
(let* ((bass  (make-cycle (list (make-cycle *theme* 1)
                                (make-cycle 0 3))))
       (c-a-r (nths '(0 1 2) *theme*))
       (l-a   (nths '(3 4)   *theme*))
       (mel   (make-cycle (list (make-cycle c-a-r)
                                (make-cycle l-a) 0)))
       (rhy   (make-weighting '((1 .9) .5))))
  (defun f (time)
    (let ((r (next rhy)))
      (p time (+ -12 (next bass)) 50 r 0)
      (let* ((n (pick 1 2 3 1 2))
             (f (div r n))
             (m (next mel n))
             (m (nth-inc (min (pick 0 1 2) (1- n))
                         (pick 0 -12 0 0) m))
             )
        (and (odds .1) (p time (+ 12 (pickl l-a)) 90 5 3))
        ;;(pa time m f 50 '(2 4 6) f)
        (pa (+ #[.5 b] time) (reverse m) f 50 '(6 4 2) f)
        )
      (aat (+ time #[r b]) #'f it))))

(f (now))

(fp 2 82)
(fp 3 59)
(fp 4 82)
(fp 6 82)

(fg .9)
;;--------------------------------------------------

(setf (bpm *tempo*) 100)
(defun f ())
(let ((notes
       (make-cycle
        (nths (decrypt-melody "car")
              (ov-scale :C3 :ryukyu)))))
  (defun f (time)
    (pa time (nth-inc 2 12 (next notes 3)) 1/3 '(30 30 40) '(0 0 0)
        (list 1/3 1/3 (pick 1/3 1 .1)))
    (pa (+ time #[.2 b]) (nth-inc 2 12 (next notes 3)) 1/3 '(30 30 40) '(1 1 1)
        (list 1/3 1/3 (pick 1/3 1 .1)))
    (p time *gm-kick* 30 6 (pick 22 23 24) :pan (pick 0 200))
    (aat (+ time #[3 b]) #'f it)))

(fp 22 10 2)
(fp 23 10 2)
(fp 24 10 2)

(fp 0 0)
(fp 1 40)

(f (tempo-sync #[1 b]))
