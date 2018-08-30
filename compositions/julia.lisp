(in-package :shiny)

(let* ((chord  (make-chord-fixed 60 3 (ov-pc-scale :melodic-minor)))
       (cc     (make-cycle (list (make-cycle (first chord) 8)
                                 (make-cycle (rest  chord)  8))))
       (rhythm (make-cycle (list (make-cycle 1 8)
                                 (make-cycle 1/4 8))))
       (chan  (make-cycle '(0 127))))
  (defun f (time)
    (let ((r (next rhythm)))
      (pa time (print (nth-inc 0 -12 (next cc))) (* .5 r) 50 0 (* .5 r)
          :pan (if (= 1 r) (next chan) 62))
      (aat (+ time r) #'f it))))

(let* ((chord (make-chord-fixed 60 3 (ov-pc-scale :melodic-minor)))
       (cc    (make-cycle
               (list (make-cycle (list (list (first chord) 1)) 8)
                     (make-cycle (list (list (rest  chord) 1/4)) 4))))
       (chan  (make-cycle '(0 127))))
  (defun f (time)
    (destructuring-bind (n r) (next cc)
      (pa time (nth-inc 0 -12 n) (* .5 r) 50 0 (* .5 r)
          :pan (if (= 1 r) (next chan) 62))
      (aat (+ time r) #'f it))))

(f (quant 4))
(defun f ())

(let* ((chord (make-chord-fixed 60 3 (ov-pc-scale :melodic-minor)))
       (notes (make-cycle
               (append (repeat 8 (list (list (first chord) 1)))
                       (repeat 8 (list (list (rest  chord) 1/2)))))))
  (defun f (time)
    (destructuring-bind (n r) (print (next notes))
      (if (listp n)
          (pa time n (* .5 r) 50 0 (* .5 r))
          (p time n 50 r 0))
      (aat (+ time r) #'f it))))

(f (quant 4))
(fp 0 72)
(fp 1 30)
(fg 1f0)

(let* ((chord (make-chord-fixed 48 3 (ov-pc-scale :melodic-minor)))
       (cc    (make-cycle (first chord)))
       (mel   (make-heap chord)))
  (defun f (time)
    (p time (next cc) 50 1 0)
    (let ((m (next mel)))
      (pa time (pick m m m (list m (pc-relative m +2 (ov-pc-scale :melodic-minor)))) 1 50 1 1))
    (aat (+ time 1) #'f it)))

(f (now))