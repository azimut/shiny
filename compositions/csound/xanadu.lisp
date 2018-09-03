(in-package :shiny)

(start-csound (gethash :xanadu *orcs*))

(bt:make-thread
 (lambda ()
   (loop
      :for frame = (csound:csoundperformksmps *c*)
      :while (= frame 0))))

;; XANADU
(make-play plucke "i1" :p4 0 :keynum 60)
(make-play pluck  "i2" :p4 0 :keynum 60)
(make-play newfm  "i3" :p4 0 :keynum 60 :llimit .2 :rlimit 2.0)

(play-newfm 90 5 :p4 .9)
(play-pluck 90 2)

;; Csound tutorial
(defun lorenz ()
  (labels ((interpolate (x)
             (round (+ 36 (* x 60)))))
    (let ((r 3.974))
      (mapcar #'interpolate
              (loop :repeat 100 :with y = .5 :collect
                 (setf y (* r y (- 1f0 y))))))))

(let ((notes (make-cycle (lorenz))))
  (defun f (time)
    (play-pluck (next notes) .5)
    (aat (+ time #[.5 b]) #'f it)))

(defun f ())
(f (now))
