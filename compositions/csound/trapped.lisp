(in-package :shiny)

(start-csound (gethash :trapped *orcs*))

(bt:make-thread
 (lambda ()
   (loop (let ((frame (csound:csoundperformksmps *c*)))
           (when (not (= frame 0))
             (return))))))

;; TRAPPED
(make-play ivory  "i1" 0  200 .001 17.8 .99)
(make-play blue   "i2" .2 600 23 10 .52)
(make-play violet "i3" 0  800 .8 57)
(make-play black  "i4" .4 1000 4600 6500 33 0.6)
(make-play green  "i5" 0 3500 .2 .1 3 10 12 27)
(make-play sand   "i9" .2 1000 .2 30 40)
(make-play taupe  "i10" 0 3500 .2 .1 3 10 12 27)
(make-play rust   "i11" 0 2200 .2)
(make-play teal   "i12" 6.8 1000 100 7000 16 0.2)
(make-play foam   "i13" .4 1500 40 7)

(play-sand 60 1 .4 1000 2.0 10 40)

(defparameter *pattern* '((0 :-> (0 1))
                          (1 :-> (0 1 0))))
(defun f ())
(let ((p (parse-patternc (cm::rwgen *pattern* '(0) 4)))
      (n (make-cycle '(60 61 64))))
  (defun f (time)
    (when (next p) (play-sand 60 2 0 500))
    (aat (+ time #[.5 b]) #'f it)))

(f (now))
