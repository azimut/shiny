(in-package :shiny)

;; https://musescore.com/user/335926/scores/629201

(defparameter c (make-chord 60 80 4 (scale 0 'harmonic)))

(defparameter song (list (first c) (rest c) 0 (rest c)
                         (first c) (rest c) 0 (rest c)
                         (first c) (rest c) 0 (rest c)
                         (first c) (rest c) (first c) (rest c)))

(pa (quant 4) song .5 50 0 .5)


(defun moon ())

(defmacro repeat-at-beat (name beat-duration &body body)
  `(defun ,name (&optional (time (funcall #'quant 4)))
     ,@body
     (aat (+ time ,beat-duration) #',name it))) 

(pa (quant 4) song .5 50 0 '(.5 .5 0 .25
                             .5 .5 0 .25
                             .5 .5 0 .25
                             .5 .5 .5 .5))

(repeat-at-beat moon (* .4 (length song))
  (pa (quant 4) song .4 50 0 (mapcar (lambda (x) (+ 0 x))
                                     '(.4 .4 0 .2
                                       .4 .4 0 .2
                                       .4 .4 0 .2
                                       .4 .4 .4 .4))))

(moon)

(fp 0 52)
(fg 2f0)
(freverb-toggle 1)
(freverb-preset 6)
(defun moon ())
