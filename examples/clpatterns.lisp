(in-package :cl-patterns)

(defparameter *clock* (make-clock (/ 110 60)))

(pdef :foo
      (pbind :instrument 1
             :channel (pseq '(0 1))
             :midinote 90
             :quant 4
             :dur 1
             :pfin 4
             :pan (pseq '(0 1))
             :amp .9))

(play :foo)
(stop :foo)
