(in-package :shiny)

;; THIS SYNTX SUCKS!!!
(pa (quant 4) (repeat 4 '(60))
    (mapcar #'rhythm `(1/4 2/4 ,(+ 2/4 1/8) 3/4))
    60
    11
    (mapcar #'rhythm `(1/4 2/4 ,(+ 2/4 1/8) 3/4)))

(defmacro drumthis (time note beats velocity channel)
  (alexandria:with-gensyms (lbeats nbeats)
    `(let ((,lbeats (length ,beats))
           (,nbeats (mapcar #'rhythm ,beats)))
       (pa ,time (repeat ,lbeats (list ,note))
           ,nbeats ,velocity ,channel ,nbeats))))

;; ?
(drumthis (quant 4) 60 '(1/4 2/4 5/8 3/4) 60 3)

(p (quant 4) 60 60 1 0)
