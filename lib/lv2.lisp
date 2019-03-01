(in-package #:shiny)

(dsp! swipe-dsp-up ((plugin (or null vug-foreign:plugin-instance))
                    (port-index fixnum)
                    dur
                    max
                    min)
  (:defaults NIL 45 1 1 0)
  (with-samples ((e (envelope
                     (make-envelope `(,max ,min ,max) '(.4 .6))
                     1 dur #'stop)))
    (foreach-frame
      (setf (f32-ref (vug-foreign:plugin-port-pointer plugin port-index))
            (coerce e 'single-float)))))

(dsp! swipe-dsp-down ((plugin (or null vug-foreign:plugin-instance))
                      (port-index fixnum)
                      dur
                      max
                      min)
  (:defaults NIL 45 1
             1 0)
  (with-samples ((e (envelope
                     (make-envelope `(,max ,min ,max) '(.4 .6))
                     1 dur #'stop)))
    (foreach-frame
      (setf (f32-ref (vug-foreign:plugin-port-pointer plugin port-index))
            (coerce e 'single-float)))))

;;--------------------------------------------------
(dsp! swipe-dsp-sin ((plugin (or null vug-foreign:plugin-instance))
                     (port-index fixnum)
                     dur)
  (:defaults NIL 45 1)
  (with-samples ((e (envelope
                     (make-envelope '(0 1 0) '(.4 .6))
                     1 dur #'stop)))
    (foreach-frame
      (setf (f32-ref (vug-foreign:plugin-port-pointer plugin port-index))
            (coerce (+ .5 (* .5 (sine 440 e))) 'single-float)))))
