(in-package #:shiny)

(dsp! swipe-dsp-up ((plugin (or null vug-foreign:plugin-instance))
                    (port-index fixnum)
                    dur
                    (max single-float)
                    (min single-float))
  (:defaults NIL 45 1 1 0)
  (with-samples ((e (envelope
                     (make-envelope `(,max ,min ,max) '(.5 .5))
                     1 dur #'stop)))
    (foreach-frame
      (setf (f32-ref (vug-foreign:plugin-port-pointer plugin port-index))
            (coerce e 'single-float)))))

(dsp! swipe-dsp-down ((plugin (or null vug-foreign:plugin-instance))
                      (port-index fixnum)
                      dur
                      (max single-float)
                      (min single-float))
  (:defaults NIL 45 1 1 0)
  (with-samples ((e (envelope
                     (make-envelope `(,max ,min ,max) '(.5 .5))
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

;;--------------------------------------------------
(defun lv2-note (time plugin keynum velocity duration)
  (declare (type vug-foreign:plugin-instance plugin)
           (type double-float time)
           (type fixnum keynum velocity))
  (rt-eval ()
    (at time #'lv2:write-event plugin (lv2:midi-message #x90 keynum velocity))
    (at (+ time (calc-beats duration))
        (lambda (k) (lv2:write-event plugin (lv2:midi-message #x80 k 0)))
        keynum))
  keynum)
(defun lv2-arp (time plugin keynum velocity duration)
  (declare (type vug-foreign:plugin-instance plugin)
           (type double-float time)
           (type fixnum velocity)
           (type list keynum))
  (let* ((lnotes  (length keynum))
         (offsets (loop :for i :from 0 :by duration :collect i :repeat lnotes)))
    (mapcar (lambda (n o) (note-test (+ time (calc-beats o)) plugin n velocity duration))
            keynum
            offsets)))
(defun lv2-control (plugin port-index value)
  (declare (type vug-foreign:plugin-instance plugin)
           (type fixnum port-index))
  ;; Change the port value in rt-thread.
  (rt-eval ()
    (setf (f32-ref (vug-foreign:plugin-port-pointer plugin port-index))
          (coerce value 'single-float)))
  value)
