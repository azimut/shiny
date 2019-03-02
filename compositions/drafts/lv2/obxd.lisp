(in-package #:shiny)

;;http://www.onicos.com/staff/iz/formats/midi-event.html

(set-rt-block-size 64)
(rt-start)

(incudine.vug:lv2->vug "https://obxd.wordpress.com" obxd)

(dsp! lv2-synth-test ((plugin (or null vug-foreign:plugin-instance)))
  (:defaults nil)
  (with ((out (cffi:null-pointer)))
    (declare (type pointer out))
    ;; We will use VUG-FOREIGN:PLUGIN-PORT-POINTER out of the DSP.
    (setf out (obxd :plugin-instance plugin))
    (foreach-frame
      ;; Plugin with multiple audio output ports, for example two:
      (out (sample (f32-ref (incudine::ptr-ref out 0) current-frame))
           (sample (f32-ref (incudine::ptr-ref out 1) current-frame))))))

(lv2-synth-test :id 123)

(defparameter *sst* (control-value 123 'plugin))

(lv2:write-event *sst* (lv2:midi-message #xc0 3 0))

(defun note-test (keynum velocity duration)
  (rt-eval ()
    (lv2:write-event *sst* (lv2:midi-message #x90 keynum velocity))
    (at (+ (now) #[duration seconds]) (lambda (k) (lv2:write-event *sst* (lv2:midi-message #x80 k 0)))
        keynum)))

(progn
  (note-test 69 30 2)
  (note-test 74 30 2))
(lv2:write-event *sst* (lv2:midi-message #xc0 0 0))
(let* ((scale  (ov-scale :c2 :minor))
       (hscale (make-heap scale))
       (rhy (make-cycle '(1 1 .5 .5 .25 .25 .5 1))))
  (defun f (time)
    (let ((r (next rhy)))
      ;;(swipe-dsp-down *sst* 20 .15)
      (swipe-dsp-down *sst* 54 (- r .2) 1)
      ;;(swipe-dsp-down *sst* 55 .25)
      (lv2-note *sst* (next hscale) 30 (- r .2))
      (aat (+ time #[r b]) #'f it))))
(set-lv2-control *sst* 20 0)
(setf (bpm *tempo*) 60)
(defun f ())

(f (now))

(set-lv2-control *sst* 20 .3)

(defvar *midiin* (jackmidi:open))
(make-responder *midiin*
                (lambda (st d1 d2)
                  (lv2:write-event *sst* (lv2:midi-message st d1 d2))))

