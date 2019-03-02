(in-package #:shiny)

(set-rt-block-size 64)
(rt-start)

(incudine::lv2->vug "https://github.com/asb2m10/dexed" dexed)

(dsp! dexed-test ((plugin (or null vug-foreign:plugin-instance)))
  (:defaults nil)
  (with ((out (cffi:null-pointer)))
    (declare (type pointer out))
    (setf out (dexed :plugin-instance plugin))
    (foreach-frame
      ;; Single audio output port for Dexed.
      (stereo (sample (f32-ref out current-frame))))))

(dexed-test :id 123)

(defparameter *dexed* (control-value 123 'plugin))

;; Program change.
(lv2:write-event *dexed* (lv2:midi-message #xc0 3 0))



(progn
  (note-test (now) 69 30 2)
  (note-test (now) 74 30 1.5))

(lv2:write-event *dexed* (lv2:midi-message #xc0 33 0))
(defun f (time)
  ;;(swipe-dsp-down *dexed* 57 1)
  (swipe-dsp-sin *dexed* 42 .5)
  (swipe-dsp-up  *dexed* 44 .5)
  (swipe-dsp-up  *dexed* 67 1)
  ;;(swipe-dsp-up *dexed* 30 1)
  (note-test (pickl (ov-scale :c4 :minor)) 30 .5)
  (aat (+ time #[1 b]) #'f it))

(aat (tempo-sync #[4 b]) #'f it)

(defun f ())
