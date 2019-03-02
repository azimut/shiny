(in-package #:shiny)

;;(ql:quickload :incudine-lv2)

(set-rt-block-size 64)
(set-rt-block-size 1)
(rt-start)

;; Error: Div by zero
;;(incudine.vug:lv2->vug "http://www.openavproductions.com/sorcer" wolper)
(incudine.vug:lv2->vug "https://obxd.wordpress.com" obxd)

(incudine.vug:lv2->vug "http://invadarecords.com/plugins/lv2/testtone" wolper)
(incudine.vug:lv2->vug "http://tumbetoene.tuxfamily.org" wolper)

(incudine.vug:lv2->vug "https://github.com/asb2m10/dexed" dexed)


(dsp! dexed-test ((plugin (or null vug-foreign:plugin-instance)))
  (:defaults nil)
  (with ((out (cffi:null-pointer)))
    (declare (type pointer out))
    (setf out (dexed :plugin-instance plugin))
    (foreach-frame
      ;; Single audio output port for Dexed.
      (stereo (sample (f32-ref out current-frame))))))

(dsp! lv2-synth-test ((plugin (or null vug-foreign:plugin-instance)))
  (:defaults nil)
  (with ((out (cffi:null-pointer)))
    (declare (type pointer out))
    ;; We will use VUG-FOREIGN:PLUGIN-PORT-POINTER out of the DSP.
    (setf out (obxd :plugin-instance plugin))
    (foreach-frame
      ;; Plugin with a single audio output port.
      ;;(stereo (sample (f32-ref out current-frame)))
      ;; Plugin with multiple audio output ports, for example two:
      (out (sample (f32-ref (incudine::ptr-ref out 0) current-frame))
           (sample (f32-ref (incudine::ptr-ref out 1) current-frame)))
      )))

(lv2-synth-test :id 123)

(defparameter *sst* (control-value 123 'plugin))
(free (node 0))
;;(at time #'fluidsynth:noteon *synth* channel n velocity)
(lv2:write-event *sst* (lv2:midi-message 1 10 80))
(lv2:write-event *sst* (lv2:midi-message (logand 0 #xF) 0 0))

(defvar *midiin* (jackmidi:open))

(make-responder *midiin*
                (lambda (st d1 d2)
                  (lv2:write-event *sst* (lv2:midi-message st d1 d2))))




(dexed-test :id 123)

(defvar *dexed* (control-value 123 'plugin))

(lv2:write-event *dexed* (lv2:midi-message #xc0 6 0))

(defun note-test (keynum velocity duration)
  (rt-eval ()
    (lv2:write-event *dexed* (lv2:midi-message #x90 keynum velocity))
    (at (+ (now) #[duration seconds])
        (lambda (k) (lv2:write-event *dexed* (lv2:midi-message #x80 k 0)))
        keynum)))

(progn
  (note-test 69 100 2)
  (note-test 74 90 1.5))






;;--------------------------------------------------
(defvar *midiin* (jackmidi:open))

(make-responder *midiin*
                (lambda (st d1 d2)
                  ;; Channel one for Dexed.
                  (when (= (logand st #xf) 0)
                    (lv2:write-event *dexed* (lv2:midi-message st d1 d2)))))

(recv-start *midiin*)


(incudine::lv2->vug "https://obxd.wordpress.com" obxd)

(dsp! obxd-test ((plugin t))
  (:defaults nil)
  (with ((out (cffi:null-pointer)))
    (declare (type pointer out))
    (setf out (obxd :plugin-instance plugin))
    (foreach-frame
      ;; Two audio output ports for Obxd.
      (out (sample (f32-ref (incudine::ptr-ref out 0) current-frame))
           (sample (f32-ref (incudine::ptr-ref out 1) current-frame))))))

(obxd-test :id 321)

(defvar *obxd* (control-value 321 'plugin))

(make-responder *midiin*
                (lambda (st d1 d2)
                  ;; Channel two for Obxd.
                  (when (= (logand st #xf) 1)
                    (lv2:write-event *obxd* (lv2:midi-message st d1 d2)))))
