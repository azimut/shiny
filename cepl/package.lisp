(uiop:define-package somecepl
  (:shadowing-import-from #:cm #:between) ;; between is in tmp-func but undef
  (:shadowing-import-from #:incudine #:buffer-data #:buffer #:sample)
  (:shadowing-import-from #:cepl #:free)
  (:use #:cl 
        #:cepl
        #:vari
        #:rtg-math
        #:nineveh
        #:arrow-macros
        #:temporal-functions
        #:cl-ppcre
        #:incudine
        #:cepl.skitter
        #:livesupport)
  (:import-from #:cm
                #:pick
                #:pickl
                #:drunk
                #:ran
                #:between
                #:odds)
  (:import-from #:incudine.vug
                #:define-vug
                #:define-ugen
                #:make-frame
                #:frame-ref
                #:delay1
                #:cout
                #:current-frame
              ;;  #:buffer-play
                #:foreach-channel
                #:foreach-frame
                #:pan2
                #:with
                #:delay-s
                #:envgen
                #:vdelay
                #:stereo
                #:midi-note-on-p
                #:midi-note-off-p
                #:midi-program-p
;;                #:mouse-button
                #:white-noise
                #:samphold
                #:pole
                #:make-local-adsr
                #:make-local-perc
                #:phasor
                #:make-f32-array
                #:phasor-loop
                #:bpf
              ;;  #:buffer-read
                #:butter-lp
                #:fractal-noise
                #:out
                #:~
                #:sine
                #:dsp!)
  (:import-from #:incudine.util
                #:with-samples
                #:f32-ref
                #:db->lin
                #:rt-eval
              ;  #:barrier
                #:return-value-p
                #:sample->fixnum
                #:non-negative-sample
                #:lin->db
                #:*SAMPLE-DURATION*
                #:*SAMPLE-RATE*
                #:*twopi-div-sr*
                #:+sample-zero+))