(uiop:define-package somecepl
  (:shadowing-import-from #:cm #:between) ;; between is in tmp-func but undef
  (:shadowing-import-from #:incudine #:buffer-data #:buffer #:sample)
  (:use #:cl 
        #:arrow-macros
        #:cl-ppcre
        #:incudine)
  (:import-from #:cm
                #:eop?
                #:rhythm
                ;; modify a list, like a chord
                #:invert
                #:transpose
                #:shuffle
                ;; lazy evaluation, can be next'd
                #:pval
                ;; random boolean
                #:odds
                ;; random "single element" picker, still you can use lists of lists
                #:pick
                #:pickl
                ;; random distribution helper
                #:ran
                #:drunk ;; brownian noise
                #:between
                ;; patterns
                #:next
                #:new
                #:weighting
                #:markov ;; higher order explicit transition, than nesting patterns
                #:cycle
                #:palindrome
                #:heap)
  (:import-from #:incudine.vug
                #:define-vug
                #:define-ugen
                #:make-frame
                #:frame-ref
                #:delay1
                #:cout
                #:vuglet
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
                #:with-control-period
                #:white-noise
                #:samphold
                #:pole
                #:make-local-adsr
                #:make-local-perc
                #:phasor
                #:make-f32-array
                #:phasor-loop
                #:bpf
                #:lpf
                #:hpf
                #:buffer-read
                #:butter-lp
                #:fractal-noise
                #:out
                #:~
                #:rand
                #:sine
                #:pulse
                #:line
                #:pink-noise
                #:dsp!)
  (:import-from #:incudine.util
                #:with-samples
                #:f32-ref
                #:db->lin
                #:+twopi+
                #:rt-eval
              ;  #:barrier
                #:return-value-p
                #:sample->fixnum
                #:non-negative-sample
                #:lin->db
                #:SAMPLE
                #:*SAMPLE-DURATION*
                #:*SAMPLE-RATE*
                #:*twopi-div-sr*
                #:+sample-zero+))
