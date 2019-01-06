(uiop:define-package shiny
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
  (:import-from #:alexandria
                #:flatten
                #:when-let
                #:when-let*
                #:sequence-of-length-p
                #:last-elt
                #:first-elt                
                #:iota
                #:extremum
                #:ensure-list
                #:appendf
                #:lastcar)
  (:import-from #:with-setf
                #:with-setf
                #:with-setf*)
  (:import-from #:incudine.external
                #:foreign-copy-samples)
  (:import-from #:cm
                #:eop?
                #:rhythm
                ;; modify a list, like a chord
                #:invert
                ;; CEPL?!??
;;                #:transpose
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
                #:maybe-expand
                #:define-vug
                #:define-ugen
                #:make-frame
                #:frame-ref
                #:delay1
                #:cout
                #:counter
                #:vuglet
                #:current-channel
                #:current-frame
                #:buffer-play
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
                ;;#:mouse-button
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
                #:buffer-write
                #:butter-lp
                #:fractal-noise
                #:out
                #:~
                ;; nieveh!!!!
                ;;#:rand
                #:sine
                #:pulse
                #:line
                #:pink-noise
                #:dsp!)
  (:import-from #:incudine.util
                #:dochannels
                #:with-samples
                #:f32-ref
                #:db->lin
                #:+twopi+
                #:rt-eval
                ;;#:barrier
                #:return-value-p
                #:sample->fixnum
                #:non-negative-sample
                #:lin->db
                #:SAMPLE
                #:*SAMPLE-DURATION*
                #:*SAMPLE-RATE*
                #:*twopi-div-sr*
                #:+sample-zero+))

(in-package #:shiny)

(defun quant (beats)
  "returns the time (+ (now) beats), sc-collider like thing"
  (+ (now) (* *sample-rate* (* (sample beats) (spb *tempo*)))))
