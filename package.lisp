;;;; package.lisp

;; vari to use dot

(uiop:define-package somecepl
  (:shadowing-import-from #:cepl #:free)
  (:shadowing-import-from #:incudine #:buffer-data)
  (:use #:cl #:cepl #:rtg-math
          #:nineveh
          #:vari
          #:cl-ppcre
          #:incudine
          #:cepl.skitter
          #:livesupport)
  (:import-from #:incudine.vug
                #:define-vug
                #:delay1
                #:cout
                #:buffer-play
                #:foreach-channel
                #:foreach-frame
                #:pan2
                #:with
                #:delay-s
                #:envgen
                #:vdelay
                #:stereo
;;                #:mouse-button
                #:white-noise
                #:samphold
                #:pole
                #:make-local-adsr
                #:make-local-perc
                #:phasor
                #:bpf
                #:buffer-read
                #:butter-lp
                #:fractal-noise
                #:out
                #:~
                #:sine
                #:dsp!)
  (:import-from #:incudine.util
                #:with-samples
                #:db->lin
                #:sample->fixnum
                #:non-negative-sample
                #:lin->db
                #:*SAMPLE-DURATION*
                #:*SAMPLE-RATE*
                #:*twopi-div-sr*
                #:+sample-zero+)
)
