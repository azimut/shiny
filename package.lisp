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
                #:cout
                #:buffer-play
                #:foreach-channel
                #:pan2
                #:stereo
                #:white-noise
                #:samphold
                #:make-local-adsr
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
                #:non-negative-sample
                #:lin->db
                #:*twopi-div-sr*
                #:+sample-zero+)
)
