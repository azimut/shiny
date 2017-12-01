;;;; package.lisp

;; vari to use dot

(uiop:define-package somecepl
    (:use #:cl #:cepl #:rtg-math
          #:nineveh
          #:vari
          #:incudine
          #:cepl.skitter
          #:livesupport)
  (:shadowing-import-from #:cepl #:free)
  (:shadowing-import-from #:incudine #:buffer-data)
  (:import-from #:incudine.vug #:define-vug #:out #:~ #:sine #:dsp!)
  (:import-from #:incudine.util
                #:with-samples
                #:db->lin
                #:non-negative-sample
                #:lin->db #:*twopi-div-sr* #:+sample-zero+)
)
