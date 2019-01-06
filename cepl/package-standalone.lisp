(uiop:define-package shiny
  (:use #:cl
        ;; misc
        #:arrow-macros
        ;; cepl
        #:cepl
        #:vari
        #:rtg-math
        #:nineveh
        #:with-setf
        #:temporal-functions
        #:cepl.sdl2-image
        #:cepl.skitter
        #:livesupport)
  (:import-from #:alexandria
                #:flatten
                #:when-let
                #:when-let*
                #:iota
                #:appendf
                #:sequence-of-length-p
                #:last-elt
                #:first-elt
                #:lastcar)
  (:import-from #:with-setf
                #:with-setf
                #:with-setf*))
