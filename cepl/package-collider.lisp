(uiop:define-package somecepl
  (:shadowing-import-from #:cm #:between) ;; between is in tmp-func but undef
  (:shadowing-import-from #:cepl #:free #:mix)
  (:use #:cl
        ;; misc
        #:arrow-macros
        #:cl-ppcre
        ;; collider
        ;; #:sc
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
  (:import-from #:sc #:*s* #:callback #:at #:quant)
  (:import-from #:cm
                #:rhythm
                ;; random boolean
                #:odds
                ;; random picker
                #:pick
                #:pickl
                ;; random walker
                #:drunk
                ;; random distribution helper
                #:ran
                #:between
                ;; patterns
                #:next
                #:new
                #:weighting
                #:markov ;; higher order explicit transition, than nesting patterns
                #:cycle
                #:palindrome
                #:heap))

;; define var, provided originally by incudine
(in-package :somecepl)
(defvar *sample-rate* 44100d0)
