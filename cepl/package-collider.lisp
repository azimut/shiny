(uiop:define-package shiny
  (:shadowing-import-from #:cm #:between)
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
  (:import-from #:pixel-spirit-deck
                #:defcard
                #:stroke
                #:g-fill
                #:circle-sdf
                #:cross-sdf
                #:flower-sdf
                #:heart-sdf
                #:hex-sdf
                #:poly-sdf
                #:rays-sdf
                #:rect-sdf
                #:rhomb-sdf
                #:spiral-sdf
                #:star-sdf
                #:tri-sdf
                #:vesica-sdf)
  (:import-from #:sc #:*s* #:callback #:at #:quant)
  (:import-from #:the-book-of-shaders
                #:g-random
                #:g-rand)
  (:import-from #:cm
                #:eop?
                #:rhythm
                ;; modify a list, like a chord
                #:invert
;;                #:transpose ;; glsl-symbols
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
                #:heap))

;; define var, provided originally by incudine
(in-package :shiny)
(defvar *sample-rate* 1d0)
