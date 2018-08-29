(uiop:define-package somecepl
  (:shadowing-import-from #:cm #:between) ;; between is in tmp-func but undef
  (:use #:cl 
        #:arrow-macros
        #:cl-ppcre
        #:sc)
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
                #:heap))

;; define var, provided originally by incudine
(in-package :shiny)
(defvar *sample-rate* 1d0)
