(uiop:define-package somecepl
  (:shadowing-import-from #:cm #:between) ;; between is in tmp-func but undef
  (:use #:cl 
        #:arrow-macros
        #:cl-ppcre
        #:sc)
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
