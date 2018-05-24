(asdf:defsystem "somecepl-cepl-standalone"
  :description "Describe somecepl here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (
               ;; ???
               #:swank
               ;; misc
               #:arrow-macros
               ;; needed on extempore.lisp
               #:cl-ppcre
               ;; cepl stuff
               #:cepl.sdl2
               #:cepl.sdl2-image
               #:livesupport
               #:skitter
               #:classimp
               #:cepl.skitter.sdl2
               #:temporal-functions
               #:dirt
               #:split-sequence
               #:rtg-math
               #:rtg-math.vari
               #:dendrite
               #:nineveh
               #:with-setf
               )
  :components ((:file "cepl/package-standalone")
               ))
