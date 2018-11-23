(asdf:defsystem "shiny-cepl-standalone"
  :description "Describe somecepl here"
  :author "Your Name <your.name@example.com>"
  :license "GPL-3.0"
  :serial t
  :depends-on (;; ???
               #:swank
               ;; misc
               #:arrow-macros
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
               #:rtg-math.vari
               #:dendrite
               #:nineveh
               #:with-setf)
  :components ((:file "cepl/package-standalone")
               (:file "lib/assets")
               (:file "lib/misc-gpu")))
