(asdf:defsystem #:somecepl
  :description "Describe somecepl here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:cepl.sdl2
               #:swank
               #:cm
               #:dendrite
               #:incudine
               #:rtg-math.vari
               #:nineveh
               #:temporal-functions
               #:arrow-macros
               #:livesupport
               #:cl-ppcre
               #:skitter
               #:classimp
               #:cepl.skitter.sdl2
               #:dirt)
  :serial t
  :components ((:file "package")
               (:file "assets")
               (:file "nudruz")
               (:file "musicutils")))
