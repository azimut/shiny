(asdf:defsystem "shiny-cepl"
  :author "someone"
  :description "+ cepl"
  :license "GPL-3.0"
  :version "0.1"
  :serial t
  :depends-on (#:swank
               #:cm
               #:incudine
               #:arrow-macros
               #:cl-ppcre
               #:cepl.sdl2
               #:livesupport
               #:skitter
               #:classimp
               #:cepl.skitter.sdl2
               #:temporal-functions
               #:dirt
               #:rtg-math.vari
               #:dendrite
               #:nineveh)
  :components ((:file "cepl/package")
               (:file "lib/assets")
               (:file "lib/musicutils")
;;               (:file "nudruz")
               (:file "lib/extempore")
               (:file "lib/overtone")
               (:file "lib/drums")
               (:file "lib/cm")
               (:file "lib/incudine")))
