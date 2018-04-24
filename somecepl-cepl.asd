(in-package :asdf-user)

(defsystem "somecepl-cepl"
  :author "someone"
  :description "+ cepl"
  :license "GNU"
  :version "0.1"
  :serial t
  :depends-on (
               #:swank
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
               #:nineveh
               )
  :components ((:file "cepl/package")
               (:file "musicutils")
               (:file "nudruz")
               (:file "extempore")
               (:file "overtone")))
