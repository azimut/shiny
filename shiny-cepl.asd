(asdf:defsystem "shiny-cepl"
  :author "azimut <azimut.github@protonmail.com>"
  :description "incudine + cepl"
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
               (:file "lib/extempore")
               (:file "lib/overtone")
               (:file "lib/drums")
               (:file "lib/cm")
               (:file "lib/incudine")))


(asdf:defsystem "shiny-cepl/fluidsynth"
  :author "azimut <azimut.github@protonmail.com>"
  :description "incudine"  
  :license "GPL-3.0"
  :version "0.1"
  :serial t
  :depends-on (#:shiny-cepl #:incudine-fluidsynth)
  :components ((:file "lib/fluidsynth")))

(asdf:defsystem "shiny-cepl/csound"
  :author "azimut <azimut.github@protonmail.com>"
  :description "incudine"  
  :license "GPL-3.0"
  :version "0.1"
  :serial t
  :depends-on (#:shiny-cepl #:csound)
  :components ((:file "lib/csound")))

(asdf:defsystem "shiny-cepl/gme"
  :author "azimut <azimut.github@protonmail.com>"
  :description "incudine"  
  :license "GPL-3.0"
  :version "0.1"
  :serial t
  :depends-on (#:shiny-cepl #:cl-gme/incudine)
  :components ((:file "lib/gme")))

(asdf:defsystem "shiny-cepl/aubio"
  :author "azimut <azimut.github@protonmail.com>"
  :description "incudine"  
  :license "GPL-3.0"
  :version "0.1"
  :serial t
  :depends-on (#:shiny-cepl #:aubio/double)
  :components ((:file "lib/aubio")))
