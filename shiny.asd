(asdf:defsystem "shiny"
  :author "azimut <azimut.github@protonmail.com>"
  :description "incudine"  
  :license "GPL-3.0"
  :version "0.1"
  :serial t
  :depends-on (#:swank
               #:cm
               #:incudine
               #:cl-ppcre)
  :components ((:file "package")
               (:file "lib/musicutils")
               (:file "lib/extempore")
               (:file "lib/cm")
               (:file "lib/overtone")
               (:file "lib/drums")
               (:file "lib/incudine")
               (:file "lib/midifile")))

(asdf:defsystem "shiny/fluidsynth"
  :author "azimut <azimut.github@protonmail.com>"
  :description "incudine"  
  :license "GPL-3.0"
  :version "0.1"
  :serial t
  :depends-on (#:shiny #:incudine-fluidsynth)
  :components ((:file "lib/fluidsynth")))

(asdf:defsystem "shiny/csound"
  :author "azimut <azimut.github@protonmail.com>"
  :description "incudine"  
  :license "GPL-3.0"
  :version "0.1"
  :serial t
  :depends-on (#:shiny #:csound)
  :components ((:file "lib/csound")))

(asdf:defsystem "shiny/gme"
  :author "azimut <azimut.github@protonmail.com>"
  :description "incudine"  
  :license "GPL-3.0"
  :version "0.1"
  :serial t
  :depends-on (#:shiny #:cl-gme/incudine)
  :components ((:file "lib/gme")))
