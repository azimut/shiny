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
               (:file "lib/drums")))

(asdf:defsystem "shiny/fluidsynth"
  :author "azimut <azimut.github@protonmail.com>"
  :description "incudine"  
  :license "GPL-3.0"
  :version "0.1"
  :serial t
  :depends-on (#:shiny #:incudine-fluidsynth)
  :components ((:file "lib/fluidsynth")))

(asdf:defsystem "shiny/cv"
  :author "azimut <azimut.github@protonmail.com>"
  :description "incudine"  
  :license "GPL-3.0"
  :version "0.1"
  :serial t
  :depends-on (#:shiny #:common-cv)
  :components ((:file "lib/cv")))

(asdf:defsystem "shiny/csound"
  :author "azimut <azimut.github@protonmail.com>"
  :description "incudine"  
  :license "GPL-3.0"
  :version "0.1"
  :serial t
  :depends-on (#:shiny #:csound)
  :components ((:file "lib/csound")))
