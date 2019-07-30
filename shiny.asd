(asdf:defsystem "shiny"
  :author "azimut <azimut.github@protonmail.com>"
  :description "incudine"
  :license "GPL-3.0"
  :version "0.1"
  :serial t
  :depends-on (#:incudine
               #:cl-ppcre
               #:cl-arrows
               #:cm
               #:serapeum
               #:str
               #:verbose)
  :components ((:file "package")
               (:file "lib/musicutils")
               (:file "lib/extempore")
               (:file "lib/overtone")
               (:file "lib/drums")
               (:file "lib/cm")
               (:file "lib/incudine")
               (:file "lib/buffers")
               (:file "lib/midifile")
               (:file "lib/cryptogram")))

(asdf:defsystem "shiny/fluidsynth"
  :author "azimut <azimut.github@protonmail.com>"
  :description "incudine"
  :license "GPL-3.0"
  :version "0.1"
  :serial t
  :depends-on (#:shiny
               #:incudine-fluidsynth)
  :components ((:file "lib/fluidsynth")))

(asdf:defsystem "shiny/lv2"
  :author "azimut <azimut.github@protonmail.com>"
  :description "incudine"
  :license "GPL-3.0"
  :version "0.1"
  :serial t
  :depends-on (#:shiny #:incudine-lv2)
  :components ((:file "lib/lv2")))

(asdf:defsystem "shiny/alsaseq"
  :author "azimut <azimut.github@protonmail.com>"
  :description "incudine"
  :license "GPL-3.0"
  :version "0.1"
  :serial t
  :depends-on (#:shiny #:cl-alsaseq)
  :components ((:file "lib/alsaseq")))

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

(asdf:defsystem "shiny/aubio"
  :author "azimut <azimut.github@protonmail.com>"
  :description "incudine"
  :license "GPL-3.0"
  :version "0.1"
  :serial t
  :depends-on (#:shiny #:aubio/double)
  :components ((:file "lib/aubio")))

(asdf:defsystem "shiny/espeak-ng"
  :author "azimut <azimut.github@protonmail.com>"
  :description "incudine"
  :license "GPL-3.0"
  :version "0.1"
  :serial t
  :depends-on (#:shiny #:espeak-ng)
  :components ((:file "lib/espeak-ng")))

(asdf:defsystem "shiny/morse"
  :author "azimut <azimut.github@protonmail.com>"
  :description "incudine"
  :license "GPL-3.0"
  :version "0.1"
  :serial t
  :depends-on (#:shiny #:cl-morse)
  :components ((:file "lib/morse")))

(asdf:defsystem "shiny/foxdot"
  :author "azimut <azimut.github@protonmail.com>"
  :description "incudine"
  :license "GPL-3.0"
  :version "0.1"
  :serial t
  :depends-on (#:shiny #:cl-lex #:yacc)
  :components ((:file "lib/foxdot/foxdot")
               (:file "lib/foxdot/foxdot-grammar")
               (:file "lib/foxdot/foxdot-play")))

(asdf:defsystem "shiny/csound-live-code"
  :author "azimut <azimut.github@protonmail.com>"
  :description "incudine"
  :license "GPL-3.0"
  :version "0.1"
  :serial t
  :depends-on (#:shiny
               #:cl-lex
               #:yacc
               #:numcl
               #:bit-smasher)
  :components ((:file "lib/csound-live-code")))

(asdf:defsystem "shiny/csound-udp"
  :author "azimut <azimut.github@protonmail.com>"
  :description "incudine"
  :license "GPL-3.0"
  :version "0.1"
  :serial t
  :depends-on (#:shiny
               #:usocket)
  :components ((:file "lib/csound-udp")))
