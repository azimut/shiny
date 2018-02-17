;;;; somecepl.asd

;; cepl.skitter.sdl - is for mouse support
;; livesupport      - provides a generic main loop with (play)

(asdf:defsystem #:somecepl
  :description "Describe somecepl here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (
               #:cepl.sdl2
               #:swank
               #:cm
               #:dendrite
               #:incudine
               #:nineveh
               #:arrow-macros
               #:livesupport
               #:cl-ppcre
               #:skitter
               #:classimp
               ;;#:cm-incudine
               #:cepl.skitter.sdl2
               #:dirt)
  :serial t
  :components (
               (:file "package")
               (:file "assets")
               ;(:file "camera")
               (:file "musicutils")))
;               (:file "examples/helpers/examples-data")
;               (:file "examples/helpers/camera")
;               (:file "examples/helpers/model-parsers")
;               (:file "examples/helpers/meshes")))
