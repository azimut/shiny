;;;; somecepl.asd

;; cepl.skitter.sdl - is for mouse support
;; livesupport      - provides a generic main loop with (play)

(asdf:defsystem #:somecepl
  :description "Describe somecepl here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:cepl
               #:cepl.sdl2
               #:swank
               #:incudine
               #:nineveh
               #:livesupport
               #:cepl.skitter.sdl2
               #:dirt)
  :serial t
  :components ((:file "package")
               (:file "assets")))

