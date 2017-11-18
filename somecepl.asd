;;;; somecepl.asd

(asdf:defsystem #:somecepl
  :description "Describe somecepl here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:cepl
               #:cepl.sdl2
               #:swank
               #:nineveh
               #:livesupport
               #:cepl.skitter.sdl2
               #:dirt)
  :serial t
  :components ((:file "package")
               (:file "somecepl")))

