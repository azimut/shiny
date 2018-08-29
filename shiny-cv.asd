(asdf:defsystem "somecepl-cv"
  :description "Describe somecepl here"
  :author "Your Name <your.name@example.com>"
  :license "GPL-3.0"
  :depends-on (
               #:swank
               #:cm
               #:incudine
               #:arrow-macros
               #:cl-ppcre
               #:common-cv
               )
  :serial t
  :components ((:file "package")
               (:file "musicutils")
               (:file "cv")
;;               (:file "nudruz")
               (:file "extempore")
               (:file "overtone")))
