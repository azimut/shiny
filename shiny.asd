(asdf:defsystem "shiny"
  :description "Describe somecepl here"
  :author "Your Name <your.name@example.com>"
  :license "GPL-3.0"
  :depends-on (
               #:swank
               #:cm
               #:incudine
               #:arrow-macros
               #:cl-ppcre
               )
  :serial t
  :components ((:file "package")
               (:file "lib/musicutils")
;;               (:file "nudruz")
               (:file "lib/extempore")
               (:file "lib/cm")
               (:file "lib/overtone")
               (:file "lib/drums")))