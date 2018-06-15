(asdf:defsystem "somecepl"
  :description "Describe somecepl here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (
               #:swank
               #:cm
               #:incudine
               #:arrow-macros
               #:cl-ppcre
               )
  :serial t
  :components ((:file "package")
               (:file "musicutils")
;;               (:file "nudruz")
               (:file "extempore")
               (:file "overtone")))
