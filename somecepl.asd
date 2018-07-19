(asdf:defsystem "somecepl"
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
               (:file "musicutils")
;;               (:file "nudruz")
               (:file "extempore")
               (:file "overtone")))
