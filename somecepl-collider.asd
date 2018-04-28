(asdf:defsystem "somecepl-collider"
  :description "Describe somecepl here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (
               #:swank
               #:cm
               #:sc
               #:arrow-macros
               #:cl-ppcre
               )
  :serial t
  :components ((:file "collider/package")
               (:file "musicutils")
               (:file "nudruz")
               (:file "extempore")
               (:file "overtone")))
