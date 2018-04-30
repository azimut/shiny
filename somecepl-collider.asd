(asdf:defsystem "somecepl-collider"
  :description "Describe somecepl here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (
               ;; ???
               #:swank
               ;; collider
               #:sc
               ;; midi-constrol
               #:local-time
               #:cl-alsaseq
               ;; fluid
               #:fluidsynth
               ;; misc
               #:arrow-macros
               #:cm
               ;; needed on extempore.lisp
               #:cl-ppcre
               )
  :serial t
  :components ((:file "collider/package")
               (:file "musicutils")
               (:file "nudruz")
               (:file "extempore")
               (:file "overtone")))
