(asdf:defsystem "somecepl-collider"
  :description "Describe somecepl here"
  :author "Your Name <your.name@example.com>"
  :license "GPL-3.0"
  :serial t
  :depends-on (
               ;; ???
               #:swank
               ;; nudruz
               #:screamer
               ;; collider
               #:cl-collider
               ;; midi-constrol
               ;;#:local-time
               #:cl-alsaseq
               ;; fluid
               #:fluidsynth
               ;; misc
               #:arrow-macros
               #:cm
               ;; needed on extempore.lisp
               #:cl-ppcre
               )
  :components ((:file "collider/package")
               (:file "musicutils")
               (:file "nudruz")
               (:file "extempore")
               (:file "overtone")
               (:file "drums")))
