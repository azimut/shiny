(asdf:defsystem "shiny-collider-cepl"
  :description "Describe somecepl here"
  :author "Your Name <your.name@example.com>"
  :license "GPL-3.0"
  :serial t
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
               ;; cepl stuff
               #:cepl.sdl2
               #:cepl.sdl2-image
               #:livesupport
               #:skitter
               #:classimp
               #:cepl.skitter.sdl2
               #:temporal-functions
               #:dirt
               #:split-sequence
               #:rtg-math
               #:rtg-math.vari
               #:dendrite
               #:nineveh
               #:with-setf
               ;; ??
               #:pixel-spirit-deck
               #:the-book-of-shaders
               )
  :components ((:file "cepl/package-collider")
               (:file "musicutils")
;;               (:file "nudruz")
               (:file "extempore")
               (:file "overtone")))
