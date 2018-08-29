(in-package :shiny)

;;;
;; ghosts.lisp
;; from notes in metalevel
;; kind of translated to a temporal recursion
;;;;

(defvar *metro* nil)
(setf *metro* (make-metro 60))

;; gestures
;; hitnote - note time
;;         (+ note 24)
;;         .5 amp
;;         dur is exponentially growing

(defun hitnote (time pitch vel dur c)
  (play-midi-note time (+ pitch 24) vel (mod dur 16) c))

;; thump - note time
;;   1) -18 note
;;      .4 amp
;;      .05 dur
;;   2) -23 note
;;      .4 amp
;;      .05
(defun thump (time pitch vel dur c cc)
  (play-midi-note time (- pitch 18) vel dur c)
  (play-midi-note time (- pitch 23) vel dur cc))

;; riff - note rhy
(defun riff (note)
  (do ((i (+ 39 (mod note 13)) (+ i 13))
       (k '() (push i k))
       (j 0 (1+ j)))
      ((> j 4) (reverse k))))

(defun ghosts (beat time root i)
  (let* ((c-beat (random-elt #(1 1/2 3/2)))
         (n-beat (+ beat c-beat)))
     (play-midi-note time root 30 .5 1)
     (when (> root 65)
       ;; ghost/thumb
       (play-midi-note time
                       (+ 24 root)
                       35
                       (+ 5 (mod (+ i 1) 10))
                       10)
     ;  (at (+ time #[.1 b]) #'play-midi-arpeggio time (riff root) 20 .1 13))
     ;; (when (= c-beat 3/2)
     ;;   (thump time root 30 .5 15 16))
     (aat (funcall *metro* n-beat) #'ghosts n-beat it
          (cm:ran :from 53 :below 77) (1+ i))))

(ghosts (funcall *metro* 'get-beat 4)
        (funcall *metro* (funcall *metro* 'get-beat 4))
        (cm:ran :from 53 :below 77) 0)

(fluidsynth:program-change *synth* 1 1)
(fluidsynth:program-change *synth* 10 43)

(flush-pending)
(off-with-the-notes *synth*)
