(in-package :shiny)

(progn
  (defparameter *mf* "/home/sendai/Downloads/Aquatic_Ambience.mid")
  (defparameter *notes-lead* (get-notes *mf* 0))
  (defparameter *notes-bass* (get-notes-durations-chords-silences *mf* 1))
  (defparameter *notes-next* (subseq (get-measures-pair *mf* 1 18) 14))
  (defparameter *quarter* (first (get-midi-tempo *mf*))))
(freset)
(fp 3 87)
(fp 1 0)
(fp 2 2)
(defun g ())
(defun f ())
;; <3
(let ((notes  (make-cycle (subseq *notes-lead* 0 63)))
      (onotes (make-cycle  *notes-next*))
      (bass   (make-cycle (subseq *notes-bass* 0 9))))
  (defun g (time)
    (destructuring-bind (n d) (next onotes)
      (pa time (cm:transpose n +12) 40 d 3 (d2o d)))
    (aat (+ time #[ (* 4 *quarter*) s]) #'g it))

  (defun f (time &optional (beat 0))
    (let* ((r .3)
           (step (* r 16)))
      ;;(play-phaser (next notes) r)
      (p time (next notes) 40 r 2)
      ;;      (play-taupe (next notes) r)
      (when (= (round (mod beat step)) 0)
        ;;        (play-taupe (+ 12 (first (next bass))) step)
        ;; (let ((*clef* "f")
        ;;       (*window-name* "other")))
        ;;(play-phaser (+ 12 (first (next bass))) step)
        (p time (+ 12 (first (next bass))) 60 step 1)
        )
      (aat (+ time #[r b]) #'f it (+ beat r)))))

(aat (tempo-sync #[*quarter* s]) #'f it)
(defun f ())
(aat (tempo-sync #[*quarter* s]) #'g it)
(defun g ())

(fp 2 60)
(let* ((scale  (reverse (ov-scale :c5 :aeolian)))
       (cscale (make-cycle scale))
       (rhythm (make-cycle '(1 1 .5 .5 1.25 .5 1))))
  (defun g (time)
    (let ((r (next rhythm)))
      (and (fluidsynth:all-notes-off *synth* 0)
           (p time (car (next cscale (pick 1 1 1 1 3))) 40 r 2))
      (aat (+ time #[r b]) #'g it))))

(aat (tempo-sync #[1 b]) #'g it)
(defun g ())

;;--------------------------------------------------

(let ((notes (make-cycle (subseq *notes-lead* 0 20))))
  (defun f (time)
    (let ((n (next notes)))
      (when (odds .1)
        (inscore-video "/home/sendai/bunny.mp4" 2
                       :alpha 100
                       :window-name "w2"
                       :video-name (pick "v2" "v4" "v1")
                       :x (between -1f0 1f0)
                       :y (between 0 .5)
                       :rotatez (between -40f0 40f0))
        (let ((*window-name* "w2"))
          (p time (+ 12 n) 60 2 0)))
      (p time (+ -12 n) 60 .5 0))
    (aat (+ time #[.5 b]) #'f it)))

(inscore-reset)
(inscore-video "/home/sendai/bunny.mp4" 10 :volume .8)

(inscore-reset)
(inscore-stream)
(inscore-init)

(fp 0 20)
(defun f ())
(f (now))

;;--------------------------------------------------

(defun video-show (name filepath time dur)
  (make-cvideo name filepath)
  (at (+ time #[dur b]) #'queue-delete name))

(let ((notes (make-cycle (subseq *notes-lead* 0 20))))
  (defun f (time)
    (let ((n (next notes)))
      (when (odds .1)
        (play-)
        (video-show :bunny "/home/sendai/bunny.mp4" time 4))
      (play-timpani n .5))
    (aat (+ time #[.5 b]) #'f it)))

(f (now))
