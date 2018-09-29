(in-package :shiny)

(defparameter *mf* "/home/sendai/Downloads/Aquatic_Ambience.mid")

(defparameter *notes-lead* (get-notes *mf* 0))
(defparameter *notes-bass* (get-notes-durations-chords-silences *mf* 1))

(let ((notes (make-cycle (subseq *notes-lead* 0 63)))
      (bass (make-cycle (subseq *notes-bass* 0 9))))
  (defun f (time &optional (beat 0))
    (let* ((r .3)
           (step (* r 16)))
      (play-phaser (next notes) r)
;;      (play-taupe (next notes) r)
      (when (= (round (mod beat step)) 0)
;;        (play-taupe (+ 12 (first (next bass))) step)
        (let ((*clef* "f")
              (*window-name* "other"))
          (play-phaser (+ 12 (first (next bass))) step)))
      (aat (+ time #[r b]) #'f it (+ beat r)))))

(play-phaser-effect 180)
(defun f ())
(f (now))


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
