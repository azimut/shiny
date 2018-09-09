(in-package :shiny)

(defparameter *mf* "/home/sendai/Downloads/Aquatic_Ambience.mid")

(defparameter *notes-lead* (get-notes *mf* 0))
(defparameter *notes-bass* (get-notes-durations-chords-silences *mf* 1))

(let ((notes (make-cycle (subseq *notes-lead* 0 63)))
      (bass (make-cycle (subseq *notes-bass* 0 9))))
  (defun f (time &optional (beat 0))
    (let* ((r .3)
           (step (* r 16)))
      (play-taupe (next notes) r)
      (when (= (round (mod beat step)) 0)
        (play-taupe (+ 12 (first (next bass))) step))
      (aat (+ time #[r b]) #'f it (+ beat r)))))

(defun f ())
(f (now))
