(in-package :shiny)

(fg 2f0)

(defparameter *mf*
  "/home/sendai/Downloads/Chrono_Trigger_-_11_Secret_of_the_Forest.mid")
(defparameter *notes* (get-notes *mf*))
(defparameter *bass*  (subseq (get-notes-durations-chords-silences *mf* 1) 0 20))
(defvar *mm* (cm:markov-analyze *notes* :order 3 :print? nil))
(freverb-toggle 1)
(freverb :roomsize .25d0
         :level 1d0
         :width 3d0)

(fp 3 52)
(fp 2 22)
(freset)
;;<3
(let ((note  (make-cycle (subseq *notes* 0 90)))
      (bass  (make-cycle *bass*))
      (s     (make-metro 60)))
  (defun f (time &optional (beat 0))
    (let ((n (next note)))
      (p time (+ 24 (first (next bass))) 50 .2 1 :pan (pick 0 128))
      (progn
        (when (funcall s 'at-beat 4)
          (p time (+ -12 n) 40 1 3))
        (p time (+ -12 n) 40 .3 0))
      ;;(p time (next *mm*) 30 .1 2)
      ;;(play-taupe (+ -12 n) .3 :amp 200 :ramp (rcosr 10 5 1) :rfreq 1000)
      )
    (aat (+ time #[.2 b]) #'f it (+ .2 beat))))

(fp 0 0)
(fp 2 68)
(fp 1 74)
(fp 1 90)

(aat (tempo-sync #[.2 b]) #'f it)
(defun f ())

;;--------------------------------------------------

(defparameter *mf* "/home/sendai/Downloads/Loro.mid")
(defparameter *notes* (get-notes *mf*))
(defparameter *notes-lead* (get-notes-durations-chords-silences *mf* 1))
(defun f ())
(let ((note (make-palindrome (subseq *notes* 0 30)))
      (lead (make-cycle      (subseq *notes-lead* 20 30))))
  (defun f (time &optional (beat 0))
    (let ((n (next note)))
      (p time n 50 .3 0)
      ;;(play-taupe (+ 12 n) .3 :amp (rcosr 140 20 5) :ramp 1)
      (destructuring-bind (n d) (next lead)
        (when (> n 0)
          (p time n 50 (* .4 d) 1))
        ;;(play-taupe n (* .4 d) :rfreq 900  :ramp 10)
        )
      )
    (aat (+ time #[.2 b]) #'f it (+ .2 beat))))
(f (now))
(fp 1 0)
(let ((notes (make-cycle (subseq *notes-lead* 0 20))))
  (defun g (time)
    (let* ((n (next notes))
           (i (first n))
           (r (* .8 (second n))))
      ;;(play-blue (+ 12 i) r)
      (p time (+ 12 i) 50 r 1)
      (aat (+ time #[r b]) #'g it))))

(g (now))
(fp 1 21)
(defun g ())
