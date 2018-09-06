(in-package :shiny)

;; ----------------------------
;; Euclidean Rythms
;; https://github.com/overtone/overtone/blob/master/src/overtone/examples/compositions/euclidean_rhythms.clj
;; ----------------------------

(setf (bpm *tempo*) 60)

(defun f ())
(defparameter *metro* (make-metro 60))
(defparameter *metre* (make-metre '(2 3 2) 2))
(let ((r (make-cycle (parse-pattern (bjorklund 3 8))))
      (s (make-cycle (parse-pattern (bjorklund 5 12))))
      (q (make-cycle (parse-pattern (bjorklund 4 12)))))
  (defun f (time &optional (beat 0))
    (when (funcall *metre* beat 1)
      (play-crash 72 2))
    (when (next r) (play-bass 60 .5 :amp .1 :bars 120))
    (when (next s) (play-snare .5 :amp 5000))
    (when (next q) (play-hihat 1 :amp 5000))
    (aat (+ time #[.5 b]) #'f it (+ .5 beat))))
(f (now))
