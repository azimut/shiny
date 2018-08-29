(in-package :shiny)

(pat1 (now))

(all-piano 0)
(fp 2 10)
(fp 1 80)
(fp 3 22)

(defparameter *trigger* (make-trigger))

(defun pat1 ())
(defpattern pat1 ((get-pattern 'newday) .3)
  (p time 50 50 (/ d 3) 0)
  (p time 50 50 d 1)
  (with-trigger-expire (*trigger* .1)
    (p time 50 50 d 3)))


(bbuffer-load "/home/sendai/projects/Moonlet/Samples/Casio/cab.ogg")
(bbuffer-load "/home/sendai/projects/Moonlet/Samples/Casio/iron.ogg")
()
(defun f ())
(let ((s (make-cycle (scale 0 'aeolian)))) (defun f (time)
          ;; (and (odds .1) (bbplay (gethash "cab.ogg" *buffers*)
          ;;                 :attenuation 1d0
          ;;                 :rpitch (next s)))
          (bbplay (gethash "iron.ogg" *buffers*)
                  :attenuation 1d0
                  :rpitch (funcall (pick #'+ #'-) (next s)))
          (aat (+ time #[1 b]) #'f it)))

(f (now))
