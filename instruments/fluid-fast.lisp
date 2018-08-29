(in-package :shiny)

;;(pa (now) '(60 62 64) 60 1 1)

(defmacro arpegg (time pitches velocity channel duration lnotes offsets)
  `(with-schedule
     (loop
        :for i :from 1 :upto ,lnotes
        :for p :in ,pitches
        :for o :in ,offsets :do
        (at (+ ,time #[o b]) #'fluidsynth:noteon *synth* ,channel p ,velocity)
        (at (+ ,time #[(* i ,duration) b]) #'fluidsynth:noteoff *synth* ,channel p))))


(defmacro chor (time pitches velocity channel duration)
  `(loop :for p :in ,pitches :do
      (fluidsynth:noteon *synth* ,channel p ,velocity)
      (at (+ ,time #[,duration b]) #'fluidsynth:noteoff *synth* ,channel p)))

;;(chor (now) '(60 62 64) 60 1 1)

;; (defgeneric pa (time pitches offset velocity channel duration)
;;   (:method (time pitches offset velocity channel duration)
;;     (p time (first pitches) velocity duration channel)
;;     (loop :for p :in (cdr pitches) :do
;;        (p (+ time #[b]) p velocity 1 ))))

(defgeneric pa (time pitches offset velocity channel &key duration)
  (:method (time pitches (offset number) (velocity integer) (channel integer)
            &key (duration offset))
    "Play arpeggio, with a constant offset and velocity"
    (let* ((lnotes (length pitches))
           (offsets (loop :for i :from 0 :by offset :collect i :repeat lnotes)))
      (arpegg time pitches velocity channel duration lnotes offsets))))

;;(arpegg (now) '(60 64 66) 60 1 1 3 '(.2 .4 .6))
