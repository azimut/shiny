(in-package :somecepl)

(fluidsynth:program-change *synth* 1 33)
(fluidsynth:program-change *synth* 1 40)
(fluidsynth:program-change *synth* 1 74)

;; expwarp test
;; Try:
;; - Modify the dur and aat
;; - Reduce the notes on make-chord
;; - Change the pc scale
(defun ew (time &optional chords)
  (let ((x (make-chord 35 64 2 (scale 0 'ryukyu))))
    (setp chords (loop :for n :from 1 :to 2 :by .1 :collect
                    (expwarp x n))))
  (let ((chord (car chords)))
    (dolist (k chord)
      (play-midi-note time k (rcosr 40 10 3/4) (random-elt #(2 1.5 3)) 1))
    (aat (+ time #[4 b]) #'ew it (cdr chords))))

(ew (now))
