(in-package :somecepl)

(fluidsynth:program-change *synth* 1 33)
(fluidsynth:program-change *synth* 1 40)
(fluidsynth:program-change *synth* 1 74)

(all-piano 1)

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
      (p time k (rcosr 40 5 3/4) 2 4))
    (aat (+ time 2) #'ew it (cdr chords))))

(ew (quant 4))
(defun ew ())
