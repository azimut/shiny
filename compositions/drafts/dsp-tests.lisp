(in-package :shiny)

(defun f ())
(f (now))
(let ((c (make-cycle (make-chord-fixed 50 3 (scale 0 'minor)))))
  (defun f (time)
    (prophet :amp .3 :freq (midihz (next c)))
    (aat (+ time #[1 b]) #'f it)))
(prophet :amp .3)

;;--------------------------------------------------
(defun f ())
(let ((maj (new weighting :of
                (list (list (list (cm:keynum :d3)) :weight .5)
                      (list (ov-scale :d3 :major) :weight .2))))
      (min (new weighting :of
                (list (list (list (cm:keynum :b4)) :weight .9 :max 2)
                      (list (ov-scale :b4 :minor) :weight .2))))
      (flip (make-cycle '(nil t))))
  (defun f (time)
    (prophet :dur .6 :amp .3 :freq (midihz (pickl (next maj))))
    (when (next flip)
      (square :dur (cosr 1 .1 1) :amp .08 :freq (midihz (pickl (next min)))))
    (aat (+ time #[.5 b]) #'f it)))

(defun at-times (time time-offsets &rest args)
  (declare (double-float time) (list time-offsets)
           (optimize (speed 3)))
  (mapcar (lambda (x) (apply #'at
                        (+ time (* *SAMPLE-RATE* (* (SAMPLE x) (SPB *TEMPO*))))
                        args))
          time-offsets))


(defun counter-melody (time d)
  (square :amp .2
          :freq (midihz (pc-relative (first d) -3 (scale 0 'lydian))))
  (at (+ time #[.5 b])
      #'square :amp .5 :freq (midihz (pc-relative (second d) -5
                                                  (scale 0 'lydian))))
  (at (+ time #[1 b])
      #'square :amp .4 :freq (midihz (pc-relative (first d) -4 (scale 0 'lydian))))
  ;;(at (+ time #[1 b]) (lambda () (setf *distance* (drunk *distance* 1f0 :low 0f0 :high 2f0))))
  )

(defun extended-melody (time d)
  (at (+ time #[(pick 1 .75) b])
            #'dsp-pulse
            :amp .2
            :dur .6
            :freq (midihz (pc-relative (third d)
                                       (odds .5 -1 1)
                                       (scale 0 'lydian)))
            :cutoff-freq (cosr 700 200 3))
  (at (+ time #[1.25 b])
            #'square
            :amp .1
            :dur .4
            :freq (midihz (+ 12 (first d)))
            ;;:cutoff-freq (cosr 700 200 3)
            ))


(defparameter *line-xp* (make-line (iota 40 :start .05 :step .05)))
(setf *distance* 0f0)
(defun f ())
(f (now))
(let ((s (make-palindrome (nths '(0 2 4) (ov-scale :c5 :lydian)))))
  (defun f (time)
    (setf *exposure* (next *line-xp*))
    (let ((c (make-chord 50 60 3 (rotate (scale 0 'ionian) 0))))
      (mapcar (lambda (x) (square :dur 1.5 :amp .7 :freq (midihz x))) c)
      (at-times time '(1 1.25) #'prophet :dur .5 :amp .5 :freq (midihz (third c)))
      (at (+ time #[.5 b]) #'prophet :dur .5 :amp .5 :freq (midihz (first c)))
      (let (;;(d (pc-invert (next s 5) (scale 0 'lydian) ))
            (d (next s 5))
            )
        ;;(green :dur (odds .6 1.5 1) :freq (midihz (pc-random 90 100 (scale 0 'lydian))))
        (dsp-pulse :amp .2 :freq (midihz (first d)) :cutoff-freq (cosr 700 200 3))
        (setf *offset* (random 1f0))
        ;;(counter-melody time d)
        (extended-melody time d)
        (at (+ time #[(pick 1 .5 .75) b])
            #'dsp-pulse
            :amp .1
            :dur .6
            :freq (midihz (pc-relative (second d)
                                       (odds .5 -1 1)
                                       (scale 0 'lydian)))
            :cutoff-freq (cosr 700 200 3))))
    (aat (+ time #[1.5 b]) #'f it)))

(defun f ())
(incudine:free (node 0))



