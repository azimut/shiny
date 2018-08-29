(in-package :somecepl)

(defun kick ())
(defun kick2 ())
(defun kick3 ())

(kick (tempo-sync #[1 b]))
(kick2 (tempo-sync #[.5 b]))
(kick3 (tempo-sync #[.5 b]))

(let ((notes (make-cycle (make-chord 60 70 6 (scale 0 'minor)))))
  (defbeat kick ((parse-pattern (bjorklund-s 5 13)) 1)
    (p time (next notes) 50 (* 2 d) 0)))

(let ((notes (make-cycle (make-chord 50 60 1 (scale 0 'minor)))))
  (defbeat kick2 ((parse-pattern (bjorklund-s 4 4)) 1)
    (p time (next notes) 50 d 2)))

(let ((notes (make-cycle (reverse (make-chord 60 70 6 (scale 0 'minor))))))
  (defbeat kick3 ((parse-pattern (bjorklund-s 3 8)) .5)
    (p time (next notes) 50 (cosr .45 .1 4) 3)))

(fg .5)
(fp 3 30)
(fp 2 44)
(fp 0 20)

(freverb-toggle 1)
(freverb-preset 6)

;;--------------------------------------------------

(let ((bd (make-cycle (bd (get-pattern 'getup))))
      (sn (make-cycle (sn (get-pattern 'getup))))
      (ch (make-cycle (ch (get-pattern 'getup))))
      (oh (make-cycle (oh (get-pattern 'getup)))))
  (defun f (time)
    (when (next bd)
      (play-instrument 'drum *gm-kick* (pick .2 .35)))
    (when (next sn)
      (play-instrument 'drum *gm-snare* (pick .35 .4)))
    (when (next ch)
      (play-instrument 'drum *gm-closed-hi-hat* .35 (pick .3 .4)))
    (when (next oh)
      (play-instrument 'drum *gm-open-hi-hat* .35 .6 (pick 1 .9 1.3)))
    (aat (+ time #[.15 b]) #'f it)))

(defun f ())
(f (now))

;;--------------------------------------------------

(let (;;(p (parse-patternc "x-x- ---x"))
      (p (make-cycle (bd (get-pattern 'cold))))
      (s (make-cycle (sn (get-pattern 'cold))))
      (c (make-cycle (ch (get-pattern 'cold))))
      (o (make-cycle (oh (get-pattern 'cold))))
      (dur .1))
  (defun drumplay (time)
    (when (next p) (play-instrument 'drum *gm-kick*))
    (when (next s) (bbplay (gethash "snare_OH_FF_9.wav" *buffers*)))
    (when (next c) (play-instrument 'drum *gm-closed-hi-hat* :amp .5))
    (when (next o) (play-instrument 'drum *gm-open-hi-hat* :amp .1 :dur .1))
    (aat (+ time #[ (cosr .2 .1 1) b])
         #'drumplay it)))

(defun drumplay ())
(drumplay (now))

(defbeat kick ((bd (get-pattern 'funkyd)) .5)
  (play-instrument 'drum *gm-kick*))

(defun kick ())
(snare (tempo-sync #[1 b]))

(let ((hh (make-cycle '(.25))))
  (defpattern pat1 ((get-pattern 'wm) .25)
    (play-instrument 'drum *gm-kick* :dur .25)
    (play-instrument 'drum *gm-snare* :dur .25)
   ;; (play-instrument 'drum *gm-closed-hi-hat* :dur 1)
   (play-instrument 'drum *gm-open-hi-hat* :dur (next hh))
    ))

(defun pat1 ())
(pat1 (now))
