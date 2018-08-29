(in-package :shiny)
(let ((scale (make-cycle (list (scale 0 'aeolian)
                               (nth-inc 3 (scale 0 'aeolian) -1))
                         3))
      (top (make-cycle '(70 75) 6))
      (n   (make-cycle '(0 2) 12)))
  (defun f (time)
    (let ((c (make-chord 60 (next top) 3 (next scale))))
      (p (list time (+ time 1))
         (nth (next n) c)
         50
         .5 0)
      (if (odds .5)
          (progn
            (pa time c 2/3 60 2 :dur 2/3)
            (pa (+ .5 time) (reverse c) 2/4 60 2 :dur 2/4))
          (p time c 40 2 2)))
    (aat (+ time 2) #'f it)))

(defun f ())
(f (now
    ))
(fp 2 90);;megadrive 90
(fp 4 80)
(fp 6 32)
(fg 1.0)

(let ((scale (make-cycle (list (scale 0 'aeolian)
                               (nth-inc 4 (scale 0 'aeolian) -1))
                         3))
      (top (make-cycle '(70 75) 6))
      (dur (make-cycle '(2 1 .75 .5) 12))
;;      (dur (make-cycle '(.5 .75 1 2) 12))
;;      (dur 2)
      )
  (defun f (time)
    (let ((c (reverse (make-chord 60 (next top) 3 (next scale))))
          (d (next dur)))
      ;;(pa time (subseq c 0 2) (/ d 2) 50 0 (/ d 2))
      (p time c 40 d 2)
      (aat (+ time d) #'f it))))

(fp 2 0)
(fp 4 0)

(let ((scale (make-cycle (list (scale 0 'aeolian)
                               (nth-inc 4 (scale 0 'aeolian) -1))))
      (top (make-cycle '(70 75) 6))
      (fac (make-cycle '(1 1 1 1 1 1 1 1 1 1 1 2 2 2 2)))
      (song (make-cycle '(72 70 67 65 63 65 68 63 60 63 55 72 67 62 70 75 60 68 65 67 75 70 72 72 62 63 58 60)))
      (dur 2))
  (defun f (time)
    (let* ((myscale (next scale))
           (c (make-chord-waltz 60 (next top) myscale))
           (d (next dur))
           (f    (next fac)))
      ;; (and (odds .6) (p time (next song) 60 .5 0))
      ;; (p (+ .5 time) (next song) 50 .25 1)
      ;; (and (odds .5) (p (+ .75 time) (next song) 50 .25 0))
      ;; (and (odds .1) (p (+ 1 time) (next song) 50 .5 1))
      (and (odds (* f .7))
           (p time
              (+ 12 (pickl (second c)))
              60 .45 1))
      (let ((note (+ 12 (pickl (second c)))))
        (and (odds (* f .8))
             (p (+ .5 time)
                note
                60 .25 1))
        (and (odds (* f .2))
             (p (+ .75 time)
                (pc-relative note (pick +1 +1 0) myscale)
                60 .5 3)))
      ;;(pa time (subseq (second c) 0 2) (/ d 2) 50 0 (/ d 2))
      (if (odds .5)
          (pa time c '(0 1 1.5) (list (rcosr 40 5 5) 35 35) '(2 4 2) (list 1 (pick .4 .5) (pick .4 .5)))
          (pa time (subseq c 0 2) '(0 1) (list (rcosr 40 5 35) 35) '(2 4) '(1 1)))
      (aat (+ time d) #'f it))))

;;--------------------------------------------------

(defparameter *bach*
  (mapcar (lambda (x) (cm:keynum x))
          '(g3 fs3 e3 d3 d3 b2 c3 d3 g2)))

;; 4.5.3 - BWV 1087
(let ((n (make-cycle *bach*))
      (m (make-cycle (reverse *bach*))))
  (defun f (time)
    (p time (next n) 50 1 0)
    (p time (next m) 50 1 1)
    (aat (+ time 1) #'f it)))

;; 4.5.4 - BWV 1072
;; choir 1
(let ((n (make-cycle
          (list (make-cycle (chord :C4 :major) 1)
                (make-cycle (chord :D4 :minor) 1))))
      (r (make-cycle '(1.5 .5))))
  (defun f (time)
    (let ((d (next r))
          (note (next n)))
      (p time note 50 d 2)
      (p (+ .5 time) note 50 (max .5 d) 4)
      (p (+ 1 time) note 50 (max .5 d) 6)
      (aat (+ time d) #'f it))))

;; choir2
(let ((n (make-cycle
          (list (make-cycle (chord :D4 :minor) 1)
                (make-cycle (chord :C4 :major) 1))))
      (r (make-cycle '(1.5 .5))))
  (defun ff (time)
    (let ((d (next r)))
      (p time (+ 12 (next n)) 50 d 2)
      (aat (+ time d) #'ff it))))

(f (quant 3))
(ff (quant 3))
(fff (quant 3))

(fg .1)
(defun f ())
(defun ff ())
(defun fff ())

(let ((n (make-cycle
          (list (make-cycle (invert
                             (chord :C4 :major)
                             ) 1)
                (make-cycle (invert
                             (chord :D4 :minor)
                             ) 1))))
      (r (make-cycle '(1.5 .5))))
  (defun fff (time)
    (let ((d (next r)))
      (p time (+ 12 (next n)) 50 d 3)
      (aat (+ time d) #'fff it))))

(fp 3 0);;mega 50

;;--------------------------------------------------
;; 4.5.6
;; Arvo Part - Cantus
;; ME: basically a descending scale playing at different offsets
;;     adding some stuff
(let ((pitches (make-cycle '(0 69 0)))
      (durations (make-cycle '(3 3 6))))
  (defun bell (time)
    (let ((d (next durations))
          (p (next pitches)))
      (p time p 70 d 5)
      ;; (and (odds .2)
      ;;      (p (+ .5 time) p 70 d 5))
      (aat (+ time d) #'bell it))))

(bell (quant 3))
(defun bell ())

(let ((pitches   (make-cycle (reverse (ov-scale :A4 :locrian))))
      (durations (make-cycle '(.5 .25))))
  (defun violin (time)
    (let ((d (next durations))
          (pitch (next pitches)))
      (p time pitch 50 d 0)
      (and (odds .2) (p time (+ 12 pitch) (rcosr 80 2 5) d (pick 1 0)))
      (p (+ time .5) pitch 50 d 1)
      (p (+ time 1) (+ -12 pitch) 50 (* 2 d) 2)
      (p (+ time 1) (+ -12 pitch) 50 (* 2 d) 3)
      ;;(p (+ time 1) (+ -12 pitch) 50 (* 2 d) 4)
      (aat (+ time d) #'violin it))))

(fg .2)
(fp 2 33)
(violin (quant 3))
(defun violin ())

(mapcar (lambda (x) (fp x 0)) '(0 1 2 3 4))

(fp 2 0)

;; ?
(freverb-preset 6)
(defun frev (time)
  (freverb-toggle 0)
  (freverb-toggle 1)
  (aat (+ time 2) #'frev it))
(frev (quant 4))
(defun frev ())

(fluidsynth:cc *synth* 2 10 65)

(fluidsynth:cc *synth* 1 10 0)
(fluidsynth:cc *synth* 0 10 127)

(defun fpan (time)
  (and (odds .2)
       (fluidsynth:cc *synth* (pick 0 1) 10 (pick 0 127)))
  (aat (+ time 1) #'fpan it))

(fpan (quant 4))
(defun fpan ())


;;--------------------------------------------------

(let ((notes (make-weighting '(:b1 :b2 :e1 :e2 :b3 :e3))))
  (defun f (time)
    (p time (pick (chord (next notes) :minor)) 50 (cosr .7 .3 3) 0)
    ;;(fpitch 0 (between 10000 15000))
    (aat (+ time .5) #'f it)))

(f (quant 4))
(defun f ())

(freset)
(fpitch 0 (between 1000 10000))
(fluidsynth:cc *synth* 0 10 64)

