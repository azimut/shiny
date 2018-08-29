(in-package :shiny)

(fp 2 44)
(defparameter *current-note* 120)
(defun v ())
(let ((n (new cycle :of '(2 4 0 2)))
      (i (new cycle :of '(i iv v))))
  (defun v (time)
    (let ((c (make-chord 50 70 3 (pc-diatonic 0 '- (next i)))))
;;      (setf *current-note* (first c))
      (p time c 35 (next n) 2))
    (aat (+ time 2) #'v it)))
(v (quant 4))

(fg 1f0)
(fp 1 89)
(defun g ())
(let* ((pan 63)
       (pan (make-cycle '(0 127))))
  (defun g (time)
    (p time 60 (rcosr 45 5 1/2) 1.2 1 :pan (next pan))
    (aat (+ time 1) #'g it)))
(g (quant 4))

(fp 8 52)
(fp 0 40)
(defparameter *other-note* 40)
(defun c ())
(defun c (time start pitch &optional duration)
  (p time pitch 20 duration 0)
;;  (setf *other-note* pitch)
  (aat (+ time duration) #'c it
       (if (> pitch (- start 12))
           start
           (pc-relative start -1 (scale 0 'ryukyu)))
       (if (> pitch (- start 12))
           (pc-relative pitch -1 (scale 0 'ryukyu))
           start)
       (random-elt #(1 2 3))))
(c (quant 4) 72 72 1)

;; --------------------------------------------------

(fp 0 89)
(fp 4 5)
(freverb-toggle 1)
(freverb-preset 6)
(fg .5)

(defparameter c (make-chord 60 75 4 (scale 0 'lydian)))

(defun piano1 ())
(defun piano1 (time)
;;  (pa time c 2 20 0 2)
  (and (odds .25) (p time (+ -12 (random-elt c)) 25 (pick 4 8) 4))
  (and (odds .75) (p time (+ 12 (random-elt c)) 30 (pick 4 8) 4))
  (aat (+ time 8) #'piano1 it))

(piano (quant 4))

(pa (quant 4) c 4 20 0 4)
(piano1 (quant 4))

(all-piano 80)

(defun piano ())
(defun piano (time &optional (channel 0))
  (let ((cc nil))
    (if (> (random 1.0) .75)
        (setf cc (list (first c) (second c) (1+ (third c)) (fourth c)))
        (setf cc c))
    (pa time cc .25 20 channel .15)
    (pa (+ 1 time) cc .25 40 channel .15)
    (pa (+ 2 time) cc .25 40 channel .15)
    (pa (+ 3 time) cc .25 40 channel .15))
  (aat (+ time 4) #'piano it channel) )

(piano (quant 4) 0)
(piano (quant 2) 30)
(fp 30 59)

(fp 3 22)
(defun g ())
;;(let ((l (new cycle  :of '(0 0 0 0 1 1 2 3 4)))))
(defun g (time)
  (synth 'soprano :freq (nth 1 c) :sus .6 :fmod .2 :amp .1 :rate 2 :pan (random-elt #(0 1)))
  (aat (+ time 1) #'g it))

(g (quant 4))

;; !!
(fp 1 72)
(fp 1 52)
(fp 1 80) ;; majora piano
(defun cla ())
(defun cla (time)
  (p time (qtanr (scale 0 'lydian) 80 5 3) 60 2 1)
  (aat (+ time 2) #'cla it))
(cla (quant 4))

(fp 10 30)
(fp 10 29)
(fp 10 28)

(fp 10 50)
(defun strin ())
(defun strin (time)
  (let ((r (pick 4 2 2 1)))
    (p time (qcosr (scale 0 'lydian) 80 5 4) 40 r 10)
    (aat (+ time r) #'strin it)))
(strin (quant 4))

(fp 11 29)
(defun f ())
(let ((i (new cycle :of '(i vi ii v)))
      (r (new cycle :of '(2 2 2 2))))
  (defun f (time)
    (let ((mychord (make-chord 80 90 3 (pc-diatonic 0 'major (next i))))
          (rr (next r)))
;;      (pa time mychord .5 90 1 .5)
      (p time mychord 40 rr 11)
      (aat (+ time rr) #'f it))))

(f (quant 4))
