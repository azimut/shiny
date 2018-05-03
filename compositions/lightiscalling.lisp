(in-package :somecepl)

(fp 1 89)
(defun g ())
(defun g (time)
  (p time 60 (rcosr 45 5 1/2) 1.2 1)
  (aat (+ time 1) #'g it))
(g (quant 4))

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

(defun piano ())
(defun piano (time)
  (let ((cc nil))
    (if (> (random 1.0) .75)
        (setf cc (list (first c) (second c) (1+ (third c)) (fourth c)))
        (setf cc c))
    (pa time cc .25 20 0 .25)
    (pa (+ 1 time) cc .25 20 0 .25)
    (pa (+ 2 time) cc .25 20 0 .25)
    (pa (+ 3 time) cc .25 20 0 .25))
  (aat (+ time 4) #'piano it) )

(piano (quant 4))

(fp 3 22)
(defun g ())
(defvar l (new cycle  :of '(0 0 0 0 1 1 2 3 4)))
(defun g (time)
  (synth 'soprano :freq (nth 1 c) :sus .6 :fmod .2 :amp .15 :rate 2 :pan (random-elt #(0 1)))
  (aat (+ time 1) #'g it))

(g (quant 4))

;; !!
(fp 1 72)
(fp 1 52)
(defun cla ())
(defun cla (time)
  (p time (qtanr (scale 0 'lydian) 80 5 3) 20 2 1)
  (aat (+ time 2) #'cla it))
(cla (quant 4))

