(in-package :somecepl)

;;--------------------------------------------------
;; Monday blues - sonic-pi

;; THIS NEEDS examples/cv.lisp EVALUATED FIRST

(defun f ())
(defun ff ())
(defun fff ())

(let ((r (make-cycle (list (make-cycle '(.5) 6)
                           (make-cycle '(.125) 8)))))
  (defun f (time)
    (let ((d (next r)))
      (p time 60 50 d 0)
      (aat (+ time d) #'f it))))
(fp 0 0);;ctk 86 - nice 28
(f (quant 4))

(fluidsynth:cc *synth* 1 10 64)

(defun ff (time)
  (p time 52 50 1 1)
  (fluidsynth:cc *synth* 1 10 (pick 0 127))
  ;;(setf *p* 1)
  ;;(push (make-instance 'to-sec :sec 16) *vevents*)
  (aat (+ time 1) #'ff it))
(fp 1 50)
(ff (quant 1))

(let ((notes (make-cycle '(:F4 :C4 :D4 :D4 :G4 :C4 :D4 :D4)))
      (wh (make-heap '(100 300 500 200))))
  (defun fff (time)
    (let ((note (cm:keynum (next notes))))
      (and (= note (cm:keynum :D4))
           (progn
             (if *fh* (setf *fh* nil) (setf *fh* 1))
             (push (make-instance 'to-sec :sec (pick 300 900 800)) *vevents*)))
      ;;(p (+ .5 time) (+ 12 note) 50 .5 10)
      ;;(if *hsv* (setf *hsv* nil) (setf *hsv* 1))
      ;; (let ((x (next wh)))
      ;;   (setf *w* x
      ;;         *h* x))
      (and (odds .5) (p (+ .75 time) (+ (pick 7 12 7) note) 45 .5 10))
      (pa (+ .5 time) (make-chord-fixed note 3 (scale 0 'minor)) .4 60 9 .4)
      (p time (+ -12 note) 60 1 2)
      (p time (+ -24 note) 60 1 2)
      (aat (+ time 1) #'fff it))))

(fp 2 100);; ctk 100 - nice 28
(fp 10 1)
(fff (quant 1))
(fp 9 80)
