(in-package :shiny)

;;--------------------------------------------------

(mbox 4 32 :C 9 14.5 -14 14 (scale 0 'dorian))

(fg .4)

(mbox-play 0 40 .5 1 0)
(mbox-play 1 40 .5 1 0)
(mbox-play 2 40 .5 1 0)
(mbox-play 3 40 .5 1 0)

(mbox-play 10 40 .5 1 0)
(mbox-play 11 40 .5 1 0)
(mbox-play 12 40 .5 1 0)
(mbox-play 17 40 .5 1 0)

(fp 1 40)

(mbox-custom 9 .5
  (if (odds .8)
      (p time (+ 12 note) 35 (* duration .45) 1)
      (pa time (pickn (pick 2 3) (make-chord-fixed (+ note 12) 3 (scale 0 'dorian)))
          (* duration .1)
          35 1
          (list (* duration .1) (* duration .1) (* duration .2))))
  )

(mplay-9)

(mplay-0)
(mplay-1)
(mplay-2)
(mplay-3)

(mplay-10)
(mplay-11)
(mplay-12)

(mplay-17)

;;--------------------------------------------------

(all-piano 0)

(mbox 4 32 :C 9 14.5 -14 14 (scale 0 'ryukyu))

(mplay-2)

(defun %mplay-2 ())

(fp 11 78)

(mbox-play 0 50  .5 1 1)
(mbox-play 1 50  .5 1 1)
(mbox-play 2 50  .5 1 1)
(mbox-play 3 60  .5 1 1)
(mbox-play 10 60 .5 .05 10)
(mbox-play 11 60 .5 .05 11)
(mbox-play 7 60  .5 1 1)

;;--------------------------------------------------

;;; Name on soundcloud - "trash"

(fp 0 4)
(fp 1 0)
(fp 5 2)
(fp 9 80 0)
(all-piano 80)

(mbox 4 8 :C 3 5 -7 14 (scale 0 'lydian))

(mbox-play 3 40 .1 .1 0)

(fg 1.0)
(fp 0 13)

(mplay-3)

(defun %mplay-2 ())
(mbox-play 7 50 .2 . 1)
;;--------------------------------------------------

(fp 0 4)
(fp 1 0)
(fp 5 2)
(fp 9 1 0)
(all-piano 80)

(mbox 4 8 :C 3 5 -7 14 (scale 0 'lydian))

(mbox-play 9 40 .2 .1 3)

(fp 3 8)

(mplay-1)

(defun %mplay-8 ())
(mbox-play 7 50 .2 . 1)

;;--------------------------------------------------


;; V: 1 2 , 3 , 12 13
(mbox 4 8 :C 3 7 -7 14 (scale 0 'ryukyu))


(mbox 4 8 :C 3 7 -7 14 (ov-pc-scale :scriabin))
(mbox 4 11 :C 3 7 -7 14 (ov-pc-scale :mixolydian))

(all-piano 0)

(fpitch 10 10000)
(fpitch 11 5000)

(mplay-0)
(mplay-1)
(mplay-2)
(mplay-3)

(fg .7)

(mbox-play 0 30 .2 .1 1)
(mbox-play 1 30 .2 .1 1)
(mbox-play 2 30 .2 .1 1)
(mbox-play 3 60 .2 .1 1)
(mbox-play 5 60 .2 .4 1)
(mbox-play 10 60 .2 .4 10)
(mbox-play 14 60 .2 .09 11)
(mbox-play 8 60 .2 .2 1)

(defun %mplay-6 m ())

(mplay-1)
(mplay-10)
(mplay-11)
(mplay-3)

(fp 30 0)
(defun strin ())
(defun strin (time)
  (let ((r (pick .2 .4 1 1)))
    (and (odds .5)
         (p time (qcosr (ov-pc-scale :minor) 75 5 5) (rcosr 50 5 1/2) r 30)
         
         )
    (aat (+ time r) #'strin it)))
(strin (quant (* .2 8)))


(mbox-play 0 40  .2 .1 1
 (p time note p-volume (* duration p-note-duration-mul) p-channel))



(defun mplay ())
(destructuring-bind (notes durations channel beats) (nth 11 (reverse *p*))
  (declare (ignore channel))
  (mplay (quant 4) (make-cycle notes)
         (make-cycle durations)
         2
         (make-cycle beats) 60))

(destructuring-bind (notes durations channel beats) (nth 2 (reverse *p*))
  (mplay (quant 4) (make-cycle notes) (make-cycle durations) 3 (make-cycle beats)))

(destructuring-bind (notes durations channel beats) (nth 3 (reverse *p*))
  (mplay (quant 4) (make-cycle notes) (make-cycle durations) 4 (make-cycle beats)))

(fp 5 25)
(freverb-toggle 1)
(freverb-preset 2)

(destructuring-bind (notes durations channel beats) (nth 14 (reverse *p*))
  (mplay (quant 4) (make-cycle notes) (make-cycle durations) 5 (make-cycle beats)))

(destructuring-bind (notes durations channel beats) (nth 8 (reverse *p*))
  (mplay (quant 4) (make-cycle notes) (make-cycle durations) 6 (make-cycle beats)))

(defparameter *metro* (make-metro 120))
(defun mplay ())
(defun mplay (time notes durations channel beats &optional (vol 60))
  (let ((beat (next beats)))
    (when (= 1 beat)
      (let ((note (next notes))
            (duration (next durations)))
        (p time note vol (/ duration 7) channel))))
  (aat (+ time .2) #'mplay it notes durations channel beats))

(freverb-toggle 1)
(freverb-preset 2)
(fp 2 4)
(fp 3 58)
(fp 4 58)
(fp 5 72)
(fp 6 58)
(fg .4)


(defun mm ())

(let ((rhythms (make-cycle '(1 2 1))))
  (defun mm (time)
    (let ((c (make-chord 70 85 3 (scale 0 'ryukyu)))
          (rhythm (next rhythms)))
      (if (odds .7)
          (pa time c (/ rhythm 3) 60 6 rhythm)
          (p time c 50 rhythm 6))
      (aat (+ time rhythm) #'mm it))))

(mm (quant 4))


#|

(mbox (funcall *metro* (funcall *metro* 'get-beat 4))
      32 :E 4 3 -7 7 (scale 1 'dorian))
 
(mbox 4 32 :C 9 14.5 -14 14 (scale 0 'dorian))

(mbox (tempo-sync #[1 b]) 32 :C 9 14.5 -14 14 (scale 0 'dorian))
(mbox (tempo-sync #[1 b]) 32 :G 10 3.5 -14 14 (scale 0 'dorian))

(mbox (tempo-sync #[1 b]) 32 :C 8 14.5 -7 14 (scale 0 'dorian))
(mbox (tempo-sync #[1 b]) 32 :D 9 1.5 -14 14 (scale 0 'dorian))

(mbox (tempo-sync #[1 b]) 32 :C 7 3 -7 14 (scale 0 'aeolian))

|#

#|
(all-piano *synth*)
(fluidsynth:program-change *synth* 3 43)
(fluidsynth:program-change *synth* 4 33)
(fluidsynth:program-change *synth* 6 43)
|#

#|
(flush-pending)
(off-with-the-notes)
|#


;;--------------------------------------------------


(mbox 4 32 :C 9 14.5 -14 14 (scale 0 'minor))
(let ((r  (make-cycle (parse-pattern (get-mbeats 0 8))))
      (n  (make-cycle (get-mnotes 0 8)))
      (rr (make-cycle (parse-pattern (get-mbeats 10 17))))
      (nn (make-cycle (get-mnotes 10 17))))
  (defun f (time)
    (when (next r)
      (let ((nn (next n)))
        (p time nn 50 1 1)
        (play-ivory nn 2 :amp 300 :vib .1)))
    (when (next rr)
      (let ((n (next nn)))
        (p time n 60 1 0)
        (play-blue n 2)))
    (aat (+ time #[.3 b]) #'f it)))

(f (now))
(defun f ())
(fp 1 40)
