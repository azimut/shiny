;;;
;; jazz.lisp
;;;


(defparameter *jazz-scale* ; dorian with decorated octave
  '(0 2 3 5 7 9 10 12 14)) 

(defparameter *jazz-changes* ; key changes
  #(bf3 ef4 bf3 bf ef4 ef bf3 bf f4 ef bf3 bf))

(defun play-midi-note (time pitch velocity dur c)
  (when (and
         (not (equal pitch "r"))
         (> pitch 0)
         (> velocity 0)
         (> dur 0))
    (progn
      (at time #'fluidsynth:noteon
          *synth* c pitch velocity)
      (at (+ time #[dur b]) #'fluidsynth:noteoff
          *synth* c pitch))))


#|
(defun jazz-high-hat (time &optional notes)
  (when (null notes)
    (setf notes (cm:next
                 (cm:new cm:cycle
                   :of (list 0 *gm-closed-hi-hat*
                             0 *gm-closed-hi-hat*))
                 't)))
  (play-midi-note time (first notes) (round (cosr 35 5 1/2)) .33 1)
  (aat (+ time #[1 b]) #'jazz-high-hat it (rest notes)))
|#
(defun jazz-high-hat ())
(defun jazz-high-hat (time)
  (p time 42 (rcosr 25 3 1/2) 1/3 0)
  (aat (+ time 2) #'jazz-high-hat it))

(jazz-high-hat (quant 4))

(defbeat hithat 1 "x-x-"
  (p time 42 (rcosr 20 3 1/2) 1/3 0))

(hithat)
(defun %hithat ())

(let ((notes   (make-weighting
                 '((0 :weight .25)
                   40
                   35)))
      (rhythms (make-cycle '(2/3 4/3)))
      (amps    (make-weighting '(21 (30 :weight .1)))))
  (defun jazz-drums (time)   
    (let ((rhythm (next rhythms)))
      (p
       time (next notes) (next amps) rhythm 0)
      (aat (+ time rhythm) #'jazz-drums
           it))))

(defun jazz-drums ())
(jazz-drums (quant 4))
(hithat)

;; wt
;; is weight of resting relative to playing
;; return weighting pattern that slightly prefers

;; playing a ride 1 pattern - 51
;;    over a ride 2 pattern - 59
(defun or12r (wt)
  (make-weighting
   (list
    (list
     (make-weighting `(51 (0 :weight ,wt)) 1)
     :weight 1.5)
    (make-weighting `(59 (0 :weight ,wt)) 1))
   2))

(defun lamps (amps)
  (mapcar
   #'round
   (mapcar
    (lambda (x) (cm:interp x 0.0 30 1.0 70))
    (cm:amplitude amps))))

; Triplet 8th: 1  2  3    4  5  6    7  8  9   10 11 12
; Cymbals:     1  -  x    1  -  1    1  x  x    1  x  1 
(defun jazz-cymbals (time rhythm &optional notes amps)
  (setp notes (next (new cycle
                      :of (list
                           51 0 (or12r 5)
                           51 0 51
                           51 (or12r 7) (or12r 7)
                           51 (or12r 3) 51))
                    't))
  (when (null amps)
    (setf amps (lamps '(:mf :mp :fff :f
                        :mp :ffff :mf :mp
                        :fff :f :mp :ffff))))
  (p time (first notes) (+ -20 (first amps)) rhythm 0)
  (aat (+ time rhythm) #'jazz-cymbals
       it rhythm (rest notes) (rest amps)))

(jazz-cymbals (quant 4) (cm:rhythm 't8 60))
(defun jazz-cymbals ())

;;;
;; <3
;;;

(defun jazz-piano (time &optional notes rhythms root)
  (when (null notes)
    (setf root (random-elt *jazz-changes*))
    (setf notes (next
                 (make-weighting
                  (list
                   (list (make-heap *jazz-scale* (make-weighting '(1 2 3 4)))
                         :weight (make-weighting '(1.5 1.65)))
                   0))
                 't)))
  (when (null rhythms)
    (if (null (odds .65))
        (setf rhythms (repeat 8 '(2/3 1/3)))
        (setf rhythms (repeat 8 '(1/3)))
;;        (setf rhythms (repeat 8 '(1)))
;;        (setf rhythms (cm:rhythm (repeat 8 '(t8)) 60))
;;        (setf rhythms (cm:rhythm '(t4 t8 t4 t8 t4 t8 t4 t8) 60))
        ))  
  (let ((rhythm (car rhythms)))
    (p
     time
     (cm:keynum (cm:transpose (car notes) root))
     (round (cosr 50 3 3/4))
     rhythm
     11)
    (aat (+ time rhythm)

         #'jazz-piano
         it
         (cdr notes)
         (cdr rhythms)
         root)))

(fp 11 62)

(jazz-high-hat (tempo-sync #[1 b]))
(jazz-cymbals  (tempo-sync #[1 b]) (cm:rhythm 't8 60))
(jazz-piano    (quant 4))
(defun jazz-piano ())


(jazz-high-hat (funcall *metro* (funcall *metro* 'get-beat 4)) )
(jazz-cymbals  (funcall *metro* (funcall *metro* 'get-beat 2)) (cm:rhythm 't8 60))
(jazz-piano    (funcall *metro* (funcall *metro* 'get-beat 4)) )

(flush-pending)
