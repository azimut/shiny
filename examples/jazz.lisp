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

(defun jazz-high-hat (time)
  (play-midi-note time *gm-closed-hi-hat* (round (cosr 35 3 1/2)) .33 0)
  (aat (+ time #[2 b]) #'jazz-high-hat it))

(jazz-high-hat (tempo-sync #[1 b]))

(defun jazz-drums (time &optional notes rhythms amps)
  (when (null notes)
    (setf notes (cm:next
                 (cm:new cm:weighting
                   :of
                   `((0 :weight .25)
                     ,cm::+electric-snare+
                     ,cm::+acoustic-bass-drum+))
                 't)))
  (when (null rhythms)
    (setf rhythms (cm:rhythm '(t4 t8 t4 t8 t4 t8) 30)))

  (when (null amps)
    (setf amps (cm:next
                (cm:new cm:weighting
                  :of '(31 (40 :weight .1)))
                't)))

  (let ((rhythm (first rhythms)))
    (play-midi-note
     time (first notes) (first amps) rhythm 7)
    (aat (+ time #[rhythm b]) #'jazz-drums
         it (rest notes) (rest rhythms) (rest amps))))

(jazz-drums (tempo-sync #[1 b]))

;; wt
;; is weight of resting relative to playing
;; return weighting pattern that slightly prefers

;; playing a ride 1 pattern
;;    over a ride 2 pattern
(defun or12r (wt)
  (cm:new cm:weighting
    :of (list
         (list
          (cm:new cm:weighting
            :of `(51 (0 :weight ,wt))
            :for 1) :weight 1.5)
          (cm:new cm:weighting
            :of `(59 (0 :weight ,wt))
            :for 1))
    :for 1))

(defun lamps (amps)
  (mapcar
   #'round
   (mapcar
    (lambda (x) (cm:interp x 0.0 10 1.0 40))
    (cm:amplitude amps))))

;;  1  -  x    1  -  1    1  x  x    1  x  1
(defun jazz-cymbals (time rhythm &optional notes amps)
  (when (null notes)
    (setf notes (cm:next (cm:new cm:cycle :of (list
                 cm::+ride-cymbal-1+ 0 (or12r 5)
                 cm::+ride-cymbal-1+ 0 cm::+ride-cymbal-1+
                 cm::+ride-cymbal-1+ (or12r 7) (or12r 7)
                 cm::+ride-cymbal-1+ (or12r 3) cm::+ride-cymbal-1+)) 't)))
  (when (null amps)
    (setf amps (lamps '(:mf :mp :fff :f
                        :mp :ffff :mf :mp
                        :fff :f :mp :ffff))))
  (play-midi-note time (first notes) (first amps) rhythm 8)
  (aat (+ time #[rhythm b]) #'jazz-cymbals
       it rhythm (rest notes) (rest amps)))

(fluidsynth:program-change *synth* 8 1)
(jazz-cymbals (tempo-sync #[1 b]) (cm:rhythm 't8 60))

;;;
;; <3
;;;

(defun jazz-piano (time &optional notes rhythms root)

  (when (null notes)
    (setf root (random-elt *jazz-changes*))
    (setf notes (cm:next
                 (cm:new cm:weighting
                   :of `(0 (,(cm:new cm:heap
                               :of *jazz-scale*
                               :for (cm:new cm:weighting
                                      :of '(1 2 3 4)))
                             :weight ,(cm:new cm:weighting
                                        :of '(1.15 1.65)))))
                 't)))

  (when (null rhythms)
    (if (null (cm:odds .2))
        (setf rhythms (repeat 8 '(1)))
;;        (setf rhythms (cm:rhythm (repeat 8 '(t8)) 60))
        (setf rhythms (cm:rhythm '(t4 t8 t4 t8 t4 t8 t4 t8) 60))))
  
  (let ((rhythm (car rhythms)))
    (play-midi-note
     time
     (cm:keynum (cm:transpose (car notes) root))
     (round (cosr 30 3 3/4))
     rhythm
     1)
    (aat (+ time #[rhythm b])

         #'jazz-piano
         it
         (cdr notes)
         (cdr rhythms)
         root)))

(jazz-high-hat (tempo-sync #[1 b]))
(jazz-cymbals  (tempo-sync #[1 b]) (cm:rhythm 't8 60))
(jazz-piano    (tempo-sync #[1 b]))

(jazz-high-hat (funcall *metro* (funcall *metro* 'get-beat 4)) )
(jazz-cymbals  (funcall *metro* (funcall *metro* 'get-beat 2)) (cm:rhythm 't8 60))
(jazz-piano    (funcall *metro* (funcall *metro* 'get-beat 4)) )

(flush-pending)
