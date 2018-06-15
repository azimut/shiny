;;;
;; jazz.lisp
;;;


(defparameter *jazz-scale* ; dorian with decorated octave
  '(0 2 3 5 7 9 10 12 14)) 

(defparameter *jazz-changes* ; key changes
  #(bf3 ef4 bf3 bf ef4 ef bf3 bf f4 ef bf3 bf))

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

(defun jazz-high-hat ())
(defun jazz-high-hat (time)
  (p time 42 (rcosr 25 3 1/2) 1/3 1)
  (aat (+ time 2) #'jazz-high-hat it))
(jazz-high-hat (quant 4))
|#


(fp 1 104)
(fg 1.0)

(fp 1 49)

(defbeat hithat 1 "x-x-"
  (p time 42 (rcosr 40 3 1/2) 1/3 1))
(hithat)
(defun %hithat ())

(fp 0 13)
(let ((notes   (make-weighting
                '((0 .25)
                  40
                  35)))
      (rhythms (make-cycle '(2/3 4/3)))
      (amps    (make-weighting '(51 (60 .1)))))
  (defun jazz-drums (time)   
    (let ((rhythm (next rhythms)))
      (p time (next notes) (next amps) rhythm 0)
      (aat (+ time rhythm) #'jazz-drums
           it))))

(defun jazz-drums ())
(jazz-drums (quant 4))

;; wt
;; is weight of resting relative to playing
;; return weighting pattern that slightly prefers

;; playing a ride 1 pattern - 51
;;    over a ride 2 pattern - 59
(defun or12r (wt)
  (make-weighting
   (list
    (list (make-weighting `(51 (0 ,wt)) 1) 1.5)
    (make-weighting `(59 (0 ,wt)) 1))
   2))

;; (defun lamps (amps)
;;   (mapcar
;;    #'round
;;    (mapcar
;;     (lambda (x) (cm:interp x 0.0 30 1.0 80))
;;     (cm:amplitude amps))))

; Triplet 8th: 1  2  3    4  5  6    7  8  9   10 11 12
; Cymbals:     1  -  x    1  -  1    1  x  x    1  x  1 
(fp 4 9)

(fg 2.0)

(let ((notes (make-cycle
              (list
               51 0 (or12r 5)
               51 0 51
               51 (or12r 7) (or12r 7)
               51 (or12r 3) 51)))
      (amps (make-cycle '(:mf :mp :fff :f
                          :mp :ffff :mf :mp
                          :fff :f :mp :ffff))))
  (defun jazz-cymbals (time rhythm)
    (p time (next notes)
       (round (* 60 (cm:amplitude (next amps))))
       rhythm 4)
    (aat (+ time rhythm) #'jazz-cymbals
         it rhythm)))

(jazz-cymbals (quant 4) (cm:rhythm 't8 60))
(defun jazz-cymbals ())

;;;
;; <3
;;;
(fp 11 20)
(let ((notes (make-weighting
              (list
               (list (make-heap *jazz-scale*
                                (make-weighting '(1 2 3 4)))
                     :weight (make-weighting '(1.5 1.65)))
               0)))
      (amps (make-weighting (list (make-cycle '(.5 .4 .7))
                                  (make-cycle '(.6 .5 .8)))))
      (rhythms (make-weighting (list (list (make-cycle '(2/3 1/3) 12) .4)
                                     (list (make-cycle '(1/3) 8) .6)))))
  (defun jazz-piano (time &optional root)
    (and (odds .4)
         (setf root (pickl *jazz-changes*)))
    (let ((rhythm (next rhythms)))
      (p
       time
       (cm:keynum (transpose (next notes) root))
       (round (* 50 (next amps)))
       rhythm
       11)
      (aat (+ time rhythm)
           #'jazz-piano
           it
           root))))

(fp 11 99)
(fg 4.0)
(jazz-piano    (quant 4))
(defun jazz-piano ())


(jazz-high-hat (funcall *metro* (funcall *metro* 'get-beat 4)) )
(jazz-cymbals  (funcall *metro* (funcall *metro* 'get-beat 2)) (cm:rhythm 't8 60))
(jazz-piano    (funcall *metro* (funcall *metro* 'get-beat 4)) )

(flush-pending)


(let* ((tonics (make-weighting (nths *jazz-scale* '(0 2 4 6 7))))
       (colors (make-weighting (nths *jazz-scale* '(1 3 5 6 8))))
       (amps   (make-cycle '(.5 .4 1.0 .9 .4 .9 .5 .4 1.0 .9 .5 .9)))
       (durs   (make-cycle '(2/3 1/3 1/3)))
       ;; beat map. t is tonic, c is color, r is rest
       (bmap   (make-cycle (list
                            ;; 5 possible patters for triplets 1-4
                            (make-weighting (list (rancyc '(:t r r :c) 1.0)
                                                  (rancyc '(:t r r r) .25)
                                                  (rancyc '(:t r :t :c) .22)
                                                  (rancyc '(:t :c :t :c) .065)
                                                  (rancyc '(:t :c :t r) .014))
                                            1)
                            ;; 5 possible patterns for 5-7
                            (make-weighting (list (rancyc '(r r :t) 1.0)
                                                  (rancyc '(r r r) .25)
                                                  (rancyc '(r :c :t) .22)
                                                  (rancyc '(:t :c :t) .038)
                                                  (rancyc '(:t :c r) .007))
                                            1)
                            ;; 5 possible patterns for 8-10
                            (make-weighting (list (rancyc '(r r :c) 1.0)
                                                  (rancyc '(r :t :c) .415)
                                                  (rancyc '(r r r) .25)
                                                  (rancyc '(:c :t :c) .11)
                                                  (rancyc '(:c :t r) .018))
                                            1)
                            ;; 2 possible values for 11
                            (make-weighting '((r :weight 1) (:t :weight .25)) 1)
                            ;; 2 possible values for 12
                            (make-weighting '((r :weight 1) (:c :weight .25)) 1)))))
  (defun jazz-bass (time)
    (let ((x (next bmap))
          (d (next durs))
          (a (next amps))
          (k nil))
      (case x
        (:t (setf k (next tonics)))
        (:c (setf k (next colors))))
      (when (numberp k)
        (p time (transpose k 60)
           (round (* 30 a))           
           d 10))
      (aat (+ time 1/3) #'jazz-bass it))))

(fp 10 99)

(jazz-bass (quant 4))
(defun jazz-bass ())
