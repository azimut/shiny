(in-package :somecepl)

;; 1 - Consistency a.k.a. Patterns
;; "The more patterns the more boring it gets..."
;; The set of notes is already a pattern, but the heap makes
;;   it less predictable by choosing elements not twice in a cycle.
;; Alternatively you can add some repetition (stuttering) by creating a cycle
;;   and provide it to the :for of the pattern with a stutter every N.
;; "Still, the more repetitions the melody make the  more stronger.
;;   The ear likes repetition"

(let ((h (make-heap (make-chord 50 70 5 (scale 0 'ryukyu))
                    (make-cycle '(1 1 1 1 1 2)))))
   (defun h (time)
     (p time (next h) 60 1 0)
     (aat (+ time 1) #'h it)))
(defun h ())
(h (quant 4))

;; 2 - Flow - Direction/Momentum
;; "Give the melody landing points"
(fp 0 79)
(fp 1 79)
(fp 2 79)
(fp 3 79)

(fp 4 79)

(let ((r (make-cycle '(1 1 1 1 1 2)))
      (h (make-heap (make-chord 50 70 5 (scale 0 'ryukyu))
                    (make-cycle '(1 1 1 1 1 2)))))
   (defun h (time)
     (let ((rr (next r)))
       (p time (next h) 60 rr 0)
       (aat (+ time rr) #'h it))))
(defun h ())
(h (quant 4))

;; 3 - interesting shape
;; "not random"

;; we use zmodt to play something here and there, the interesting result
;;   here is that sometimes it plays 2 notes instead of 1 due this is scheduled
;;   might be twice per "beat"
(fp 17 5)
(fp 18 0)

;; <3
(let ((r (make-cycle '(1 1 1 1 1 2)))
      (h (make-heap (make-chord 50 70 5 (scale 0 'ryukyu))
                    (make-cycle '(1 1 1 1 1 2 1 1 3))))
      (b (make-weighting '(3 (4 :weight .5) 2))))
   (defun h (time)
     (let ((rr (* (pick .25 .5 .5 .5 .5 .5) (next r)))
           (n  (next h)))
       (when (zmodt 4) (p time n (drunk 45 5) 4 (+ 5 (random 10))))
       ;;;;;       (when (zmodt 8) (p (+ .25 time) (+ 12 n) 65 (pick .5 1 2 .5) 17))
       (when (= rr 1)
         (pa time (make-chord-alberti 75 85 (scale 0 'ryukyu))
             (/ rr (next b))
             '(40 40 45 40) '(20 21 22 24) rr))
       ;;(p time n (rcosr 65 5 10) (+ (pick .6 .8 1 2 3) rr) (random 5))       
       (aat (+ time rr) #'h it))))

(freverb-toggle 1)
(freverb-preset 1)

(fg 1.0)

(freverb :roomsize 0.6d0 :damp 0.1d0 :width 0.9d0 :level .8d0)

(defun h ())
(h (quant 4))

(let ((r (make-cycle '(1 1 1 1 1 2))))
   (defun h (time)
     (let ((rr (* (pick .25 .5 .5 .5 .5 .5) (next r))))
       (p time
          ;;(qtanr (scale 0 'ryukyu) 60 10 10)
          (qcosr (scale 0 'ryukyu) 60 10 10)
          (rcosr 60 5 5) rr 0)
       (aat (+ time rr) #'h it))))

;; 4 - catch |  low/hig
;; "we follow what's different"
;; "it's all about contrast" ... like have a pattern and add something

(make-cycle )
