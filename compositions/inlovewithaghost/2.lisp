(in-package :somecepl)

;; Trying the ideas on the song
;; https://www.youtube.com/watch?v=Ql_dEdMEjl4
;; https://musescore.com/skyfox125/scores/3871256

(freverb-toggle 1)
(freverb :roomsize .6d0 :damp .1d0 :level .9d0 :width 10d0)
(fp 10 80 8) ;; sine 80 - 8
(fp 2 101 0)

(let ((o (new palindrome :of '(.75 .6 .5 .45) :elide t)))
  (defun f (time)
     (let ((offset (next o))
           (mychord (make-chord 55 70 4 *phrygian*)))
       (pa time mychord offset '(60 60 55 50) 10  (+ offset .1))
;;       (p (+ time (* 2 offset)) mychord (rcosr 35 5 1/2) offset 3)
;;       (p (+ time (* 3 offset)) (first mychord) 40 (* offset 2) 2)
;;       (aat (+ time (* 4 offset)) #'f it)
       )))

(f (quant 4))

(fp 11 116 8)


(let ((o (new cycle :of '(1 3))))
  (defun f (time)
    (let ((mychord (make-chord-5 55 70 *phrygian*)))
      (pa time mychord '(0 1) '(60 50) 1 '(1 3))
      (pa (+ time (random-elt #(1 2 0))) (cdr mychord) .5 60 2 .5)
      ;; (pa time (mapcar (lambda (x) (+ 12 x)) (if (odds .6)
      ;;                                       (reverse (cadr mychord))
      ;;                                       (cadr mychord)))
      ;;     .5 60 3 .5)
;;      (aat (+ time 4) #'f it)
      )))

(freverb-toggle 0)
(f (quant 4))

(pa (quant 4) '(60 62 64) .2 60 1 .2)

(fg .9)
(f (quant 4))

(fg .3)
(fp 1 52 0)
(fp 1 0)
(fp 3 49)

(fpitch 1 1000)

;; normal
(freverb :roomsize .2d0)


;; eerie
(freverb :roomsize .6d0 :damp .1d0 :level .9d0 :width 5d0)

(freset)

(pa (now) '(60 62 64) .5 60 0)

(off-with-the-notes)



;; THIS SYNTX SUCKS!!!
(pa (quant 4) (repeat 4 '(60))
    (mapcar #'rhythm `(1/4 2/4 ,(+ 2/4 1/8) 3/4))
    60
    11
    (mapcar #'rhythm `(1/4 2/4 ,(+ 2/4 1/8) 3/4)))

(defmacro drumthis (time note beats velocity channel)
  (alexandria:with-gensyms (lbeats nbeats)
    `(let ((,lbeats (length ,beats))
           (,nbeats (mapcar #'rhythm ,beats)))
       (pa ,time (repeat ,lbeats (list ,note))
           ,nbeats ,velocity ,channel ,nbeats))))

;; ?
(fp 7 115)
(drumthis (quant 4) 60 '(1/4 2/4 5/8 3/4) 20 7)
