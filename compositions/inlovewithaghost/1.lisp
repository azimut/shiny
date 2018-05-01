(in-package :somecepl)

;; Trying the ideas on the song
;; https://www.youtube.com/watch?v=Ql_dEdMEjl4
;; https://musescore.com/skyfox125/scores/3871256

(let ((o (new cycle :of '(1.5 1.25 1 1.25))))
  (defun f (time)
     (let ((offset (next o))
           (mychord (make-chord-fade 55 70 *phrygian*)))
;;       (pa time mychord offset 60 1)
;;       (p (+ time (* 2 offset)) mychord (rcosr 35 5 1/2) offset 3)
;;       (p (+ time (* 3 offset)) (first mychord) 40 (* offset 2) 2)
;;       (aat (+ time (* 4 offset)) #'f it)
       )))

(fg .9)
(f (quant 4))

(fg .3)
(fp 2 101 0)
(fp 1 52 0)
(fp 1 0)
(fp 3 49)
(fp 1 80 8)

(fpitch 1 1000)

;; normal
(freverb :roomsize .2d0)

;; eerie
(freverb :roomsize .6d0 :damp .4d0 :level .9d0 :width 5d0)

(freset)

(pa (now) '(60 62 64) .5 60 0)

(off-with-the-notes)
