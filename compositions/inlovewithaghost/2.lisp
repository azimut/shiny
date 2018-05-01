(in-package :somecepl)

;; Trying the ideas on the song
;; https://www.youtube.com/watch?v=Ql_dEdMEjl4
;; https://musescore.com/skyfox125/scores/3871256

(let ((o (new palindrome :of '(.75 .6 .5 .45) :elide t :repeat 1)))
  (defun f (time)
     (let ((offset (next o))
           (mychord (make-chord 55 70 4 *phrygian*)))
       (pa time mychord offset '(60 60 55 50) 1)
;;       (p (+ time (* 2 offset)) mychord (rcosr 35 5 1/2) offset 3)
;;       (p (+ time (* 3 offset)) (first mychord) 40 (* offset 2) 2)
       (aat (+ time (* 4 offset)) #'f it)
       )))

(let ((o (new cycle :of '(1 3))))
  (defun f (time)
    (let ((mychord (make-chord-waltz1 55 70 *phrygian* 0 5)))
      (pa time mychord 60 1 1))))


(fg .9)
(f (quant 4))

(fg .3)
(fp 2 101 0)
(fp 1 52 0)
(fp 1 0)
(fp 3 49)
(fp 1 80 8) ;; sine 80 - 8

(fpitch 1 1000)

;; normal
(freverb :roomsize .2d0)

;; eerie
(freverb :roomsize .6d0 :damp .1d0 :level .9d0 :width 5d0)

(freset)

(pa (now) '(60 62 64) .5 60 0)

(off-with-the-notes)
