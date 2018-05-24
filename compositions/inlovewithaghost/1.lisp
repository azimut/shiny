(in-package :somecepl)

;; Trying the ideas on the song
;; https://www.youtube.com/watch?v=Ql_dEdMEjl4
;; https://musescore.com/skyfox125/scores/3871256

(let ((o (new cycle :of '(1.5 1.25 1 1.25) :repeat 1)))
  (defun f (time)
     (let ((offset (next o))
           (mychord (make-chord-5 55 70 *phrygian*))
           (schord (make-chord 55 70 4 *phrygian*)))
       ;; sax
       (pa time (if (odds .9)
                    schord
                    (reverse schord))
           offset 40 1 offset)
       ;; goblin
;;       (pa time mychord offset (list 20 15) 2 (list offset (* 3 offset)))
       ;;; tuba
;;       (p (+ time (* 3 offset)) mychord (rcosr 16 3 1/2) offset 4)
;;       (p (+ time (* 3 offset)) (first mychord) 15 (* offset 2) 5)
       (aat (+ time (* 4 offset)) #'f it)
       )))

(f (quant 4))
(fp 4 58)

(fp 5 42)
(fg .9)
(fp 1 65)

(fg .3)
(fp 2 101 0)
(fp 1 52 0)
(fp 1 0)
(fp 3 49)
(fp 1 80 8)

(fpitch 1 1000)

;; normal

(freverb-toggle 1)
(freverb :roomsize .2d0)

(freverb-preset 6)
;; eerie
(freverb :roomsize .6d0 :damp .4d0 :level .9d0 :width 5d0)
(freverb :roomsize .6d0 :damp .4d0 :level .9d0 :width 25d0)

(freset)

(pa (now) '(60 62 64) .5 60 1 1)

(off-with-the-notes)
