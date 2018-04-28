(in-package :somecepl)

;; Trying the ideas on the song
;; https://www.youtube.com/watch?v=Ql_dEdMEjl4
;; https://musescore.com/skyfox125/scores/3871256

(setf (bpm *tempo*) 90)

(let ((o (new cycle :of '(1.5 1.25 1))))
  (defun f (time)
     (let ((offset (next o))
          (mychord (make-chord 55 70 3 *phrygian*)))
     (pa time mychord offset 60 1)
     (p (+ time #[(* 3 offset) b]) (car mychord) 60 2 2)
     (aat (+ time #[(* 4 offset) b])
          #'f it))))

(f (now))

(fg .2)
(fp 2 101 0)
(fp 1 0)
(fp 3 96)
(fp 1 80 8)

;; normal
(freverb :roomsize .2d0)

;; eerie
(freverb :roomsize .9d0 :damp .4d0 :level .9d0 :width 5d0)

(freset)

(flush-pending)
