(in-package :shiny)
;;--------------------------------------------------
;; Cover of Steven Yi:
;; csound-live-code/practice/2019-05-13-rhythmic.orc
;;
;; Tempo: 120

(progn
  (setf (bpm *tempo*) 120)
  (update-1beat))

(flush-pending)
(off-with-the-notes)

(fg 2f0)

(progn (fp 7 89)
       (fp 1 0)
       (fp 2 70)
       (fp 5 52)
       (fp 6 52)
       (fp 8 52)
       (fpan 8 100)
       (fpan 6 20))

(aat (tempo-sync #[1 b]) #'p1 it)
(defun p1 ())

;; <3
(let ((p1 (hexpat "f"))
      (p2 (hexpat "cfab cbaf bdbe dc"))
      (p3 (hexpat "cfab cbaf b"))
      (p4 (hexpat "0c"))
      (p5 (hexpat "000f"))
      (scale (scale 0 'aeolian)))
  (defun p1 (time)
    (when (next p1)
      (p time (pc-relative (note :c2)
                           (nth-beat 2         '(0 4 7 11 14 18 21 18 14 11))
                           scale)
         40 1 1))
    (when (next p2)
      (p time (pc-relative (note :c2)
                           (+ 4 (nth-beat 1.93 '(0 4 7 11 14 18 21 18 14 11)))
                           scale)
         (* (choose .7) (rcosr 45 5 10)) .8 2))
    ;;
    (when (next p3)
      (p time (pc-relative (note :c2)
                           (+ 7 (nth-beat 1.7  '(0 4 7 11 14 18 21 18 14 11)))
                           scale)
         (* (choose .7) (rsinr 45 5 10)) .1 5))
    (when (next p4)
      (p time (pc-relative (note :c4) (nth-beat 1.7 '(12 6))
                           scale)
         (rcosr 45 5 10) .8 6))
    (when (next p5)
      (p time (pc-relative (note :c6)
                           (nth-beat 2.7 '(0 2 3 4))
                           scale)
         30 .8 8))
    (aat (+ time #[.8 b]) #'p1 it)))

