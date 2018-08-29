(in-package :shiny)

;; Trying the ideas on the song
;; https://www.youtube.com/watch?v=Ql_dEdMEjl4
;; https://musescore.com/skyfox125/scores/3871256

(freverb-toggle 1)
(freverb :roomsize .6d0 :damp .1d0 :level .9d0 :width 10d0)
(fp 10 80 8) ;; sine 80 - 8
(fp 2 101 0)


(fp 10 14)

(fp 10 0 0)
(try-sounds (quant 4) 10 10 4)

;; !!
;; slow down and speed up chord rhythm and bar length
(let ((o (new palindrome :of '(.75 .6 .5 .45) :elide t)))
  (defun f (time)
     (let ((offset (next o))
           (mychord (make-chord 55 70 4 *phrygian*)))
       (pa time mychord offset '(60 60 55 50) 10 (list offset
                                                       offset
                                                       offset
                                                       (+ offset (pick .1 .2))))
       (p (+ time (* 2 offset)) mychord (rcosr 35 5 1/2) offset 3)
       (p (+ time (* 3 offset)) (first mychord) 40 (* offset 2) 2)
       (aat (+ time (* 4 offset)) #'f it))))

(defun f ())
(f (quant 4))

(fp 11 116 8)

(fp 1 0)
(fp 2 0)
(fp 3 0)
(fp 4 0)

(fp 5 69)


(defun f ())
(defun f (time chan)
  (let* ((c (make-chord 65 80 5 *phrygian*))
         (s (second c))
         (ss (make-chord-fixed s 3 *phrygian*))
         )
    (pa time (reverse c) .2 60 chan '(.2 .2 .2 .2 1))
    ;; a problem with a bass here is that it might not change from the previous
    ;; bass, a heap like structure would help. Also it won't matter that much if
    ;; the bass doesn't chant through the composition...i think
    (p (+ time 1.2) (transpose ss -12) 25 1.5 (1+ chan)))
  (aat (+ time 2.5) #'f it chan))

(f (quant 4) 1)
(f (quant 4) 3)

;; <3
;; alternate between:
;; - a descending arpeggio
;; - the same descending arpeggion but with the first note an interval below
;; add some "melody" by playing a random part of the chord
(defun ff ())
(defun ff (time)
  (let* ((pc (ov-pc-scale :lydian))
         (mychord (make-chord 55 70 4 pc))
         (r       (reverse mychord))
         (rr      (append (list (pc-relative (first r) -1 pc))
                          (subseq r 1 4))))
    (if (odds .5)
        (pa   time      r  .25 '(65 55 50 50) 5 .25)
        (pa   time      r  '(0 .5 .5 .5) '(65 55 50 50) 5 .5))
    (pa  (+ 1 time) rr .25 50 6 .25)
    (if (odds .5)
        (pa  (+ 2 time) r  .25 '(65 50 50 50) 7 .25)
        (pa  (+ 2 time) rr  '(0 .5 .5 .5) '(65 50 50 50) 7 .5))
    (if (odds .1)
        (p   (+ time 1) (pick-random-list mychord 3) (rcosr 45 5 1/2) 3 2)
        (progn
          (p   (+ time 1) (first mychord) (rcosr 45 5 1/2) 2 2)
          (p   (+ time 3) (pick (list (second mychord) (third mychord))
                                (first mychord))
               (rcosr 45 5 1/2) 1 2)))
    (pa  (+ 3 time) rr .25 50 8 .25)
    (aat (+ time 4) #'ff it)))

;;;;
(fg .5)
(let (;;(i (make-cycle '(i vi iii vii)))
      (i (make-cycle '(iv v iii vi)))
      ;;(i (make-cycle '(iv^7 v7 iii7 vi7)))
      )
  (defun ff (time)
    (let* (;;(pc (ov-pc-scale :lydian))
           ;;(mychord (make-chord 55 75 4 pc))
           (pc (pc-diatonic 0 'major (next i)))
           (lchord 4) ;; !
           (larp (/ 1 lchord))
           (mychord (make-chord 55 75 lchord pc))
     ;;      (mychord (mapcar (lambda (x) (pc-relative x -7 pc)) mychord))
           (r       (reverse mychord))
;;           (r (mapcar (lambda (x) (pc-relative x 2 pc)) r))
           ;;(r (mapcar (lambda (x) (pc-relative x -100 pc)) r))
           (rr      (append (list (pc-relative (first r) -1 pc))
                            (subseq r 1 lchord))))
      ;; (if (odds 1)
      ;;     (pa   time
      ;;           r
      ;;           ;;(pick r mychord)
      ;;           ;;(pick r (reverse (mapcar (lambda (x) (pc-relative x +1 pc)) mychord)))
      ;;           larp '(65 55 50 50) 5 larp)
      ;;     (pa   time      r  '(0 .5 .5 .5) '(65 55 50 50) 5 .5))
      (pa  (+ 1 time) rr larp 50 6 larp)
      ;; (if (odds 1)
      ;;     (pa  (+ 2 time)
      ;;          r
      ;;          ;;(pick r rr)
      ;;          ;;(pick r (mapcar (lambda (x) (pc-relative x +1 pc)) mychord))
      ;;          larp '(65 50 50 50) 7 larp)
      ;;     (pa  (+ 2 time) rr  '(0 .5 .5 .5) '(65 50 50 50) 7 .5))
      ;; (pa (+ time 1.5) (repeat 2 (list (+ (pick 0 12) (pickl mychord)))) .5 (pick 40 55) 11 .3)
      ;; (pa (+ time 2.5) (repeat 2 (list (+ (pick 0 12) (pickl mychord)))) .5 (pick 40 55) 11 .3)
      (if (odds 1)
          (p   (+ time 1) (pick-random-list mychord 2) (rcosr 35 5 1/2) 3 10)
          (progn
            (p   (+ time 1) (first mychord) (rcosr 40 5 1/2) 2 2)
            (p   (+ time 3) (pick (list (second mychord) (+ -12 (third mychord)))
                                  (first mychord))
                 (rcosr 40 5 1/2) 1 2)))
      (pa  (+ 3 time) rr larp 50 8 larp)
      (aat (+ time 4) #'ff it))))

;;;;;; -----------------




(fp 13 35)
(fp 10 49)
(fp 2 0)
(fp 11 6)

(defun ff ())
(ff (quant 4))

(fpress 2 0)

(fp 2 49)
(fp 10 49)

(fg 1.0)
(fp 2 52)
;;(fp 2 80)
;;(fp 5 80)

;; Majora
(loop for x from 5 upto 8 do (fp x 41))
;; Megadrive
(fp 5 12 0)
(fp 6 12 0)
(fp 7 12 0)
(fp 8 12 0)

(freverb-toggle 0)

(fp 8 44 0)

(p (quant 4) 60 60 1 8)

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


