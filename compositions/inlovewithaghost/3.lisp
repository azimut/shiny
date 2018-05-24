(in-package :somecepl)

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

(let ((o (new palindrome :of '(.75 .6 .5 .45) :elide t)))
  (defun f (time)
     (let ((offset (next o))
           (mychord (make-chord 55 70 4 *phrygian*)))
       (pa time mychord offset '(60 60 55 50) 10  (+ offset .1))
       (p (+ time (* 2 offset)) mychord (rcosr 35 5 1/2) offset 3)
       (p (+ time (* 3 offset)) (first mychord) 40 (* offset 2) 2)
;;       (aat (+ time (* 4 offset)) #'f it)
       )))

(f (quant 4))

(fp 11 116 8)

(fp 1 0)
(fp 2 0)
(fp 3 0)
(fp 4 0)

(fp 5 69)

(defun pick-random-list (list)
  (let* ((l (length list))
         (n (- l (random l))))
    (loop :for x :in list :collect x :repeat n)))

(defun f ())
(defun f (time chan)
  (let* ((c (make-chord 65 80 5 *phrygian*))
         (s (second c))
         (ss (make-chord-fixed s 3 *phrygian*))
         )
    (pa time (reverse c) .2 60 chan '(.2 .2 .2 .2 1))
    (p (+ time 1.2) ss 60 1.5 (1+ chan)))
  (aat (+ time 2.5) #'f it chan))

(f (quant 4) 1)
(f (quant 4) 3)

;; <3
(defun ff ())
(defun ff (time)
  (let* ((mychord (make-chord 55 70 4 *phrygian*))
         (r       (reverse mychord))
         (rr      (append (list (pc-relative (first r) -1 *phrygian*))
                          (subseq r 1 4))))
    (pa   time      r  .25 50 5 .2)
    (pa  (1+ time)  rr .25 '(65 50 50 50) 6 .25)
    (pa  (+ 2 time) r  .25 50 7 .25)
    ;; (if (odds .1)
    ;;     (p   (+ time 1) (pick-random-list mychord) (rcosr 36 5 1/2) 4 5)
    ;;     (progn
    ;;       (p   (+ time 1) (pick-random-list mychord) (rcosr 36 5 1/2) 2 5)
    ;;       (p   (+ time 3) (pick-random-list mychord) (rcosr 36 5 1/2) 2 5)))
    (pa  (+ 3 time) rr .25 '(60 50 50 50) 8 .2)
  (aat (+ time 4) #'ff it)))

(fp 1 0 1)
(fp 2 0 1)
(fp 3 0 1)
(fp 4 0 1)

(freverb-toggle 0)
(ff (quant 4))

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


