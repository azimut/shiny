(in-package :shiny)

(fp 2 40)
(fp 0 20)
(fg 2f0)
(fp 1 40)
;; A - 1010 - up/back/off
;; 8 - 1000 - up/back
;; 2 - 0010 - off
;;
;; C - 1100 - up/back/weak-sincopated
;; 1 - 0001 - weak-syncopated
;;
;; B - 1011 - up/back/weak-sync
;; 4 - 0100 - weak-sync
;; 9 - 1001 - up/weak-sync
(at (tempo-sync #[(* .3 16) b])
    #'eval
    (defpattern h
        (((parse-pattern (bjorklund 3 12) :true-char #\1)
          (parse-pattern (bjorklund 2 12) :true-char #\1)
          (parse-pattern (bjorklund 4 12) :true-char #\1)
          (parse-pattern (bjorklund 1 12) :true-char #\1))
         .3)
      (p time 40 40 .2 0)
      (p time 52 30 .1 0)
      (p time 60 40 .2 0)
      (p time (pc-quantize (drunk 70 5) (scale 0 'minor)) 40 .3 2)))


(fp 2 52)
(aat (tempo-sync #[ (* 16 .3) b]) #'h it)
(defun h ())

(defun f (time)
  (when (hbeat time "cc8a")
    (p time (nth 0 (ov-scale :c3 :minor)) 30 .1 0))
  (when (hbeat time "cc8a")
    (p time (nth 0 (ov-scale :c4 :minor)) 30 .1 0))
  (when (hbeat time "ffff")
    (p time (nth (pick 0 4 7 11) (ov-scale :c5 :minor)) 30 .1 0))
  (when (hbeat time "ffff")
    (p time (pc-relative (note :c2)
                         (+ (* (mod time 3) 7)
                            (* (mod time 2) 2)
                            (* (mod time 5) 4)
                            (* (mod time 7) 3))
                         (scale 0 'minor))
       30 .1 2))
  (when (hbeat time "ffff")
    (p time (pc-relative (note :c3)
                         (+ 4
                            (* (mod time 3) 7)
                            (* (mod time 2) 2)
                            (* (mod time 5) 4)
                            (* (mod time 7) 3))
                         (scale 0 'minor))
       30 .1 1))
  (aat (+ time #[1 b]) #'f it))

(setf (bpm *tempo*) 120)

(aat (tempo-sync #[1 b]) #'f it)
(defun f ())


;;--------------------------------------------------
;; Cover of Steven Yi:
;; csound-live-code/practice/2019-05-13-rhythmic.orc
;;
(let ((p1 (hexpat "f"))
      (p2 (hexpat "cfab cbaf bdbe dc"))
      (p3 (hexpat "cfab cbaf b"))
      (p4 (hexpat "0c"))
      (p5 (hexpat "000f")))
  (defun p1 (time)
    (when (next p1)
      (p time (pc-relative (note :c2)
                           (nth-beat 2 '(0 4 7 11 14 18 21 18 14 11))
                           (scale 0 'minor))
         30 .8 1))
    (when (next p2)
      (p time (pc-relative (note :c2)
                           (+ 4 (nth-beat 1.93 '(0 4 7 11 14 18 21 18 14 11)))
                           (scale 0 'minor))
         (+ 30 (- (random 10))) .8 1))
    (when (next p3)
      (p time (pc-relative (note :c2)
                           (+ 7 (nth-beat 1.7 '(0 4 7 11 14 18 21 18 14 11)))
                           (scale 0 'minor))
         (+ 30 (- (random 10))) .1 5))
    (when (next p4)
      (p time (pc-relative (note :c4) (nth-beat 1.7 '(12 6))
                           (scale 0 'minor))
         30 .8 5))
    (when (next p5)
      (p time (pc-relative (note :c6)
                           (nth-beat 2.7 '(0 2 3 4))
                           (scale 0 'minor))
         (+ 30 -10) .8 5))
    (aat (+ time #[.8 b]) #'p1 it)))

(fp 5 52)
(fp 1 11)

(aat (tempo-sync #[1 b]) #'p1 it)
(defun p1 ())


;; /** Non-interpolating oscillator. Given phase in range 0-1,
;; returns value within the give k-array table. */
;; opcode xosc, i, ik[]
;;   iphase, kvals[]  xin
;;   indx = int(lenarray:i(kvals) * (iphase % 1))
;;   xout i(kvals, indx)
;; endop
;; (round (* 4
;;           (/ (mod (/ (node-uptime 0) *1beat*) (* 2 4)) (* 2 4))))



;; /** Given period in beats, return current phase of global
;; clock in range [0-1) */
;; opcode phsb, i, i
;;   ibeats xin
;;   iticks = ibeats * 4
;;   xout (i(gk_clock_tick) % iticks) / iticks
;; endop
;;
;; ???
;; (/ (mod (/ (node-uptime 0) *1beat*)
;;         (* 2 4))
;;    (* 2 4))

;; xosc(phsb(2), array(0,4,7,11, 14, 18, 21, 18, 14, 11))
