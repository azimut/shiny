(in-package #:shiny)
(setf *fx-path* "/home/sendai/.local/lib64/python3.4/site-packages/FoxDot/snd/")
(fx-clear)
;; Scale.default="minor"
;; Clock.bpm=140
(setf (bpm *tempo*) 140f0)

;; c1 >> play("@", dur=1/4,
;;                 sample=P[:8].rotate([0,1,3]),
;;                 rate=4,
;;                 pan=-0.5)
(fx-load "_/at/" #\@)
(let ((s (make-cycle (fx-rotate (range 0 7) '(0 1 3)))))
  (defun c1 (time)
    (fx-play "@" :sample (next s) :amp .2 :rate 4 :pan .25)
    (aat (+ time #[.25 b]) #'c1 it)))
(aat (tempo-sync #[.25 b]) #'c1 it)
(defun c1 ())

;; c2 >> play("#", dur=40,
;;                 room=1,
;;                 amp=2,
;;                 pan=0.5)
(fx-load "_/hash/0_crash.wav" #\#)
(defun c2 (time)
  (fx-play "#" :amp .2 :pan .75)
  (aat (+ time #[40 b]) #'c2 it))
(aat (tempo-sync #[40 b]) #'c2 it)
(defun c2 ())

;; d1 >> play("<V:><  * ><[--]>")
(fx-load "v/upper/heavy 0.wav"         #\V)
(fx-load "_/colon/hh01.wav"            #\:)
(fx-load "_/asterix/0_clap.wav"        #\*)
(fx-load "_/hyphen/0_hihat_closed.wav" #\-)

(destructuring-bind (p1 p2 p3)
    (fx-pat "<V:><  * ><[--]>")
  (defun d1 (time)
    (fx-play (next p1) :amp .2)
    (fx-play (next p2) :amp .2 :lpf 500)
    (fx-play (next p3) :amp .2)
    (aat (+ time #[1 b]) #'d1 it)))
(aat (tempo-sync #[1 b])  #'d1 it)
(defun d1 ())
;; b1 >> dbass(dur=PDur(3,8),
;;             sus=2,
;;             chop=4,
;;             shape=PWhite(0,1/2),
;;             pan=PWhite(-1,1))
;; .sometimes("offadd", 4) + var([0,2],4)
;; offadd, adds a new voice 4 sem "sometimes" in this case
;; var() in this case is used to add the 0 or 2 each 4 beats to both 2 or single note
(let* ((dur    (make-cycle (pdur 3 8)))
       (pan    (make-weighting '(0 127)))
       (pan    (make-cycle '(.01 .99)))
       (offset (make-cycle (make-var '((0 2) 4) 1)))
       (va1r    (var '(0 2) 4))
       (note   (make-cycle
                (list (make-cycle
                       '(0)
                       (pval (between 4 12)))
                      4))))
  (defun b1 (time)
    (let ((d (next dur)))
      ;;(fpan 0 (next pan))
      (csound-chnset (next pan) "VoxHumana.pan")
      (and (< 4 (mod (round (/ (node-uptime 0) *1beat*)) 16) 16)
           (at (+ #[(/ d 2) b] time)
               #'clc 23 (pc-relative (note :c4)
                                     (+ 4 (next var))
                                     (scale 0 'minor))
               (rcosr 70 5 5) (* 2 d)))
      ;;#+nil
      (clc 23 (pc-relative (note :c3)
                           (next var)
                           (scale 0 'minor))
           (rcosr 70 5 5) (* 2 d))
      (aat (+ time #[d b]) #'b1 it))))

(fp 0 1)
(fg 2f0)
(aat (tempo-sync #[1 b]) #'b1 it)
(defun b1 ())
;; p1 >> space([7,6,4,P*(2,1),0],
;;                      dur=8,
;;                      pan=(-1,1))

;;Master().hpf=var([0,4000],[76,4])

