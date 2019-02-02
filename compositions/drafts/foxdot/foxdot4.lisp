(in-package #:shiny)
;; Scale.default="minor"
;; Clock.bpm=140
(setf (bpm *tempo*) 140f0)
;; c1 >> play("@", dur=1/4,
;;                 sample=P[:8].rotate([0,1,3]),
;;                 rate=4,
;;                 pan=-0.5)
(fx-load "_/at/gb_noise (1).wav" '@)
(fx-load "_/at/gb_noise (2).wav" '@)
(fx-load "_/at/gb_noise (3).wav" '@)
(fx-load "_/at/gb_noise (4).wav" '@)
(fx-load "_/at/gb_noise (5).wav" '@)

(let ((s (make-cycle (fx-rotate (iota 8 :start 1) '(0 1 3)))))
  (defun c1 (time)
    (bbplay (fx-buf '@ (next s)) :amp .2 :rate 4 :pan .25)
    (aat (+ time #[1 b]) #'c1 it)))

(aat (tempo-sync #[1 b]) #'c1 it)
(defun c1 ())
;; c2 >> play("#", dur=40,
;;                 room=1,
;;                 amp=2,
;;                 pan=0.5)
(fx-load-simple "_/hash/0_crash.wav" 'h)
(defun c2 (time)
  (bbplay 'h :amp .2 :pan .75)
  (aat (+ time #[40 b]) #'c2 it))
(defun c2 ())
(aat (tempo-sync #[1 b]) #'c2 it)


;; d1 >> play("<V:><  * ><[--]>")
(fx-load-simple "v/upper/heavy 0.wav" 'V)
(fx-load-simple "_/colon/hh01.wav" '-)
(fx-load-simple "_/asterix/0_clap.wav" '*)
(fx-load-simple "_/hyphen/0_hihat_closed.wav" '_)
(let ((v1 (make-cycle '(V -)))
      (v2 (make-cycle '(nil nil * nil)))
      (v3 (make-cycle '(_))))
  (defun d11 (time)
    (bbplay (next v3) :amp .2)
    (aat (+ time #[.5 b]) #'d11 it))
  (defun d1 (time)
    (bbplay (next v1) :amp .2)
    (bbplay (next v2) :amp .2)    
    (aat (+ time #[1 b]) #'d1 it)))

(aat (tempo-sync #[1 b]) #'d1 it)
(aat (tempo-sync #[.5 b]) #'d11 it)
(defun d1 ())
(defun d11 ())
;; b1 >> dbass(dur=PDur(3,8),
;;             sus=2,
;;             chop=4,
;;             shape=PWhite(0,1/2),
;;             pan=PWhite(-1,1))
;; .sometimes("offadd", 4) + var([0,2],4)
(let* ((dur (make-cycle (pdur 3 8)))
       (pan (make-cycle '(0 127)))
       (scale (ov-scale :C5 :minor))
       (offset (make-cycle (make-var '((0 2) 4) 1)))
       (note (make-cycle
              (list (make-cycle
                     '(0)
                     (pval (between 4 12)))
                    4))))
  (defun b1 (time)
    (let ((d (next dur)))
      (p time
         (nth (mod (+ (next offset) (next note)) 7)
              scale)
         (rcosr 70 5 5) (* 2 d) 0 :pan (next pan))
      (aat (+ time #[d b]) #'b1 it))))
(fp 0 20)
(aat (tempo-sync #[1 b]) #'b1 it)
(defun b1 ())
;; p1 >> space([7,6,4,P*(2,1),0],
;;                      dur=8,
;;                      pan=(-1,1))

;;Master().hpf=var([0,4000],[76,4])

