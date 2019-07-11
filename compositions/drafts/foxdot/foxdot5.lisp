(in-package :shiny)
;;
;; ten-lines-or-less/industrialis.py
;;
(ql:quickload :shiny/foxdot)
(setf *fx-path* "/home/sendai/.local/lib64/python3.4/site-packages/FoxDot/snd/")
(clean-buffers)
(fg 2f0)

(fx-load-simple "x/lower/0_kick_drum.wav"     "x")
(fx-load-simple "_/hyphen/0_hihat_closed.wav" "-")
(fx-load-simple "d/lower/0_wood.wav"          "d")
(fx-load-simple "v/lower/0_low_bass.wav"      "v")
(fx-load-simple "v/upper/heavy 0.wav"         "V")
(fx-load-simple "_/colon/hh01.wav"            "_")
(fx-load-simple "o/lower/1_snare_drum.wav" "o")

(setf (bpm *tempo*) 160d0)

;; d1 >> play(P["V::"][:16] & P["<v ><  |o1| >"], drive=0.1, rate=1.2)
(destructuring-bind (p1 p2 p3)
    (fx-pat "<(_v)__(Vv)><v ><  o >")
  (defun f (time)
    (bbplay (next p1) :amp .1 :rate 1.2)
    (bbplay (next p2) :amp .4 :rate 1.2)
    (bbplay (next p3) :amp .4 :rate 1.2)
    (aat (+ time #[1 b]) #'f it)))

(aat (tempo-sync #[4 b]) #'f it)
(defun f ())

;; cp >> play("* ")
(fx-load-simple "_/asterix/0_clap.wav" "*")
(let ((p1 (fx-pat "* ")))
  (defun g (time)
    (bbplay (next p1) :amp .1 :lpf 500)
    (aat (+ time #[1 b]) #'g it)))
(aat (tempo-sync #[1 b]) #'g it)
(defun g ())

;; these will work better (or at all) on synth
;; b1 >> bass(dur=1/4, formant=PRand(8)[:8], rate=PRand(5,10)[:8], pan=PWhite(-1,1))
(fp 2 0)
(let ((vel (make-line (iota 40 :start 0)))
      (m1  (make-metre '(8 4 2) .5)))
  (defun bass (time)
    (if (funcall m1 time 1)
        (progn
          (p time 72 (next vel) 1/4 2)
          (p (+ #[.125 b] time) 72 (next vel) 1/4 2))
        (progn
          (fpan 2 (pick 0 127))
          (p time 60 (next vel) 1/4 2)))
    (fpitch 2 (+ 8192 (round (* (between 5 10) (/ 8192 12)))))
    (aat (+ time #[1/4 b]) #'bass it)))
(aat (tempo-sync #[1/4 b]) #'bass it)
(defun bass ())

;; b2 >> sawbass(var([0,5,2,[3,6]],[8,6,1,1]), dur=PDur(3,8)).spread()
(let ((dur   (make-cycle (pdur 3 8)))
      (var   (var '(0 5 2 3) '(8 6 1 1)))
      (scale (ov-scale :c4 :minor)))
  (defun h (time)
    (let ((d (next dur)))
      (p time (nth (funcall var) scale) 40 d 0)
      (p time (nth (funcall var) scale) 40 d 1)
      (aat (+ time #[d b]) #'h it))))

;; Spread
(let ((pg 116))
  (fpitch 1 (+ 8192
               (round (* .125 (/ 8192 12)))))
  (fpitch 0 (+ 8192
               (round (* .125 (/ 8192 12)))))
  (fpan 0 0)
  (fpan 1 127)
  (fp 1 pg)
  (fp 0 pg))

(freset)
(aat (tempo-sync #[1 b]) #'h it)
(defun h ())

