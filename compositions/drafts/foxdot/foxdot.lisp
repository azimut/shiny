(in-package x#:shiny)

(setf *fx-path* "/home/sendai/.local/lib64/python3.4/site-packages/FoxDot/snd/")
(fx-clear)

(setf (bpm *tempo*) 100)
;; Clock.bpm=100; Scale.default="minor"

;;p1 >> pulse([0,-1,-2,-3], dur=8, lpf=600, lpr=0.2, crush=8) + (0,2,4,const(6))

(dsp! dsp-pulse (freq amp dur atk rel cutoff-freq lpr (dw fixnum))
  (:defaults 440 .1 1 .001 .4 1000 1 8)
  (with-samples ((i (pulse (* dw freq) amp))
                 (i (lpf i cutoff-freq lpr))
                 (i (incudine.vug:downsamp dw i))
                 (i (* i (envelope (make-perc atk rel)
                                   1 dur #'incudine:free))))
    (stereo i)))

(let ((note  (make-cycle '(0 -1 -2 -3)))
      (scale (scale 0 'minor)))
  (defun f (time)
    (let ((n (next note)))
      (dsp-pulse
       (midihz (pc-relative (note :c4) n scale))
       .1 8 .001 .9 600 .2 2)
      (dsp-pulse
       (midihz (pc-relative (note :c4) (+ 4 n) scale))
       .1 8 .1 .9 600 .2 2)
      (aat (+ time #[3.5 b])
           #'dsp-pulse
           (midihz (pc-relative (note :c4) (+ (pick 2 -2 2) n) scale))
           .1 8 .1 .9 600 .2 2))
    (aat (+ time #[8 b]) #'f it)))

(aat (tempo-sync #[1 b]) #'f it)
(defun f ())

;;--------------------------------------------------
(fx-load "x/lower/0_kick_drum.wav"  "x")
(fx-load "v/lower/0_low_bass.wav"   "v")
(fx-load "o/lower/0_snare_drum.wav" "o")

(fx-load-simple "x/lower/0_kick_drum.wav"  #\x)
(fx-load-simple "v/lower/0_low_bass.wav"   #\v)
(fx-load-simple "o/lower/0_snare_drum.wav" #\o)

;; d1 >> play("(x )( x)o{ vx[xx]}", crush=16, rate=.8)
;; .every([24,5,3], "stutter", 4, dur=3)

(let ((p1 (fx-pat "(x )( x)o{ vx[xx]}"))
      (m1 (make-metre '(24 5 3) .5)))
  (defun d1 (time)
    (let ((n (next p1)))
      ;;(if (funcall m1 time 1))
      (fx-play n :rate .8 :downsamp 16 :dur .3 :amp .2)
      ;;(fx-play (repeat 4 (ensure-list n)) :rate .8 :downsamp 16 :dur .3 :amp .2)
      )
    (aat (+ time #[.3 b]) #'d1 it)))

(let ((p1 (fx-pat "(x )( x)o{ vxX}"))
      (m1 (make-metre '(24 5 3) .5)))
  (defun d1 (time)
    (if (funcall m1 time 1)
        (let ((n (next p1)))
          (bbplay n :lpf 400 :amp .1 :downsamp 16 :rate 2)
          (at (+ time #[.15 b]) #'bbplay n
              :lpf 400 :amp .1 :downsamp 16  :rate 2))
        (progn
          (bbplay (next p1) :amp .2 :downsamp 16 :rate .8
                            :lpf 400)))
    (aat (+ time #[.3 b]) #'d1 it)))

(aat (tempo-sync #[.2 b]) #'d1 it)
(defun d1 ())

(let ((dur (make-cycle
            (list
             (make-cycle
              (make-cycles 1 '(x NIL) '(NIL x) 'o '(x xx v NIL))
              (make-cycle '(24 5 3)))
             (make-cycle
              (make-cycles 4 '(x NIL) '(NIL x) 'o '(x xx v NIL))
              3)))))
  (defun g (time &optional (beat 0))
    (when-let ((n (print (next dur))))
      (case n
        (x (bbplay n :amp .3 :downsamp 16 :rate .8))
        (v (bbplay n :amp .3 :downsamp 16 :rate .8))
        (xx (progn
              (at (+ time #[.1 b])
                  #'bbplay 'x :amp .3 :rate 2 :downsamp 16 :rate .8)
              (bbplay 'x :amp .3 :rate 2 :downsamp 16 :rate .8)))
        (o  (bbplay n :amp .3 :downsamp 16 :rate .8))))
    (aat (+ time #[.2 b]) #'g it (+ beat .2))))
(aat (tempo-sync #[1 b]) #'g it)
(defun g ())
;;--------------------------------------------------

(fx-load "_/hyphen/0_hihat_closed.wav" "-")
(fx-load "s/lower/0_shaker.wav"        "s")
(fx-load "_/tilde/0_ride.wav"          "~")
(fx-load "_/asterix/0_clap.wav"        "*")

;;d2 >> play("<-s>< ~*~>").every(30.5, "jump", cycle=32)
;; hot to jump?
(let ((tempo (make-metro 220)))
  (destructuring-bind (p1 p2)
      (fx-pat "<-s>< ~*~>")
    (defun d (time)
      (when (funcall tempo 'at-beat 30)
        (next p1 2) (next p2 2))
      (fx-play (next p1) :amp .2 :lpf 500)
      (fx-play (next p2) :amp .1 :lpf 500)
      (aat (+ time #[.3 b]) #'d it))))

(defun d ())
(aat (tempo-sync #[1 b]) #'d it)
