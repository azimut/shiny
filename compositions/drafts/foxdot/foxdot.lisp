(in-package #:shiny)
(defun f ())
(let ((c (make-cycle '(0 -1 -2 -3))))
  (defun f (time)
    (let ((n (next c)))
      (meniere:dsp-pulse
       (midihz (pc-relative 60 n (scale 0 'minor)))
       .1 1 .25 600))
    (aat (+ time #[1 b]) #'f it)))
(f (now))

;;--------------------------------------------------
(bbuffer-load
 "/home/sendai/projects/FoxDot/FoxDot/snd/x/lower/0_kick_drum.wav"
 'x)
(bbuffer-load
 "/home/sendai/projects/FoxDot/FoxDot/snd/v/lower/0_low_bass.wav"
 'v)
(bbuffer-load
 "/home/sendai/projects/FoxDot/FoxDot/snd/o/lower/0_snare_drum.wav"
 'o)
;; d1 >> play("(x )( x)o{ vx[xx]}", crush=16, rate=.8)
;; .every([24,5,3], "stutter", 4, dur=3)
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
(bbuffer-load
 "/home/sendai/projects/FoxDot/FoxDot/snd/s/lower/0_shaker.wav"
 's)
(bbuffer-load
 "/home/sendai/projects/FoxDot/FoxDot/snd/_/hyphen/0_hihat_closed.wav"
 '-)
(bbuffer-load
 "/home/sendai/projects/FoxDot/FoxDot/snd/_/tilde/0_ride.wav"
 '~)
(bbuffer-load
 "/home/sendai/projects/FoxDot/FoxDot/snd/_/asterix/0_clap.wav"
 '*)

;;d2 >> play("<-s>< ~*~>").every(30.5, "jump", cycle=32)
;; hot to jump?
(let ((v1 (make-cycle '(- s)))
      (v2 (make-cycle '(NIL ~ * ~))))
  (defun d (time &optional (beat 0))
    (when-let ((n (next v1)))
      (bbplay n :amp .1))
    (when-let ((n (next v2)))
      (bbplay n :amp .1))
    (aat (+ time #[.2 b]) #'d it (+ beat .2))))

(meniere:dsp-)
(defun d ())
(aat (tempo-sync #[1 b]) #'d it)
