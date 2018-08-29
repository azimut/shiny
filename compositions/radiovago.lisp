(in-package :shiny)

;; Radio vago the sinking (trying to get the sounds)

#|
SynthDef(”softfrog_”++x, {
         arg out, freq=440, prate=180, pwidth=0.5,
                  sustain=0.3, amp=0.1;
         var env, u;
         env = Env.linen(Rand(0.001, 0.003), Rand(0.1, 0.3), 0.01);
         freq = freq + LFNoise2.kr(5, 10);
         u = SinOsc.ar(
                       freq,
                       LFPulse.perform(x,
                                       prate,
                                       0,
                                       Line.kr(pwidth, 0, sustain)
                                       ),
                       amp
                       );
         u = BRF.ar(u, freq, 0.1) * EnvGen.kr(env, doneAction:2);
         Out.ar(out, Pan2.ar(u, Ra nd(-1,1)))})
|#

(defsynth nice ((out 0) (freq 440) (prate 180) (pwidth .5) (sustain .3) (amp .1))
    (let* ((env (linen (rand.ir .001 .003)
                       (rand.ir .1 .3)
                       .01))
           (freq (+ freq (lf-noise2.kr 5 10)))
           (u (sin-osc.ar freq
                          (lf-pulse.kr prate 0 (line.kr pwidth 0 sustain))
                          amp))
           (u (* (env-gen.kr env :act :free)
                 (brf.ar u freq .1))))
      (out.ar out (pan2.ar u (rand.ir -1 1)))))

(defparameter *some* (synth 'nice :amp .1))

(defun f (time sca)
  (let ((mc (make-chord 50 70 3 *phrygian*)))
    (synth 'nice :freq (midicps (first mc)) :prate 170 :pwidth .2)
    (at (+ time .25)
      (synth 'nice :freq (midicps (second mc)) :prate 170 :pwidth .2))
    (at (+ time 5)
      (synth 'nice :freq (midicps (third mc)) :prate 170 :pwidth .2)))
;;  (aat (+ time 1) #'f it sca)
  )

(rlpfd.ar )

(f (quant 4) (ov-scale :c4 :major))

;;----------------------------------------------------------------------
