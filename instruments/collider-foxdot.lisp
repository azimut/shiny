(in-package :somecepl)

(named-readtables:in-readtable :sc)

;;         (filter (b-hipass.ar osc 200))
;;                (vib 0)
(defsynth ambi ((amp 1) (sus 1) (pan 0) (freq 1)
                (fmod 0) (rate 0) (bus 0))
  (let* (;;(freq (+ fmod (in.kr bus 1)))
         (freq [freq (+ fmod freq)])
         (freq (+ freq (* (x-line.kr (* freq rate) (+ freq (lf-noise2.ar 4 1 sus)))
                          (sin-osc.ar (+ freq (lf-noise2.ar 4 2)))) ))
         (osc  (sin-osc.ar (+ freq (rand.ir .99 1.01)) (rand.ir 0 2)))
         (env  (env-gen.ar (env [0 (* amp .5) (* amp .5) 0]
                                [(/ sus 4) sus (/ sus 4)])
                           :act :free))
         (osc (* osc env .25))
         (osc (pan2.ar osc pan)))
    (out.ar bus osc)))


(defparameter *some* (synth 'ambi :freq 230 :sus 2 :amp .3))
(free *some*)

(defsynth soprano ((amp 1) (sus 1) (pan 0) (freq 0)
                   (vib 0) (fmod 0) (rate 0) (bus 0))
  (let* ((freq [freq (+ fmod freq) ])
         (sus (* sus 1.75))
         (amp (/ amp 2))
         (freq (vibrato.kr freq (+ 4 rate)))
         (osc (+ (sin-osc.ar (* 3 freq) 0 amp)
                 (sin-osc-fb.ar (* 3 freq) 0 (/ amp 2))))
         (env (env-gen.ar (env [0 amp 0] [ (/ sus 2) (/ sus 2) ]) :act :free))
         (osc (* osc env))
         (osc (* .5 (mix osc)))
         (osc (pan2.ar osc pan)))
    (out.ar bus osc)))

(defparameter *some* (synth 'soprano :freq 430 :sus 2 :fmod .1 :amp .5 :rate 0))
(free *some*)



