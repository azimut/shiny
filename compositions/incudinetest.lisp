(in-package :somecepl)

(enable-sharp-t-syntax)

(dsp! vuglet-test (freq amp)
  (vuglet ((osc (freq amp)
                (* amp (sin (* (phasor freq 0)
                               +twopi+))))
           (fm (fcar fmod amp (k fixnum))
               (osc (+ fcar (osc fmod (* fcar (/ k)))) amp))
           (lp (in)
               (* 0.5 (+ in (delay1 in))))
           (lp4 (in) #4t(lp in)))
    (with-samples ((f1 freq)
                   (f2 (- 20000 f1))
                   (g (* amp .5))
                   (this (+ (lp (fm f1 111 g 8))
                            (lp4 (fm f2 70 g 18)))))
      (out this this))))

(vuglet-test 100 .2 :id 2)

(set-control 2 :freq (if (odds .2) 110 220))
(incudine:free (node 0))



(dsp! highest-note-test ((chan fixnum) a d s r)
  "Highest priority monosynth (single trigger)."
  (with ((keynum (incudine.vug:midi-highest-keynum chan)))
    (with-samples ((glide (incudine.vug:exp-midi-cc chan 1 0.01 1.2))
                   (amp (incudine.vug:midi-amp
                         incudine.vug:*linear-midi-table*
                         chan keynum))
                   (amp-prev +sample-zero+)
                   (gate +sample-zero+))
      (if (zerop amp)
          (setf gate +sample-zero+ amp amp-prev)  ; Note off.
          (setf gate (sample 1) amp-prev amp))
      (stereo (* (envelope (make-adsr a d s r) gate 1 #'identity)
                 (incudine.vug:gbuzz (incudine.vug:lag (incudine.vug:midi-cps *default-tuning* keynum) glide)
                        (incudine.vug:lag amp   0.02) 20 1 .6))))))
