(in-package :shiny)

;;--------------------------------------------------
;; Sonic-pi
(define-vug ss (freq mfreq amp)
  "helper for square function"
  (cond
    ((> freq 0d0) (* amp mfreq .0001))
    ((< freq 0d0) (- (* amp mfreq .0001)))
    (t (* amp freq .0001))))

(dsp! square (freq amp dur atk)
  (:defaults 110 .1 1 .25)
  (with-samples ((in (sine freq amp))
                 (in (ss in freq amp))
                 (in (* in (envelope (make-perc atk dur)
                                     1 dur #'incudine:free))))
    (stereo in)))

;;--------------------------------------------------
;; Sonic-pi
;; Missing sc (normalizer in)
(dsp! dsp-pulse (freq amp dur cutoff-freq)
  (:defaults 440 .5 1 1000)
  (with-samples ((i (pulse freq amp))
                 (i (lpf i cutoff-freq 1))
                 (i (* i (envelope (make-perc 0 dur)
                                   1 dur #'incudine:free))))
    (stereo i)))

;;--------------------------------------------------
;; Overtone - Prophet
(dsp! prophet (freq amp dur cutoff-freq rq attack decay)
  (:defaults 440 .5 1 12000 .3 1 2)
  (with-samples
      ((i (+
           (pulse freq 1 (* .1 (+ 1.2 (sine 1 1))))
           (pulse freq 1 (* .8 (/ (+ 1.2 (sine .3 1) .7) 2)))
           (pulse freq 1 (* .8 (/ (+ 1.2 (1- (mod (sine .4 1) 2))) 2)))
           (pulse freq 1 (* .8 (/ (+ 1.2 (1- (mod (sine .4 1 .19) 2))) 2)))
           (* .5 (pulse (/ freq 2)
                        1
                        (* .8 (/ (+ 1.2 (1- (mod (sine (+ 2 (white-noise .2)))
                                                 2)))
                                 2))))))
       (i (incudine.vug:lpf (* i i) cutoff-freq rq))
       (i (* amp .01 i))
       (i (* i (envelope (make-perc 0 dur)
                         1 dur #'incudine:free))))
    (stereo i)))

;;--------------------------------------------------
;; Overtone - All i wanted
(dsp! green (freq dur amp rms)
  (:defaults 440 1 1 0)
  (with-samples
      ((in (incudine.vug:sine (+ freq (* 3 (incudine.vug:sine 3)))))
       (in (incudine.vug:clip in -.4d0 .4d0))
       (in (incudine.vug:lpf in 800d0 1))
       (in (* in (envelope (make-adsr .001 .8 .1 1)
                           (incudine.vug:line 1 0 dur #'incudine:free))))
       (in (* in amp)))
    (setf rms in)
    (out in in)))

(dsp! keen (freq dur amp rms)
  (:defaults 440 1 1 0)
  (with-samples
      ((in (incudine.vug:pulse (+ freq (* 3 (incudine.vug:sine 9)))))
       (in (incudine.vug:lpf in 800 1))
       (in (* in (envelope (make-adsr .08 .2 .1 1)
                           (incudine.vug:line 1 0 dur #'incudine:free))))
       (in (* in amp)))
    (setf rms in)
    (out in in)))

(dsp! bass (freq amp dur rms)
  (:defaults 110 1 4 0)
  (with-samples
      ((decay (min 2d0 (- dur .5d0)))
       (in (sine freq))
       (in (+ in (* .1 (white-noise))))
       (in (lpf in 700 (line 0.3 .1 4 #'free)))
       (in (lpf in 1800 1d0))
       (in (+ in (sine (* freq))))
;;       (in (* in (signum (sine 1))))
       (in (* in (envelope (make-perc .01 decay))))
       (in (* in amp)))
    (setf rms in)
    (out in in)))

;;--------------------------------------------------
;; Overtone - ixi.clj
(define-vug pm-osc (carfreq modfreq pmindex)
  (sine carfreq (sine modfreq 0 pmindex)))

;; FIXME!
(dsp! ixi-kick (mod-freq
                mod-index
                sustain
                beater-noise-level
                pan
                amp)
  (:defaults 2.6 5 .4 .025 0 .3)
  (with-samples ((freq 80)
                 (pitch-contour (line (* freq 2) freq .02 #'identity))
                 (drum-osc (pm-osc pitch-contour mod-freq (/ mod-index 1.3)))
                 (drum-lpf (lpf drum-osc 1000 1))
                 (drum-env (* drum-lpf
                              (envelope (make-perc .005 sustain)
                                        :done-action #'incudine:free)))
                 (beater-source (* (white-noise) beater-noise-level))
                 (beater-hpf (hpf beater-source 500 1))
                 (lpf-cutoff-contour (line 6000 500 .03 #'identity))
                 (beater-lpf (lpf beater-hpf lpf-cutoff-contour 1))
                 (beater-env (* beater-lpf
                                (envelope (make-perc .000001 1)
                                          :done-action #'incudine:free)))
                 (kick-mix (* (+ drum-env beater-env) 2 amp)))
    (stereo kick-mix)))

(define-vug dsp-saw (freq)
  (sine freq))

;; https://github.com/supercollider/supercollider/blob/master/SCClassLibrary/Common/Audio/Noise.sc
(define-vug lf-noise0 ()
  (white-noise))

(dsp! ixi-snare (drum-mode-level
                 snare-level
                 snare-tightness
                 sustain
                 amp)
  (:defaults 1 50 1200 .04 .3)
  (with-samples
      ((freq 305)
       (drum-mode-env (envelope (make-perc .005 sustain)
                                :done-action #'incudine:free))
       (drum-mode-sin-1 (* (sine (* freq .53)) drum-mode-env .5))
       (drum-mode-sin-2 (* (sine freq) drum-mode-env .5))
       (drum-mode-pmosc (* 5 drum-mode-env (pm-osc (dsp-saw (* freq .085))
                                                   184
                                                   (/ .5 1.3))))
       (drum-mode-mix (* drum-mode-level (+ drum-mode-sin-1
                                            drum-mode-sin-2
                                            drum-mode-pmosc)))
       (snare-noise (* amp .8 (lf-noise0)))
       (snare-env (envelope (make-perc .0001 sustain)
                            :done-action #'incudine:free))
       (snare-brf-1 (* .5 (incudine.vug:bpf snare-noise 8000 .1)))
       (snare-brf-2 (* .5 (incudine.vug:bpf snare-brf-1 5000 .1)))
       (snare-brf-3 (* .5 (incudine.vug:bpf snare-brf-2 3600 .1)))
       (snare-brf-4 (* snare-env (incudine.vug:bpf snare-brf-3 2000 .1)))
       (snare-reson (* snare-level (incudine.vug:resonz snare-brf-4 snare-tightness 1)))
       (snare-drum-mix (* amp (+ drum-mode-mix snare-reson))))
    (stereo snare-drum-mix)))


