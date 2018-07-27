(in-package :somecepl)


;;<3
(dsp! green (freq dur volume)
  (:defaults 440 1 1)
  (with-samples
      ((in (incudine.vug:sine (+ freq (* 3 (incudine.vug:sine 3)))))
       (in (incudine.vug:clip in -.4d0 .4d0))
       (in (incudine.vug:lpf in 800d0 1))
       (in (* in (envelope (make-adsr .001 .8 .1 1)
                           (incudine.vug:line 1 0 dur #'incudine:free))))
       (in (* in volume)))
    (out in in)))

(dsp! keen (freq dur volume)
  (:defaults 440 1 1)
  (with-samples
      ((in (incudine.vug:pulse (+ freq (* 3 (incudine.vug:sine 9)))))
       (in (incudine.vug:lpf in 800 1))
       (in (* in (envelope (make-adsr .08 .2 .1 1)
                           (incudine.vug:line 1 0 dur #'incudine:free))))
       (in (* in volume)))
    (out in in)))

(let* ((pc (scale 0 'aeolian))
       (r  1)
       (bass  (make-cycle '(t nil nil nil)))
       (upper (make-cycle '(t nil)))
       (lead  (make-cycle (make-chord 60 80 5 pc))))
  (defun f (time)
    (let ((chord (make-chord 50 70 2 pc))
          (d (next r)))
      (green (cm:hertz (car (next chord)))
             :dur (pick d (/ d 2))
             :volume .2)
      (green (cm:hertz (next lead))
             :dur (/ d 2)
             :volume .2)
      (at (+ time #[ (/ d 2) b])
          #'green (cm:hertz (next lead))
          :dur (/ d 2)
          :volume .4)
      (when (next bass)
        (bass (cm:hertz (+ -12 (car chord)))
              :dur 4
              :volume .3))
      (when (next upper)
        (keen (cm:hertz (+ (pick 0 12) (cadr chord)))
              :dur (* 2 d)
              :volume .2))
      (at (+ time #[d b]) #'f (+ time #[d b])))))

(f (tempo-sync #[1 b]))
(flush-pending)

(defun f ())

(incudine:free (node 0))

;;--------------------------------------------------
;; BELOW THIS POINT WIP...

(dsp! bass (freq volume dur)
  (:defaults 110 1 4)
  (with-samples
      ((decay (min 2d0 (- dur .5d0)))
       (in (sine freq))
       (in (+ in (* .1 (white-noise))))
       (in (lpf in 700 (line 0.3 .1 4 #'free)))
       (in (lpf in 1800 1d0))
       (in (+ in (sine (* freq))))
;;       (in (* in (signum (sine 1))))
       (in (* in (envelope (make-perc .01 decay))))
       (in (* in volume)))
    (out in in)))

(bass)

(incudine:free (node 0))
(incudine.vug:sine )
(dsp! organ (freq dur volume)
  (:defaults 440 1 1)
  (with-samples
      ((in (signum (sine freq)))
       (in (lpf in 500 1))
       (in (hpf in
                (* 200 (+ 1 (sine 9)))
                .1))
       (in (* in (envelope (make-adsr .001 .8 .1 1)
                           (line 1 0 dur #'free)))))
    (out in in)))

(organ)
(incudine:free (node 0))
