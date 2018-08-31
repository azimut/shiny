(let ((chord (make-cycle (make-chord-fixed 60 3 (scale 0 'ryukyu))))
      (here  (make-cycle '(t nil)))
      ;;(lead  (make-cycle (make-chord-fixed 80 5 (scale 0 'ryukyu))))
      (lead  (make-cycle *spectrum*)))
  (defun f (time)
    (let ((n (next chord)))
      ;;(playcsound *inst3* n 2 2 .2)
      ;; (if (odds .5)
      ;;     (p time 60 60 1 0)
      ;;     (progn
      ;;       (p time 60 60 .5 2 :pan 0)
      ;;       (p (+ time #[.5 b]) 60 60 .5 3 :pan 127)))
      (when (next here)
        (playcsound *inst1* (next lead) 4)))
    (aat (+ time #[2 b])
         #'f it)))

(defpattern k ((gethash 'wm *patterns*) .25)
  (bbplay "kick_OH_F_9.wav" :attenuation .2 :rate 1)
  (bbplay "snare_OH_FF_9.wav" :attenuation .2 :rate .2)
  (bbplay "hihatClosed_OH_F_20.wav" :attenuation .05 :rate 2)
  ;;(bbplay "hihatOpen_OH_FF_6.wav" :attenuation .05 :rate -2)
  )

(k (tempo-sync #[1 b]))
(f (tempo-sync #[1 b]))

(defun f ())
(defun k ())

(let ((chord (make-cycle (make-chord-fixed 60 3 (scale 0 'ryukyu))))
      (here  (make-cycle '(t nil)))
      ;;(lead  (make-cycle (make-chord-fixed 80 5 (scale 0 'ryukyu))))
      (lead  (make-line *expand*)))
  (defun f (time)
    (let ((n (next chord)))
      (playcsound *inst3* (+ 12 n) 2 2 .2)
      ;; (if (odds .5)
      ;;     (p time 60 60 1 0)
      ;;     (progn
      ;;       (p time 60 60 .5 2 :pan 0)
      ;;       (p (+ time #[.5 b]) 60 60 .5 3 :pan 127)))
      (when (next here)
        (playcsound *inst3* (cm:transp (next lead) 0) 2 3 0.4)))
    (aat (+ time #[1 b])
         #'f it)))



(defpattern k ((gethash 'getup *patterns*) .5)
  (playcsound *inst3* *gm-kick* d 4 .9)
  (playcsound *inst3* *gm-snare* d 2 .2)
  (playcsound *inst3* *gm-closed-hi-hat* d 4 .1))

(defun k ())
(k (now))

