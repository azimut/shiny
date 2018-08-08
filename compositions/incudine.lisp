(bbuffer-load "/home/sendai/projects/sonic-pi/etc/samples/guit_em9.flac")
(bbuffer-load "/home/sendai/projects/sonic-pi/etc/samples/loop_garzul.flac")

(defun f (time)
  (bbplay (gethash "guit_em9.flac" *buffers*)
          :rate (pick .25 .5 1)
          :attenuation .5)
  (bbplay (gethash "loop_garzul.flac" *buffers*)
          :rate (pick .5 1)
          :attenuation 1)
  (aat (+ time #[8 b]) #'f it))

(defun f ())
(f (now))

;;--------------------------------------------------

(bbuffer-load "/home/sendai/Downloads/sample/EM0903.wav")

(bbuffer-load "/home/sendai/Downloads/sample/EM0902-EM0902.wav")
(bplay (gethash "EM0902-EM0902.wav" *buffers*) 1 0 t
       :attenuation 1)

(bbuffer-load "/home/sendai/Downloads/sample/LOG0106.wav")
(bplay (gethash "LOG0106.wav" *buffers*) 1 0 t
       :attenuation 1
       :id 2)

(bplay-beat (gethash "EM0903.wav" *buffers*) 2 nil)

(bplay (gethash "EM0903.wav" *buffers*) 1 0 t
       :attenuation 1)
(put-phrase "nuisance" "EM0903.wav" 21 1)
(put-phrase "why" "EM0903.wav" 8.4 2)
(put-phrase "feel" "EM0903.wav" 10.5 3)

(word-play (pick "why" "nuisance" "feel")
           :rate (pick 1 2 1.5)
           :id (1+ (random 30)))
(word-play "why") 
(word-play "nuisance" :beat-length 4)
(incudine:free (node 0))

(bbuffer-load "/home/sendai/whatever.wav")
(bplay (gethash "whatever.wav" *buffers*) 1 0 t
       :attenuation 1)

(set-controls
 2
 :rate 10f0 ; (drunk 5f0 2f0)
 :attenuation .4d0
 :cfreq 10)

;;--------------------------------------------------

(play-sample 'drum *gm-cowbell*)

(make-instrument 'drum "/home/sendai/Downloads/sample/OH/")

(push-note 'drum *gm-kick* "kick_OH_F_9.wav")
(push-note 'drum *gm-side-stick* "snareStick_OH_F_9.wav")
(push-note 'drum *gm-snare* "snare_OH_FF_9.wav")
(push-note 'drum *gm-closed-hi-hat* "hihatClosed_OH_F_20.wav")
(push-note 'drum *gm-pedal-hi-hat* "hihatFoot_OH_MP_12.wav")
(push-note 'drum *gm-open-hi-hat* "hihatOpen_OH_FF_6.wav")
(push-note 'drum *gm-low-floor-tom* "loTom_OH_FF_8.wav")
(push-note 'drum *gm-hi-floor-tom* "hiTom_OH_FF_9.wav")
(push-note 'drum *gm-crash* "crash1_OH_FF_6.wav")
(push-note 'drum *gm-ride* "ride1_OH_FF_4.wav")
(push-note 'drum *gm-chinese* "china1_OH_FF_8.wav")
(push-note 'drum *gm-cowbell* "cowbell_FF_9.wav")
(push-note 'drum *gm-open-triangle* "bellchime_F_3.wav")
(push-note 'drum *gm-ride-bell* "ride1Bell_OH_F_6.wav")

(defun f (time dur &optional (beat 0))
  (and (zmod beat 1) (play-instrument 'drum *gm-kick* dur .4))
  (and (zmod beat 3) (play-instrument 'drum *gm-snare* dur .4))
  (and (zmod beat 2) (play-instrument 'drum *gm-side-stick* dur .4))
  (and (zmod beat 2.5) (play-instrument 'drum *gm-side-stick* dur .4))
  (aat (+ time #[dur b]) #'f it .5 (+ beat dur)))

(defun f ())
(f (now) 1)

;;--------------------------------------------------

(define-vug grain-gen-id
    (buf unit-rate frames gain rate start-pos lp-freq
         lp-q lp-dist peak-freq peak-q peak-gain hp-freq hp-q
         a length r)
  (with-samples
      ((snippet (buffer-read buf (* (phasor (* rate unit-rate) start-pos) frames)
                             :wrap-p nil :interpolation :cubic)))
    (lpf18
     (peak-eq
      (hpf
       (* (envelope (make-local-envelope `(0 ,gain ,gain 0) `(,a ,length ,r)) 1 1 #'identity)
          snippet)
       hp-freq hp-q)
      peak-freq peak-q peak-gain)
     lp-freq lp-q lp-dist)))
