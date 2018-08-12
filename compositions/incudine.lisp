(in-package :somecepl)

(define-vug select (l)
  (car (alexandria:shuffle l)))

(dsp! ambi (freq vol)
  (:defaults 100 1)
  (with-samples
      ((note  (incudine.vug:lag 70 1))
       (freq  (midihz note))
       (detune1 (incudine.vug:lag 12 1))
       (detune2 (incudine.vug:lag 24 1))
       (freq2 (midihz (+ note detune1)))
       (freq3 (midihz (+ note detune2)))
       (noise (select (list (pink-noise .002)
                            (white-noise .002))))
       (in (+ (incudine.vug:ringz noise freq  .2)
              (incudine.vug:ringz noise freq  .2)
              (incudine.vug:ringz noise freq2 .2)
              (incudine.vug:ringz noise freq3 .2)))
       (in (tanh in))
       (in (incudine.vug:lpf in 110 .1))
       (in (* 100 in)))
    (out in in)))

(ambi :id 2)
(incudine:free (node 2))


;; https://www.systemshock.org/shocklogs/
(bbuffer-load "/home/sendai/Downloads/sample/LOG0119.wav")
(bbuffer-load "/home/sendai/Downloads/sample/LOG0120.wav")

(put-phrase "joy"        "LOG0120.wav"  5.5 3.7)
(put-phrase "apart"      "LOG0120.wav"  9   4)
(put-phrase "separate"   "LOG0120.wav" 13    .8)
(put-phrase "individual" "LOG0120.wav" 14.5 4)
(put-phrase "undo"       "LOG0119.wav" 17   2.2)

(word-play "separate" :rate -1)

;;--------------------------------------------------

(bbuffer-load
 "/home/sendai/projects/sonic-pi/etc/samples/guit_em9.flac")
(bbuffer-load
 "/home/sendai/projects/sonic-pi/etc/samples/loop_garzul.flac")

(defun f (time)
  (bbplay (gethash "ice.ogg" *buffers*)
          ;;          :rate (pick .25 .5 1)
          :beat-length 8
          :attenuation .5
;;          :id 2
          )
;;   (bbplay (gethash "guit_em9.flac" *buffers*)
;;           ;;          :rate (pick .25 .5 1)
;;           :beat-length 8
;;           :attenuation .5
;; ;;          :id 2
;;           )
  ;; (bbplay (gethash "loop_garzul.flac" *buffers*)
  ;;         :rate (pick .5 1)
  ;;         :attenuation 1
  ;;         :id 3)
  (aat (+ time #[8 b]) #'f it))

(defun f ())
(f (now))
(incudine:free (node 0))

;;--------------------------------------------------
(bbuffer-load "/home/sendai/Downloads/sample/EM0902-EM0902.wav")
(bplay (gethash "EM0902-EM0902.wav" *buffers*) 1 0 nil
       :attenuation 1
       :id 2)

(bbuffer-load "/home/sendai/Downloads/sample/LOG0106.wav")
(bbplay (gethash "LOG0106.wav" *buffers*)
        :attenuation 1
        :id 2
        :loop-p nil)

(bbuffer-load "/home/sendai/Downloads/sample/EM0903.wav")
(bbplay (gethash "EM0903.wav" *buffers*)
        :beat-length 8)
(bplay (gethash "EM0903.wav" *buffers*) 1 0 nil
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
(bplay (gethash "whatever.wav" *buffers*) 1 0 nil
       :attenuation 1
       :id 2)

(defun g ())
(defun g (time)
  (let ((cfreq (drunk 60 5)))
    (set-controls
     2
     :cfreq cfreq
     :rate (funcall (pick '+ '+ '+ '- '+ '+ '+ '+)
                    (+ cfreq (pick 0 0 1 -1 -2)))))
  (aat (+ time #[1 b]) #'g it))
(g (now))

(set-controls
 2
 :rate 1f0
 :attenuation 1d0
 :cfreq 1)

;;--------------------------------------------------

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
