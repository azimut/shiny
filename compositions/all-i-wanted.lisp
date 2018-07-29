(in-package :somecepl)

;; instruments were attempts to copy the ones from ctford song
;; from overtone/supercollider to incudine, some thing I dunno how to translate
;; melody is just me trying to make things sound less awful, haven't read original
;; song melody just yet

(defvar *buffers* (make-hash-table :test #'equal))
(defvar *sample-words* (make-hash-table :test #'equal))

(defun bbuffer-load (filename)
  "buffer-load wrapper for global hash caching and easy access"
  (let ((hkey (file-namestring filename)))
    (when (not (gethash hkey *buffers*))
      (setf (gethash hkey *buffers*)
            (buffer-load filename)))))

(defstruct phrase
  filename
  start-at
  dur)

(defun put-phrase (word filename start-at dur)
  (setf (gethash word *sample-words*)
        (make-phrase :filename filename
                     :start-at start-at
                     :dur dur)))

(dsp! bplay ((buf buffer) rate start-pos (loop-p boolean) attenuation rms cfreq)
  (:defaults 0d0 1 0 nil .00001 0 100)
  (with-samples ((in (incudine.vug:buffer-play
                      buf rate start-pos loop-p #'stop))
                 (inn (* in attenuation))
                 (inn (+ inn
;;                        (incudine.vug:lpf inn 500 50)
                        (incudine.vug:hpf inn 100 20)
;;                        (incudine.vug:butter-hp inn 1020)
                        )))
    (out inn inn)))

(defun word-play (word rate
               &key (loop-p nil) (attenuation 1f0) (id 2))
  "bplay wrapper with duration support AND word slice"
  (when word
    (let* ((obj (gethash word *sample-words*))
           (dur (phrase-dur obj))
           (buf (gethash (phrase-filename obj)
                         *buffers*))
           (sample-rate (buffer-sample-rate buf))
           (start-pos (* sample-rate (phrase-start-at obj))))
      (bplay buf rate start-pos loop-p
             :attenuation attenuation
             :id id)
      (when dur
        (at (+ (now) #[dur b]) #'free (node id))))))

(bbuffer-load "/home/sendai/curso/furi-dream-cc.wav")
(bbuffer-load "/home/sendai/curso/furi-better-cc.wav")
(bbuffer-load "/home/sendai/curso/furi-eternity-cc.wav")
(bbuffer-load "/home/sendai/curso/furi-forget-cc.wav")
(bbuffer-load "/home/sendai/curso/furi-search-cc.wav")
(bbuffer-load "/home/sendai/curso/furi-clock-cc.wav")


(put-phrase "nothing" "furi-better-cc.wav" 1.7 1.9)
(put-phrase "better" "furi-better-cc.wav" 0 1.2)
(put-phrase "stuck" "furi-better-cc.wav" 4 1.45)
(put-phrase "jstuck" "furi-better-cc.wav" 4.35 .46)
(put-phrase "real" "furi-better-cc.wav" 5.4 4)

(put-phrase "dream" "furi-dream-cc.wav" 0 6)
(put-phrase "feel" "furi-dream-cc.wav" 6.05 5)

(put-phrase "eternity" "furi-eternity-cc.wav" 0 4.1)
(put-phrase "wait" "furi-eternity-cc.wav" 3.2 .6)

(put-phrase "into" "furi-forget-cc.wav" 0 4.7)
(put-phrase "matters" "furi-forget-cc.wav" 2.5 2)
(put-phrase "forget" "furi-forget-cc.wav" 15.4 2.3)
(put-phrase "her" "furi-forget-cc.wav" 19 2)

(put-phrase "search" "furi-search-cc.wav" 0 5.8)
(put-phrase "searchsearch" "furi-search-cc.wav" 0 2.8)
(put-phrase "hope" "furi-search-cc.wav" 6 4.5)
(put-phrase "missing" "furi-search-cc.wav" 13.4 2.35)

(put-phrase "time" "furi-clock-cc.wav" 0 4.7)
(put-phrase "still" "furi-clock-cc.wav" 6.9 4)
(put-phrase "stop" "furi-clock-cc.wav" 11.8 1.8)
(put-phrase "tick" "furi-clock-cc.wav" 16.5 3.5)

(word-play "time" 1d0 :attenuation 2d0 :id 40)

;;<3
(dsp! green (freq dur volume rms)
  (:defaults 440 1 1 0)
  (with-samples
      ((in (incudine.vug:sine (+ freq (* 3 (incudine.vug:sine 3)))))
       (in (incudine.vug:clip in -.4d0 .4d0))
       (in (incudine.vug:lpf in 800d0 1))
       (in (* in (envelope (make-adsr .001 .8 .1 1)
                           (incudine.vug:line 1 0 dur #'incudine:free))))
       (in (* in volume)))
    (setf rms in)
    (out in in)))

(dsp! keen (freq dur volume rms)
  (:defaults 440 1 1 0)
  (with-samples
      ((in (incudine.vug:pulse (+ freq (* 3 (incudine.vug:sine 9)))))
       (in (incudine.vug:lpf in 800 1))
       (in (* in (envelope (make-adsr .08 .2 .1 1)
                           (incudine.vug:line 1 0 dur #'incudine:free))))
       (in (* in volume)))
    (setf rms in)
    (out in in)))

(dsp! bass (freq volume dur rms)
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
       (in (* in volume)))
    (setf rms in)
    (out in in)))

;;--------------------------------------------------

(defun f ())

(f (tempo-sync #[12 b]))

(let* ((pc (scale 0 'aeolian))
       (r  1)
       (bass  (make-cycle '(t nil nil nil)))
       (upper (make-cycle '(t nil)))
       (factor (make-heap '(1 2 3)))
       (lead  (make-cycle  (make-chord-fixed 60 5 pc)))
       (phrase nil)
       (phrase (make-cycle '("dream" nil nil nil "better" nil
                             "real" nil
                             "stuck" nil "stuck"  nil "jstuck" nil "jstuck" nil nil nil)))
;;     (phrase (make-cycle '("dream" nil nil nil)))
;;       (phrase (make-cycle '("wait" nil nil)))
       )
  (defun f (time)
    (let ((chord (make-chord 50 70 2 pc))
          (d (next r)))
      
      (and (not (node-id (node 40)))
           (word-play (next phrase) .9
                      :attenuation 2f0
                      :id 40))
      
      (green (cm:hertz (car (next chord)))
             :dur (pick d (/ d 2))
             :volume .2
             :id 10)
      
      (if *hsv* (setf *hsv* nil) (setf *hsv* t))

      (green (cm:hertz (next lead))
             :dur (/ d 2)
             :volume .2
             :id 11)
      (at (+ time #[ (/ d 2) b])
          #'green (cm:hertz (next lead))
          :dur (/ d 2)
          :volume .4)
      (when (next bass)
        (setf *single* (pick 50 100 50 25))
        (bass (cm:hertz (+ -12 (car chord)))
              :dur 4
              :volume .25
              :id 2))
      (when (next upper)
        (setf *factor* (next factor))
        (keen (cm:hertz (+ (pick 0 12) (cadr chord)))
              :dur (* 2 d)
              :volume .2
              :id 3))
      (at (+ time #[d b]) #'f (+ time #[d b])))))

(f (tempo-sync #[12 b]))

(defun f ())

;; just repeat the lead note twice
(let* ((pc (scale 0 'aeolian))
       (r  1)
       (factor (make-heap '(1 2 3 1 1)))
       (bass  (make-cycle '(nil nil nil t)))
       (upper (make-cycle '(nil t)))
       (phrase nil)
       (phrase (make-cycle '("into" nil nil nil
                             "matters" nil nil nil
                             "into" nil
                             "matters" nil "matters" nil nil nil)))
       (lead  (make-cycle  (make-chord-fixed 60 5 pc))))
  (defun f (time)
    (let ((chord (make-chord 50 70 2 pc))
          (d (next r)))
      (if *hsv* (setf *hsv* nil) (setf *hsv* t))
      (and (not (node-id (node 40)))
           (word-play (next phrase) .9
                      :attenuation 2f0
                      :id 40))

      (green (cm:hertz (car (next chord)))
             :dur (pick d (/ d 2))
             :volume .2)
      
      (let ((n (next lead)))
       (green (cm:hertz n)
              :dur (/ d 2)
              :volume .2)
       (at (+ time #[ (/ d 2) b])
           #'green (cm:hertz n)
           :dur (/ d 2)
           :volume .4))


      ;; (let ((n (next lead)))
      ;;  (green (cm:hertz n)
      ;;         :dur (/ d 2)
      ;;         :volume .2)
      ;;  (at (+ time #[.25 b])
      ;;      #'green (cm:hertz n)
      ;;      :dur (/ d 2)
      ;;      :volume .4)
      ;;  (at (+ time #[.5 b])
      ;;      #'green (cm:hertz n)
      ;;      :dur (/ d 2)
      ;;      :volume .4)
      ;;  (at (+ time #[.75 b])
      ;;      #'green (cm:hertz n)
      ;;      :dur (/ d 2)
      ;;      :volume .4))

      
      (when (next bass)
        (setf *single* (pick 50 100 50))
        (bass (cm:hertz (+ -12 (car chord)))
              :dur 4
              :volume .25))
      (when (next upper)
        (setf *factor* (next factor))

        (keen (cm:hertz (+ (pick 0 12) (cadr chord)))
              :dur (* 2 d)
              :volume .2))
      (at (+ time #[d b]) #'f (+ time #[d b])))))


(defvar *reps* .1)
(setf *reps* .9)

(let* ((pc (scale 0 'aeolian))
       (r  1)
       (factor (make-heap '(2 3 4 3 4)))
       (bass  (make-cycle '(nil nil nil t)))
       (upper (make-cycle '(nil t)))
       (lead  (make-cycle (make-chord-fixed 60 5 pc)))
       (phrase nil)
       (phrase (make-cycle '("search" nil nil nil
                             "searchsearch" nil nil nil
                             "hope" nil nil nil
                             "searchsearch" nil nil nil
                             "missing" nil nil nil nil)))
;;      (phrase "wait")
       (odds  (make-weighting `(.1 (.5 ,(pval *reps*))))))
  (defun f (time)
    (let ((chord (make-chord 50 70 2 pc))
          (d (next r)))

      (and (not (node-id (node 40)))
           (word-play (next phrase) .9
                      :attenuation 2f0
                      :id 40))

      (if *hsv* (setf *hsv* nil) (setf *hsv* t))

      (green (cm:hertz (car (next chord)))
             :dur (pick d (/ d 2))
             :volume .2)
      

      (let ((n (next lead)))
       (green (cm:hertz n)
              :dur (/ d 2)
              :volume .2)
       (and (odds (next odds))
            (at (+ time #[.25 b])
                #'green (cm:hertz n)
                :dur (/ d 2)
                :volume .4))
       (at (+ time #[.5 b])
           #'green (cm:hertz n)
           :dur (/ d 2)
           :volume .4)
       (and (odds (next odds))
            (at (+ time #[.75 b])
                #'green (cm:hertz n)
                :dur (/ d 2)
                :volume .4))
       )
      (setf *single* (pick 50 50 25))
      (when (next bass)
;;        (setf *single* (pick 50 100 50))
        (bass (cm:hertz (+ -12 (car chord)))
              :dur 4
              :volume .25))
      (when (next upper)
        (setf *factor* (next factor))
        (keen (cm:hertz (+ (pick 0 12) (cadr chord)))
              :dur (* 2 d)
              :volume .2))
      (at (+ time #[d b]) #'f (+ time #[d b])))))

;;--------------------------------------------------
(let ((phrase (make-cycle '("time" nil nil nil
                            "still" nil nil nil
                            "stop" nil nil nil
                            "tick" nil nil nil))))
  (defun f (time)
    (setf *factor* 1)
    (setf *single* 100)
    (setf *size* 1f0)
    (setf *stop* t)
    (and (not (node-id (node 40)))
         (word-play (next phrase) 1d0
                    :attenuation 2f0
                    :id 40))
    (at (+ time #[1 b]) #'f (+ time #[1 b]))))
;;--------------------------------------------------


(let* ((pc (scale 0 'aeolian))
       (r  1)
       (bass  (make-cycle '(nil nil nil t)))
       (upper (make-cycle '(nil t)))
       (factor (make-heap '(2 3 4 3 4)))
       (lead  (make-cycle  (make-chord-fixed 60 5 pc))))
  (defun f (time)
    (let ((chord (make-chord 50 70 2 pc))
          (d (next r)))
;;      (setf *stop* nil)
      (if *hsv* (setf *hsv* nil) (setf *hsv* t))

      (green (cm:hertz (car (next chord)))
             :dur (pick d (/ d 2))
             :volume .4)
      (green (cm:hertz (next lead))
             :dur (/ d 3)
             :volume .2)
      (at (+ time #[(/ d 3) b])
          #'green (cm:hertz (next lead))
          :dur (/ d 3)
          :volume .3)
      (at (+ time #[(* 2 (/ d 3)) b])
          #'green (cm:hertz (next lead))
          :dur (/ d 3)
          :volume .4)
      (when (next bass)
        (setf *single* (pick 50 100 50 25))
        (bass (cm:hertz (+ -12 (car chord)))
              :dur 4
              :volume .25))
      (when (next upper)
        (setf *factor* (next factor))                
        (keen (cm:hertz (+ (pick 0 12) (cadr chord)))
              :dur (* 2 d)
              :volume .2))
      (at (+ time #[d b]) #'f (+ time #[d b])))))

;; (defun gg ())
;; (defun gg (time)
;;   (let ((d (pick 2 2 2 2 2 2 2 2 2 2 1)))
;;     (organ (cm:hertz (pick 48 48 48 48 48 48 48 48 60))
;;            :dur d
;;            :volume .2
;;            :id 20)
;;     (at (+ time #[d b]) #'gg (+ time #[d b]))))
;; (gg (tempo-sync #[12 b]))

(let* ((main (make-cycle '(nil nil nil nil nil t)))
       (lead (make-heap (make-chord-fixed 75 3 (scale 0 'aeolian))))
       (sizes (make-heap '(.2 .3 .4 .5 .6 .7 .8 .9)))
       (phrase nil)
       (phrase (make-cycle '("her" nil nil nil
                             "her" nil nil nl
                             nil nil nil nil
                             nil nil nil nil)))
       )
  (defun dd (time)
    (let ((l (next lead)))
      (p time l (rcosr 70 5 5) 1 1 :pan (pick 0 127))
      (and (not (node-id (node 40)))
           (word-play (next phrase) .9
                      :attenuation 2f0
                      :id 40))

      (setf *size* .01)
      (when (next main)        
        (p time (+ -12 l) (rcosr 60 5 5) 1 2)
        (setf *size* (next sizes))
        (green (cm:hertz l) :dur 6 :volume .5d0)
        (at (+ time #[.2 b])
            #'green (cm:hertz l) :dur 6.1 :volume .5d0)
        ))
    (at (+ time #[1 b]) #'dd (+ time #[1 b]))))

(dd (tempo-sync #[12 b]))
(defun dd ())

(fp 2 2)
(fp 1 4)
(fg 1f0)

(f (tempo-sync #[12 b]))
(flush-pending)

(setf *stop* t)
(defun f ())

(incudine:free (node 0))

;;--------------------------------------------------
;; BELOW THIS POINT WIP...



;; (bass)
;; (incudine:free (node 0))
;; (dsp! organ (freq dur volume)
;;   (:defaults 440 1 1)
;;   (with-samples
;;       ((in (mod (sine freq) 1d0))
;;        (in (lpf in 500 1))
;;        (in (hpf in
;;                 (* 200 (+ 1 (sine 9)))
;;                 .1))
;;        (in (* in (envelope (make-adsr .001 .8 .1 1)
;;                            (line 1 0 dur #'free)))))
;;     (out in in)))
;; (organ)
;; (incudine:free (node 0))
