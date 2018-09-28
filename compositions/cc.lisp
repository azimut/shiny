(in-package :shiny)

(bbuffer-load "/home/sendai/clips/arvo1.wav")
(bbplay "arvo1.wav" :amp .2)

(put-phrase "sound" "arvo1.wav" 3 4.4)
(put-phrase "kill" "arvo1.wav" 25 3.5)
(put-phrase "free" "arvo1.wav" 53 3.5)

(put-phrase "art" "arvo1.wav" 57 3)
(put-phrase "but" "arvo1.wav" 61 3)
(put-phrase "necessary" "arvo1.wav" 64 2.5)

(word-play "sound" :amp .2)
(word-play "kill" :amp .2)
(word-play "free" :amp .2)

(word-play "art" :amp .2)
(word-play "but" :amp .2)
(word-play "necessary" :amp .2)

(bbuffer-load "/home/sendai/clips/obsession.wav")
(put-phrase "obsessed" "obsession.wav" 1.8 .4)
(put-phrase "mind" "obsession.wav" .4 2.8)
(put-phrase "filter" "obsession.wav" 3.5 4)
(bbuffer-load "/home/sendai/clips/go.wav")
(bbplay "go.wav" :amp .2)

(put-phrase "cosmos" "go.wav" 0 5)
(put-phrase "simple" "go.wav" 6.45 3.95)
(put-phrase "endless" "go.wav" 11 3.6)

(word-play "cosmos" :amp .2 :downsamp 8 :beat-length 4)
(word-play "simple" :amp .2 :downsamp 6 :beat-length 4)
(word-play "endless" :amp .2 :downsamp 8 :beat-length 4)

(word-play "mind" :amp .5 :downsamp 6)
(word-play "filter" :amp .5 :downsamp 6)


(aat (tempo-sync #[4 b]) #'f it 0 -12 20)
(aat (tempo-sync #[4 b]) #'f it 1  0  100)
(aat (tempo-sync #[4 b]) #'f it 2  12 64)
(aat (tempo-sync #[4 b]) #'f it 2  24 64)


(fp 2 1)
(fp 0 37)
(fg 1f0)

(defparameter *golang*
  (new cycle
    :of '("cosmos" "simple" "endless")
    :repeat 1))

(defparameter *art*
  (new cycle
    :of '("art" "but" "necessary")
    :repeat 1))

(fg .5)
(defparameter *c0* 0)
(defun f ())
(let* ((d (pval *c0*))
       (s (ov-scale :C3 :phrygian))
       (n (nths '(4 2 1 3 2) s))
       (c (nths '(0 5 6 7) s))
       (p (new weighting
            :of
            `((,c  :weight 1)
              (,n  :weight ,d)
              ((0) :weight .25 :max 1))))
       (q (make-weighting
           (list 1/16 1/8 (make-cycle 1/32 2)))))
  (defun f (time chan offset pan)
    (unless (= chan 0)
      (let ((r (rhythm (next q) 20))
            (note (pickl (next p))))
        (when (not (= note 0))
          (and (odds .1) (incf *c0* .01))
          (when (= chan 0)
            (unless (node-alive 38)
              (when (odds .1) (word-play (next *golang*) :id 38 :amp .4 :pan (pick .2 .5 .8))))
            (if (member note n)
                (viseq:push-cvideo
                 :pi "/home/sendai/clips/pi.mp4"
                 ;;           :pos 23
                 )
                (viseq:push-cvideo
                 :pi "/home/sendai/clips/pi.mp4"
                 :pos 22
                 :rotation (between 10f0 40f0)
                 :xpos 50
                 :ypos 50
                 ))
            (at (+ time #[1 b]) #'viseq:delete-cvideo :pi))
          (when (= chan 1)
            (if (member note n)
                (viseq:push-cvideo
                 :pi2 "/home/sendai/clips/pi-rest.mp4"
                 :pos 40
                 :repeat (pick 2 4)
                 :is-negative t)
                (viseq:push-cvideo
                 :pi2 "/home/sendai/clips/pi-rest.mp4"
                 :pos (between 10 40)
                 ))
            (at (+ time #[2 b]) #'viseq:delete-cvideo :pi2))
          (when (and (odds .1) (= chan 2))
            (unless (node-alive 38)
              (when (word-play (pick "art" "necessary" "kill" "sound" "free")
                               :downsamp (pick 4 8)
                               :id 38 :amp .4 :pan (pick .2 .5 .8))
                (viseq:push-ctext :obs (pick "car" "la") (between 20 50) (between 20 50))
                (at (+ time #[3 b]) #'viseq:delete-ctext :obs))))
          (p
           time
           (cm:transpose note  offset)
           (rcosr 43 3 4)
           r
           chan
           :pan pan))
        (aat (+ time #[r b]) #'f it chan offset pan)))))
