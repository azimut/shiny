(in-package :shiny)

(make-orc :ourvoice
          :filename "ourvoice"
          :filepath "/home/sendai/projects/csound-instruments/pinkston/")

;;(start-csound (get-orchestra :ourvoice))
(start-thread)

(start-csound
 (merge-orcs
  (get-orchestra :ourvoice)
  (get-orchestra :xanadu)))

(make-play voice-a "i1"
           :amp 10000 :keynum 60 :fmt1 609 :db1 0 :fmt2 1000 :db2 -6
           :fmt3 2450 :db3 -12 :fmt4 2700 :db4 -11 :fmt5 3240 :db5 -24
           :left 1f0 :right 1f0)
(make-play voice-e "i1"
           :amp 10000 :keynum 60 :fmt1 400 :db1 0 :fmt2 1700 :db2 -9
           :fmt3 2300 :db3 -8 :fmt4 2900 :db4 -11 :fmt5 3400 :db5 -19
           :left 1f0 :right 1f0)
(make-play plucke "i2" :p4 0 :keynum 60)
(make-play pluck  "i3" :p4 0 :keynum 60)
(make-play newfm  "i4" :p4 0 :keynum 60 :llimit .2 :rlimit 2.0)

(bbuffer-load "/home/sendai/Downloads/sample/OH/kick_OH_F_9.wav")
(bbuffer-load "/home/sendai/Downloads/sample/OH/snare_OH_F_9.wav")
(bbuffer-load "/home/sendai/clips/obsession.wav")
(put-phrase "obsessed" "obsession.wav" 1.8 .4)

(put-phrase "mind" "obsession.wav" .4 2.8)
(put-phrase "filter" "obsession.wav" 3.5 4)

(word-play "filter" :amp .2)
(word-play "mind" :amp .2)

(bbplay "obsession.wav" :amp .2)

(defpattern k (("x-x- x-x- x-x- x-xx"
                "--x") .25)
  (bbplay "kick_OH_F_9.wav" :amp .03 :rate (between .8 1.2))
  (if (odds .5)
      (bbplay "snare_OH_F_9.wav" :amp .03 :rate (between .8 1.2))
      (play-plucke 30 .5)))
(defun k ())
(k (tempo-sync #[1 b]))

(fp 0 22)
(let ((voice (parse-patternc "xxx-"))
      (lead (make-heap (append '(0) (ov-scale :C3 :minor)))))
  (defun f (time)
    (p time (next lead) 40 1 0)
    (and (and (odds .01) (not (node-alive 9)))
         (word-play "obsessed" :id 9 :amp .2))
    (if (next voice)
        (play-voice-a 60 1 :amp 522 :right .1)
        (play-voice-e 60 1 :amp 522 :left .1))
    (aat (+ time #[1 b]) #'f it)))

(defun f ())
(f (tempo-sync #[1 b]))

(let* ((voice (parse-patternc "xxx-"))
       (prob (make-line (iota 30 :start .1 :step .01)))
       (q (make-weighting (list (make-cycle 1 2)
                                2
                                .5)))
       (chords (new weighting :of
                    `(((40 43 47) :weight .9)
                      ((40 44 47) :weight .9)
                      ((40 43 46) :weight ,(pval (next prob)) :max 1)))))
  (defun f (time)
    (let ((c (next chords))
          (r (next q)))
      
      (if (next voice)
          (play-voice-a 60 1 :amp 522 :right .1)
          (play-voice-e 60 1 :amp 522 :left .1))

      (pa time c (* r .333) '(50 45 45) 1 (* r .333))
      ;; (pa time (cm::tintab (cm::transp c 10) cm::ionian)
      ;;     (* r .333) 50 2 (* r .333))
      (and (and (odds .1) (not (node-alive 9)))
           (word-play "obsessed" :id 9 :amp .2))
      (aat (+ time #[r b]) #'f it))))

(fp 2 21)
(fp 1 22)
(fp 2 20)
;; (bbuffer-load "/home/sendai/clips/numerologist.wav")
;; (bbplay "numerologist.wav" :amp .2)
;; (put-phrase "soon" "numerologist.wav" 0 1.7)
;; (put-phrase "discard" "numerologist.wav" 1.7 3)
;; (put-phrase "longer" "numerologist.wav" 5.1 2.8)
;; (put-phrase "numerologist"
;;             "numerologist.wav" 8.3 2)
;; (word-play "soon" :amp .2)
;; (word-play "discard" :amp .2)
;; (word-play "longer" :amp .2)
;; (word-play "numerologist" :amp .2)
;; (bbplay "obsession.wav" :amp .2 :downsamp 6)


;; SHINY> (nths '(4 2 1 3 2) (ov-scale :C3 :minor))
;; (55 51 50 53 51)
;; '(48 56 58 60)

