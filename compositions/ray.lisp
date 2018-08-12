(in-package :somecepl)

;;; Code from "undo" video, along with examples/20180810.lisp

(let ((bd (make-cycle (bd (get-pattern 'cold))))
      (sn (make-cycle (sn (get-pattern 'cold))))
      (ch (make-cycle (ch (get-pattern 'cold))))
      (oh (make-cycle (oh (get-pattern 'cold))))
      (cch (make-cycle '(1 1 .9)))
      (tt (make-weighting  '(.2 (.15 .01))))
      (c (make-cycle (make-chord 80 90 3 (scale 0 'dorian)))))
  (defun f (time)
    (when (next bd)
      (play-instrument 'drum *gm-kick* .5))
    (when (next sn)
      (play-instrument 'drum *gm-snare* .5))
    (when (next ch)
      (play-instrument 'drum *gm-closed-hi-hat* 1 1 (pick .9 1 1.1)))
    (when (next oh)
      (pa time
          (nth-inc 0 -12 (make-chord 70 90 (pick 2 3 3 3 4) (scale 0 'dorian)))
          (pick .3 .2 .15 .1) 90  0 .3)
      (play-instrument 'drum *gm-open-hi-hat* (pick 1 1 1 .5 .25) (pick .8 .6 .7) (pick .8 .9 1)))
    (aat (+ time #[(next tt) b])
         #'f it)))

(fg .5)
(defun f ())
(f (now))

(bbuffer-load "/home/sendai/projects/Moonlet/Samples/ice.ogg")
(bbuffer-load "/home/sendai/projects/Moonlet/Samples/hat.ogg")
(bbuffer-load "/home/sendai/projects/Moonlet/Samples/kick.ogg")
(bbuffer-load "/home/sendai/projects/Moonlet/Samples/openhat.ogg")
(bbuffer-load "/home/sendai/projects/Moonlet/Samples/snare.ogg")

(freverb-toggle 1)
(freverb-preset 5)

(let ((lead (make-heap (make-chord 80 90 3 (scale 0 'dorian)))))
  (defun f (time)
    (let ((c (make-chord-alberti 60 75 (scale 0 'dorian))))
      (p time (next lead) 80 2 3)
;;      (pa time (nth-inc 0 -12 c) .5 '(60 65 70) '(0 1 2) '(.5 1 1))
      )
    (aat (+ time #[2 b])
         #'f it)))

(fp 3 24)

(let* ((p  (get-pattern 'kml))
       (bd (make-cycle (bd p)))
       (sn (make-cycle (sn p)))
       (oh (make-cycle (oh p)))}
       (ch (make-cycle (ch p))))
  (defun g (time)
    (when (next bd) (bbplay (gethash "kick.ogg" *buffers*)
                            :attenuation .2
                            :beat-length .1))
    (when (next oh) (bbplay (gethash "openhat.ogg" *buffers*)
                            :attenuation .2
                            :beat-length 1))
    (when (next ch) (bbplay (gethash "hat.ogg" *buffers*)
                            :attenuation .2
                            :beat-length .5))
    (when (next sn) (bbplay (gethash "snare.ogg" *buffers*)
                            :attenuation .2
                            :beat-length .5))
    (aat (+ time #[.2 b])
         #'g it)))

(fg 1f0)
(fp 0 0)
(fp 1 0)

(f (tempo-sync #[.2 b]))
(defun g ())
(g (tempo-sync #[2 b]))



;;--------------------------------------------------

;; https://www.systemshock.org/shocklogs/
(bbuffer-load "/home/sendai/Downloads/sample/LOG0119.wav")
(bbuffer-load "/home/sendai/Downloads/sample/LOG0120.wav")

(put-phrase "joy"        "LOG0120.wav"  5.5 3.7)
(put-phrase "apart"      "LOG0120.wav"  9   4)
(put-phrase "separate"   "LOG0120.wav" 13    .8)
(put-phrase "individual" "LOG0120.wav" 14.5 4)
(put-phrase "undo"       "LOG0119.wav" 17   2.2)

(bbuffer-load "/home/sendai/Downloads/sample/Dirt-Samples-master/gtr/0001_cleanC.wav")
(bbuffer-load "/home/sendai/Downloads/sample/Dirt-Samples-master/gtr/0002_ovrdC.wav")
(bbuffer-load "/home/sendai/Downloads/sample/Dirt-Samples-master/gtr/0003_distC.wav")

(bbuffer-load "/home/sendai/projects/Moonlet/Samples/hat.ogg")
(bbuffer-load "/home/sendai/projects/Moonlet/Samples/kick.ogg")
(bbuffer-load "/home/sendai/projects/Moonlet/Samples/openhat.ogg")
(bbuffer-load "/home/sendai/projects/Moonlet/Samples/snare.ogg")

(let* ((c (make-heap  (scale 0 'dorian)))
       (p (get-pattern 'getup))
       (bd (make-cycle (bd p)))
       (sn (make-cycle (sn p)))
       (oh (make-cycle (oh p)))
       (ch (make-cycle (ch p)))
       (guitar (make-cycle '(700 701 702 703 704 705 706 707 708 709 710)))
       (h (make-cycle '(nil nil nil nil nil nil nil nil nil nil
                        nil nil nil nil nil nil nil nil t))))
  (defun g (time)
    (when (next bd)
      (bbplay (gethash "kick.ogg" *buffers*)
              :attenuation (pick .8 .5)
              :beat-length (pick .5 -.5)
              )
     (word-play "separate" :attenuation .5 :id 90 :corrupt t)
      )
    (when (next sn)
     (bbplay (gethash "snare.ogg" *buffers*)
              :attenuation .6
              :beat-length .5)
;;      (word-play "undo" :attenuation .5 :corrupt t :id 90)
     )
    (when (next oh)
      (bbplay (gethash "openhat.ogg" *buffers*)
              :attenuation .6

              :beat-length .5)
;;      (word-play "joy" :attenuation .5 :corrupt t)
      )
    ;; (when (next ch)
    ;;   (with-trigger (*trigger*)
    ;;     (bbplay (gethash "hat.ogg" *buffers*)
    ;;             :attenuation .6
    ;;             :beat-length (pick -.5 .5 nil)
    ;;             )))
    (and (odds .9) (bbplay (gethash "0001_cleanC.wav" *buffers*)
                    :rpitch (+ 12 (next c))
                    :attenuation .2
                    :id (next guitar)))
    ;; (when (next h)
    ;;   (let ((buf (gethash "0003_distC.wav" *buffers*)))
    ;;     (with-trigger-expire (*vtrigger* 1.5)
    ;;         (play-lsample-f buf
    ;;                         :dur 4
    ;;                         :loopstart 0 ;;(* .3 (buffer-frames buf))
    ;;                         :loopend   0 ;;(* .7 (buffer-frames buf))
    ;;                         :rate (pitch-to-ratio (next c))
    ;;                         :amp .1
    ;;                         :id 30
    ;;                         ))))
    (aat (+ time #[.25 b])
         #'g it)))

(defun g ())
(g (now))
(incudine:free (node 0))
