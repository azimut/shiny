(in-package :shiny)

(fg 2f0)

(freverb-toggle 1)
(freverb-preset 6)

(fp 0 40)
(fp 1 0)

(setf *instances* 1)

(defun pat1 ())
(defpattern pat1 ((get-pattern 'blood) .5)
  (p time *gm-kick* (rcosr 60 5 3) .35 1)
  (p time *gm-snare* (rcosr 60 5 3) .35 1)
  (p time *gm-closed-hi-hat* (rcosr 60 5 3) .35 1)
  (p time *gm-open-hi-hat* (rcosr 65 5 3) .35 1))

(pat1 (tempo-sync #[4 b]))

(defun f ())
(off-with-the-notes)
(bbuffer-load "/home/sendai/Downloads/sample/Dirt-Samples-master/click/000_click0.wav")
(bbuffer-load "/home/sendai/Downloads/sample/Dirt-Samples-master/click/001_click1.wav")
(bbuffer-load "/home/sendai/Downloads/sample/EM0505.wav")
(bbuffer-load "/home/sendai/Downloads/sample/EM0902-EM0902.wav")

(put-phrase "fear" "EM0505.wav" 1 9)
(put-phrase "afraid" "EM0505.wav" 10 4.2)
(put-phrase "existence" "EM0505.wav" 14.2 5.8)
(put-phrase "memories" "EM0902-EM0902.wav" 1 6)
(put-phrase "ffear" "EM0902-EM0902.wav" 8 3)
(put-phrase "bye" "EM0902-EM0902.wav" 17 18)

(word-play "memories" :id 20 :attenuation .5)

(incudine:free (node 0))
(defun f ())
(defun pat1 ())

(let ((i (make-heap '(180 100 140 50 180 100 140 1)))
      (c (make-cycle (reverse (make-chord-fixed 60 3 (scale 0 'lydian))))))
  (defun f (time)
    (setf *instances* (next i))
    (when (and (odds .1) (not (node-alive 40)))
      (word-play (pickl (list-words)) :id 40 :attenuation .1
                 :rate (pick .9 .8)))
    
    (bbplay (pick "000_click0.wav" "001_click1.wav")
            :rate (pick 1 -1))
    (let ((n (next c)))
      (p time n 50 2 0)
      (with-trigger-expire (*trigger* 1)
        (p time (+ 12 n) 70 (pick .25 .1 .5) 0))
      )
    (aat (+ time #[2 b]) #'f it)))

(f (tempo-sync #[2 b]))

(setf (clear-color) (v! .2 .2 1 0))

(fp 2 52)

