(in-package :somecepl)

(defun play-midi-note (time pitch velocity dur c)
  (when (and
         (not (equal pitch "r"))
         (> pitch 0)
         (> velocity 0)
         (> dur 0))
    (progn
      (at time #'fluidsynth:noteon
          *synth* c pitch velocity)
      (at (+ time #[dur b]) #'fluidsynth:noteoff
          *synth* c pitch))))


;;; Second-order Markov process. The transition table is taken 
;;; from chapter 8 of "Computer Music" by Dodge/Jerse. Sounds
;;; best with slow strings.

(defparameter *m-foster*
  (new cm:markov
    :of '((b3  d4  :-> d4)
          (cs4 d4  :-> (d4 .3125) (e4 .3125) (a4 .3125))
          (d4  d4  :-> (cs4 .125) (d4 .125) (e4 .5625) (fs4 .125)
           (a4 .0625))
          (e4  d4  :-> (b3 .0625) (d4 .0625) (e4 .25) (fs4 .3125)
           (a4 .0625) (cs5 .0625) (d5 .1875))
          (fs4 d4  :-> (e4 .75) (fs4 .1875) (g4 .0625))
          (a4  d4  :-> (e4 .6875) (fs4 .3125))
          (b4  d4  :-> d4)
          (d4  b3  :-> d4)
          (d4  cs4 :-> d4)
          (e4  cs4 :-> d4)
          (d4  e4  :-> (d4 .1875) (e4 .25) (fs4 .5) (a4 .0625))
          (e4  e4  :-> (cs4 .0625) (d4 .75) (e4 .0625) (fs4 .125))
          (fs4 e4  :-> (cs4 .125) (d4 .4375) (e4 .1875) (fs4 .125)
           (a4 .0625) (d5 .0625))
          (d4  fs4 :-> (e4 .4375) (fs4 .1875) (g4 .125) (a4 .25))
          (e4  fs4 :-> (d4 .0625) (e4 .1875) (fs4 .3125) (g4 .25)
           (a4 .0625) (b4 .0625)) 
          (fs4 fs4 :-> (d4 .1875) (e4 .25) (fs4 .3125) (g4 .125)
           (a4 .0625))
          (g4  fs4 :-> (e4 .5) (g4 .5))
          (a4  fs4 :-> (d4 .3125) (e4 .25) (fs4 .1875) (g4 .0625)
           (a4 .125) (b4 .0625))
          (b4  fs4 :-> (e4 .6875) (fs4 .3125))
          (d4  g4  :-> (fs4 .6875) (b4 .3125))
          (fs4 g4  :-> (fs4 .25) (g4 .1875) (a4 .3125) (b4 .1875))
          (g4  g4  :-> (g4 .5) (a4 .5))
          (a4  g4  :-> fs4)
          (b4  g4  :-> b4)
          (a4  gs4 :-> a4)
          (d4  a4  :-> (fs4 .25) (a4 .75))
          (e4  a4  :-> (a4 .8125) (b4 .1875))
          (fs4 a4  :-> (fs4 .125) (a4 .625) (b4 .1875) (d5 .0625))
          (g4  a4  :-> (d4 .125) (a4 .625) (d5 .25))
          (gs4 a4  :-> a4)
          (a4  a4  :-> (fs4 .25) (g4 .0625) (gs4 .0625)
           (a4 .3125) (b4 .3125)) 
          (b4  a4  :-> (d4 .0625) (fs4 .5625) (g4 .0625) (a4 .125)
           (b4 .0625) (d5 .125))
          (d5  a4  :-> (fs4 .875) (a4 .125))
          (e5  a4  :-> a4)
          (fs4 b4  :-> a4)
          (g4  b4  :-> a4)
          (a4  b4  :-> (d4 .0625) (fs4 .0625) (a4 .75) (b4 .0625)
           (b4 .0625))
          (b4  b4  :-> (fs4 .125) (a4 .75) (d5 .125))
          (cs5 b4  :-> a4)
          (d5  b4  :-> (g4 .0625) (a4 .3125) (b4 .3125) (d5 .25))
          (d4  cs5 :-> d5)
          (d5  cs5 :-> (b4 .75) (d5 .25)) 
          (e5  cs5 :-> d5)
          (d4  d5  :-> (a4 .125) (b4 .6875) (cs5 .1875) )
          (e4  d5  :-> cs5)
          (a4  d5  :-> (a4 .3125) (b4 .3125) (cs5 .1875) (d5 .125))
          (b4  d5  :-> (a4 .5625) (b4 .125) (cs5 .3125))
          (cs5 d5  :-> (b4 .3125) (e5 .625))
          (d5  d5  :-> b4)
          (d5  e5  :-> (a4 .3125) (cs5 .6875)))))

;; p2 is rhythmic pattern that randomly selects
;; between several rhythmic motives that are
;; characterisitic of Foster's sytle
(defparameter *r-foster*
  (make-weighting
   `((,(make-cycle '(h h)) :weight .375)
     (,(make-cycle '(q q q q)) :weight .125)
     (,(make-cycle '(h q q)) :weight .125)
     (,(make-cycle '(q q h)) :weight .125)
     (,(make-cycle '(q h q)) :weight .25)
     (w :weight .125))))

(defun foster (time &optional notes rhythms root chan)
  (let ((rhythm (cm:rhythm (cm:next rhythms) 120)))
    (p time (cm:keynum (cm:transpose (cm:next notes) root)) 30 rhythm chan)
    (aat (+ time rhythm)
         #'foster
         it notes rhythms root chan)))

(fp 1 20)
(fp 2 30)
(fp 3 40)

(defun foster ())

(foster (quant 4) *m-foster* *r-foster* -12 0)
(foster (quant 12) *m-foster* *r-foster*   0 1)
(foster (quant 24) *m-foster* *r-foster*  12 2)
(foster (quant 36) *m-foster* *r-foster*  24 3)

(all-piano *synth*)
(flush-pending)
(off-with-the-notes *synth*)

(fluidsynth:program-change *synth* 1 *nk-oboe*)

(fluidsynth:program-change *synth* 2 77)
(fluidsynth:program-change *synth* 2 38)

(fluidsynth:program-change *synth* 0 34)
(fluidsynth:program-change *synth* 3 77)
