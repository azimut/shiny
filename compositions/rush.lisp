(in-package :shiny)

(load-csound
 (merge-orcs
  (get-orchestra :ourvoice)
  (get-orchestra :physmodl 0)
  (get-orchestra :phaser)
  (get-orchestra :hallown '(5 3))))

(make-play bass "i2" :amp 1000 :keynum 60 :pluckdur .25 :left 1f0 :right 1f0)
(make-play phaser "i3" :amp 3000 :keynum 60)
(make-play phaser-effect "i4" :change .9)
(make-play phaser-reverb "i5" :p1 .93 :p2 1.2 :p3 1 :p4 7000 :p5 1)
(make-play bell "i6" :amp 1000 :keynum 60 :pan 1f0 :outch1 3 :outch2 4)
(make-play organ "i7" :amp 200 :keynum 60 :pan .5 :outch1 1 :outch2 2)
(play-bell 60 1 :pan .8 :amp 3000)

(play-phaser-effect 180)

(defun g ())
(defun f ())

(let ((pan (make-palindrome (iota 20 :start .1 :step .1)))
      (up  (make-cycle '(70 80 90 90)))
      (lead (make-cycle (ov-scale :c4 :minor))))
  (defun g (time)
    (and (zmodt 2) (funcall
                    (pick #'play-voice-a #'play-voice-e
                          #'play-voice-iy #'play-voice-o)
                    (next lead) 2.2 :amp 4000))
    (and (zmodt 3)
         (destructuring-bind (a b c)
             (make-chord 60 (next up) 3 (scale 0 'minor))
           (and (odds .4)
                (play-voice-a a 1 :amp (rcosr 950 100 4) :left (next pan)))
           (and (odds .5)
                (play-voice-e b 1 :amp (rcosr 950 100 5) :right (next pan)))
           (and (odds .9)
                (play-voice-oo c 1 :amp (rcosr 950 100 3) :left (next pan)))))
    (aat (+ time #[1 b]) #'g it)))


(let ((rhythm (make-weighting '((.5 .9) 1 0)))
      (i      (make-cycle '(V II I IV))))
  (defun f (time)
    (let ((r (next rhythm)))
      (and (zmodt 2)
           (play-bass 30 .2 :amp 100 :pluckdur .1 :left .2))
      (play-organ-arp
       (make-chord 50 60 2 (pc-diatonic 0 '- (next i)))
       (* r (pick .5 .4 .6))
       (* r .5))
      (aat (+ time #[r b]) #'f it))))

(let ((rhythm (make-weighting '((.5 .9) 1 0)))
      (i      (make-cycle '(V II I IV))))
  (defun f (time)
    (let ((r (next rhythm)))
      (and (zmodt 2)
           (play-bass 30 .2 :amp 100 :pluckdur .1 :left .3 :right 1.2))
      (if (odds .9)
          (play-organ-arp
           (make-chord 50 60 2 (pc-diatonic 0 '- (next i)))
           (* r (pick .5 .4 .6))
           (* r .5))
          (play-bell
           (pick 0 (make-chord 60 70 2 (pc-diatonic 0 '- (next i))))
           (+ 1 r)
           :amp 700 :pan (pick .3 .7)))
      (aat (+ time #[r b]) #'f it))))

(f (tempo-sync #[3 b]))
(g (tempo-sync #[1 b]))
