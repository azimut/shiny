(in-package :shiny)

(start-csound (gethash :trapped *orcs*))

(bt:make-thread
 (lambda ()
   (loop
      :for frame = (csound:csoundperformksmps *c*)
      :while (= frame 0))))

(csound:csoundcompileorc *c* (get-orc :trapped))
(csound:csoundreadscore *c*  (get-table :trapped))

;; TRAPPED
(make-play ivory  "i1" :p4 0 :keynum 60 :amp 200 :vib .001 :glis 17.8 :drop .99)
(make-play blue   "i2" :p4 0 :keynum 60 :amp 600 :reverb .6 :lfo 23 :harm 10 :sweep .52)
(make-play violet "i3" :p4 0 :keynum 60 :amp 800 :reverb .8 :rand 57)
(make-play black  "i4" :p4 0 :keynum 60 :amp 800 :swp 4600 :eswp 6500 :band 33 :rev 0.6)
(make-play green  "i5" :p4 0 :keynum 60 :amp 900 :rev .2 :pan .1 :carr 3 :modf 10 :modi 12 :rand 27)
(make-play cooper "i6" :p4 .81 :sweeps 3000 :sweepe 17 :band 10 :rev .6 :amp 1.6)
(make-play pewter "i7" :amp 1000 :keynum 60 :bphase .2 :ephase .7 :oamp .6 :oscf 2 :oscm 3 :rev .12)
(make-play red    "i8" :amp 4 :swp 20 :eswp 8000 :band 590 :rand1 2 :rand2 9.9 :rev .6)
(make-play sand   "i9" :delay .2 :keynum 60 :amp 500 :rev .2 :ramp 6.2 :rfreq 320)
(make-play taupe  "i10" :p4 0 :keynum 60 :amp 500 :rev .8 :ramp 5 :rfreq 223)
(make-play rust   "i11" :delay 0 :keynum 60 :amp 1200 :rev .2)
(make-play teal   "i12" :p4 0 :keynum 60 :amp 1000 :swp 100 :pswp 7000 :band 16 :rev .2)
(make-play foam   "i13" :p4 0 :keynum 60 :amp 1000 :vib 40 :glis 7)
(make-play swirl "i99" :pan 2)

(defparameter *expwarp*
  (loop :for n :from 1 :to 3 :by .1 :collect
     (mapcar #'round (cm:expwarp '(36 55 64) n))))

(let ((n (make-heap *expwarp*)))
  (defun f (time)
    (let ((i (reverse (next n))))
      (play-sand (nth 0 i) 4 .2 500)
      (at (+ time #[1 b]) #'play-green (nth 2 i) 1 .2 500)
      (at (+ time #[2 b]) #'(lambda () (play-sand (nth 1 i) 1 .2 500 .9))))
    (aat (+ time #[4 b]) #'f it)))
(defun f ())
(f (now))

(defparameter *m5chds*
  (loop :for n :in (cm::m5 (cm::indices 12)) :collect (cm::transp '(0 3 5 7) n)))

(defparameter *rgrthing*
  (cm::generic-branch #'cm::rgr-alldim1 *m5chds*))

