(in-package #:shiny)

;; requires azimut/rocketman
;; load "rsg4.rocket"

(freset)
(setf (bpm *tempo*) 120)
(defun f ())
(progn
  (fg 2f0)
  (fp 0 71)
  (fp 1 70)
  (fp 2 70)
  (fp 3 68)
  ;;
  ;; (fpan 2 120)
  ;; (fpan 3 5)
  ;;(fspread 2 t)
  (fspread 3 nil)
  )

(fp 4 47)

(let ((s (scale 0 'minor))
      (v (vector 0 0))
      (l))
  (defun g (time)
    ;;#+nil
    (and (or (< .99 (incandescent::rocket-get "song:ter.appear") 1.1)
             (odds (incandescent::rocket-get "song:ter.appear")))
         (setf (svref v 1) (beat))
         (p time (setf l (pc-random 60 75 s)) 80 .5 1))
    ;;#+nil
    (and (or (< .9 (incandescent::rocket-get "song:cua.appear") 1.1)
             (odds (incandescent::rocket-get "song:cua.appear")))
         (when (zerop (mod (beat) 2))
           (setf (svref v 0) (beat))
           (p (+ #[.25 b] time)
              (ivar (stutter (make-chord-fixed 72 3 s)) 1)
              70
              1
              3)))
    ;;#+nil
    (and (odds .1)
         (pa time
             (stutter
              (make-chord-fixed (min 84 (pc-quantize (+ 24 (rsinr 60 10 6)) s))
                                3 s))
             80
             .5
             4
             .2))
    ;;#+nil
    (when (or (< .9 (incandescent::rocket-get "song:sec.appear") 1.1)
              (and (odds (incandescent::rocket-get "song:sec.appear"))
                   (= 3.5 (mod (beat) 4f0))))
      (p time 48 40 2 1))
    ;;#+nil
    (setf incandescent::*scene-index* (floor (incandescent::rocket-get "scene")))
    (p time (pc-relative 60 (ivar (list 0 0 (pick 2 2 2 1) 0) .5)
                         s)
       (floor (* (incandescent::rocket-get "song:main.volume")
                 (ivar (transpose '(30 30 35 30)
                                  (if (position (beat) v :test #'=)
                                      0
                                      5))
                       .5)))
       .35 0)
    (setf l nil)
    (aat (+ time #[.5 b]) #'g it)))

(let ((time (tempo-sync #[.5 b])))
  (at time #'incandescent::rocket-pause-toggle)
  (aat time #'g it))
(defun g ())
