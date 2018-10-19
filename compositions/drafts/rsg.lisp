(in-package :shiny)

;; "silverline"

;; "Each major scale has a relative minor scale, and you
;;  can determine the relative minor by using the major
;;  scale's sixth note"

(defvar *maj* (ov-scale :C4 :major))
(defvar *min* (ov-scale :A4 :minor))

(make-orc :ourvoice
          :filename "ourvoice"
          :filepath "/home/sendai/projects/csound-instruments/pinkston/")

(start-csound (get-orchestra :ourvoice))
(start-thread)

(make-play voice-a "i1"
           :amp 10000 :keynum 60 :fmt1 609 :db1 0 :fmt2 1000 :db2 -6
           :fmt3 2450 :db3 -12 :fmt4 2700 :db4 -11 :fmt5 3240 :db5 -24
           :left 1f0 :right 1f0)

(fg 1f0)
(freverb-toggle 1)
(freverb-preset 5)

(fp 0 0)
(fp 1 1)
(fp 2 0)
(fp 3 0)

(viseq::wait-key 100)
(viseq:push-cvideo
 :la "/home/sendai/clips/lali-machines.mp4"
 :stop-pos 180
 :scale .01f0
 :glitch nil
 :is-negative nil)

(viseq:push-cvideo
 :car "/home/sendai/clips/lali-machines.mp4"
 :is-visible nil
 :repeat 2
 :stop-pos (* 25 77)
 :pos (* 25 75)
 :restart-pos (* 25 75)
 :scale 1f0
 :is-negative nil)

(f (tempo-sync #[1 b]))
(defun f ())
(defparameter *appear-video*
  (make-line (append (iota 20 :start .01 :step .05) '(1f0))))
(defparameter *appear-vol*
  (make-line (iota 15 :start 10 :step 3)))
(let* ((maj (make-weighting *maj* 3))
       (min (make-weighting *min* 3))
       (chan (make-cycle '(0 1)))
       (notes (make-cycle (list maj min))))
  (defun f (time)
    (viseq:push-cvideo :la "/home/sendai/clips/lali-machines.mp4"
           :scale (next *appear-video*) :xpos 50 :ypos 50)
    (let ((v (next *appear-vol*)))
      (pa time (next notes 3) .3333 (rcosr v 3 4) (next chan) .3333)
      (pa (+ time #[.15 b])
          (next notes 3)
          '(0 .33333)
          (list v (+ v 10))
          3
          .2))
    (aat (+ time #[1 b])
         #'f it)))

(defparameter *appear* (make-line (iota 50 :start 50 :step -.5)))
(defparameter *oddity* .01)
(let* ((maj (make-weighting *maj* 3))
       (min (make-weighting *min* 3))
       (chan (make-cycle '(0 1)))
       (voice (make-line (iota 15 :start .2 :step .02)))
       (oddity (make-weighting (list .1 (list 1 (pval *oddity*)))))
       (notes (make-cycle (list maj min)))
       (voi (make-cycle *min*))
       (vis (make-weighting (list '(nil 1) (list t (pval *oddity*))))))
  (defun f (time)
    (incf *oddity* .05)
    (pa time (next notes 3) .3333 50 (next chan) .3333)
    (pa (+ time #[.15 b])
          (next notes 3)
          '(0 .3333)
          50
          3
          .2)
    (let ((ttime (+ time #[(pick .666 .333 .333 .333) b])))
      (if (next vis)
          (progn
            (at ttime (lambda () (viseq:push-cvideo
                          :car "/home/sendai/clips/lali-machines.mp4"
                          :is-visible t
                          :repeat 2
                          :flip (pick -2 1)
                          :stop-pos (* 25 82)
                          ;;                        :pos (* 25 80)
                          :restart-pos (* 25 70)
                          :scale 1f0
                          :is-negative (pick nil t))))
            (at (+ time #[(pick .3333 .66666) b])
                (lambda () (play-voice-a
                       (let ((v (next voi)))
                         (list v (+ v -12) (+ v -24)))
                       .66666 :left (random 1f0) :right (random 1f0) :amp (* (next *appear*) 500)
                       ))))
          (viseq:push-cvideo
                          :car "/home/sendai/clips/lali-machines.mp4"
                          :is-visible nil))
      
      (pa ttime
          (if (odds (next oddity))
              (reverse (next notes 3))
              (next notes 3))
          '(.3333)
          (round (next *appear*))
          2
          1))
    (aat (+ time #[1 b])
         #'f it)))

(bbuffer-load "/home/sendai/quicklisp/local-projects/aubio/static/au")

(f (now))
(defun f ())
