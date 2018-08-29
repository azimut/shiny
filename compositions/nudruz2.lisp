(in-package :shiny)

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

(defvar *notes* nil)

(setf
 *notes*
  (cm::transp (cm::tzrandchain '(5 15 21) 200) 50)
 ;; (cm::transp (cm::tzrandchain '(0 3 4) 20) 50)
 ;; (cm::fromto '(61 72 75) '(50 65 75))
 ;;(cm::smoothlist (cm::entropy (cm::transp '(50 36 57) 12)))
  ;;(cm::smoothlist (cm::entropy (cm::transp '(50 36 57 61) 12)))
 ;;(cm::transp (mapcar (lambda (x) (car (cm::stack-by x 7))) (cm::flatter (mapcar (lambda (x) (cm::stravrot x 'n)) (cm::subsequences (cm::randhexrow) 6)))) 45)
 )

(fg 1f0)

(defun f ())
(let ((notes (make-cycle (cm::entropy (cm::transp '(50 36 57) 12)))))
  (defun f (time)
    (let ((n (next notes)))
      (green (midihz (first n)) .5)
      (when (second n)
        (at (+ time #[.5 b]) #'green (midihz (second n)) .5))
      (when (third n)
        (at (+ time #[1 b]) #'green (midihz (third n)) .5)))
    (aat (+ time #[1.5 b]) #'f it)))

(f (now))

(fp 0 28)

(let ((notes  (make-cycle *notes*))
      (rhythm (make-cycle (cm::code->durs (cm::resclassvec 3 5 7))))
      (incident (make-cycle '(t nil))))
  (defun f (time)
    (let* ((r (next rhythm))
           (n (next notes))
           (l 4)
           (n (subseq n 0 (min l (length n)))))
      (p time (nth-inc 0 24 n) 60 r 1 :pan (pick 0 127))
      (when (next incident)
        (pa time n '(0 .25) 60 2 '(.25 1))
        (pa (+ time #[1 b])
            (pick (cm::transp n 12) n)
            '(0 .25) 60 2 '(.25 1)))
      (aat (+ time #[r b]) #'f it))))

(fp 2 35)
(fp 3 50)

(freverb-toggle 1)
(freverb-preset 5)
(fchorus-toggle 1)

(defun f ())

(fp 0 20)
;;?
(let ((rhythms (make-cycle (cm::ferney '(7) (cm::randvec 50 3 3))))
      (notes (make-cycle
              (loop for n in (shuffle '(1 2 3 4 5))
                 append (cm::extract-intervals (cm::transp (cm::stack-up cm::row-34) 24) n)))))
  (defun f (time)
    (let ((d (* .25 (next rhythms))))
      (p time (next notes) 40 d 0)
      (aat (+ time #[d b]) #'f it))))

;; ***

(let ((rhythms (make-cycle '(.125)))
      (notes (make-cycle
              (cm::chooser
               (cm::mod12  (cm::drunkvec 5 100))
               (cm::transp (cm::stack-up cm::row-34) 24))))
      (pan   (make-weighting '(0 127))))
  (defun f (time)
    (let ((d (next rhythms)))
      (p time (next notes) 40 d 0)
      ;; (if (zmodt 20)
      ;;     (p time (pickl '(24 28 37 46 50 59 68 77 81 90 99 103))
      ;;        60 1 1 :pan (next pan)))
      (aat (+ time #[d b]) #'f it))))

(defun f ())

(fg 1f0)
(f (now))
(fp 0 10)

(let ((notes (make-cycle
              (cm::transpose
               (shuffle
                (cm::diachrom-filt
                 (cm::subsets-len (cm::indices 12) 4) 2/4)) 60))))
  (defun f (time)
    (pa time (subseq (next notes) 3)
        (list 0 .1 (pick .1 .15 .15 .15) .3)
        '(50 50 60 50)
        0
        '(.1 .1 .3 .3) :pan 63)
    (aat (+ time #[.7 b]) #'f it)))
(defun f ())
(setf (bpm *tempo*) 20)

(defun k ())
(defpattern k ('("x---" "xxxx----" "----x-------xx--" "---x") .25)
  (p time *gm-kick* 20 d 10)
  (p time *gm-snare* 20 d 10)
  (p time *gm-closed-hi-hat* 20 d 10)
;;  (play-instrument 'drum *gm-open-hi-hat* :dur d :amp .02)
  )

(k (tempo-sync #[.7 b]))
(fp 0 23)
(f (now))

(setf (bpm *tempo*) 90)

(let ((notes (make-cycle *notes*)))
  (defun f (time)
    (let ((n (next notes)))
      (pa time n 1 60 2 1))
    (aat (+ time #[3 b]) #'f it)))
(defun f ())

(f (now))

(let ((rhythm (parse-patternc (cm:flatter (cm:fromto-stepper '(0 1 1 0 1 0 0 0) '(1 0 0 1 0 1 0 0))))))
  (defun f (time)
    (when (next rhythm)
      (p time 60 60 1 2))
    (aat (+ time #[1 b])
         #'f it)))
