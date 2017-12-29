(in-package :somecepl)

;; https://sourceforge.net/p/incudine/mailman/message/34320609/
;; https://ccrma.stanford.edu/realsimple/faust_strings/faust_strings.pdf

(define-vug diffgtz (x)
  (if (plusp (- x (delay1 x)))
      1.0d0
      0.0d0))

(define-vug decay (n x)
  (- x (if (plusp x)
           (/ n)
           0.0d0)))

(define-vug release (n x)
  (~ (+ x (decay n it))))

(define-vug trigger (n x)
  (if (plusp (release n (diffgtz x)))
      1.0d0
      0.0d0))

(define-vug pickdir (in coef)
  (with-samples ((g (- 1 coef)))
    (pole (* g in) coef)))

(define-vug pickpos (in beta p)
  (with ((ppdel (sample->fixnum (* beta p))))
    (- (delay-s in 2048 ppdel) in)))

(define-vug symmetric-twozero (in h0 h1)
  (with-samples ((del1 (delay1 in)))
    (+ (* h0 del1) (* h1 (+ in (delay1 del1))))))

(define-vug damping-filter2 (in freq t60 b)
  (with-samples ((k (/ (* freq t60)))
                 (rho (expt .001d0 k))
                 (h0 (* (+ 1 b) 0.5))
                 (h1 (* (- 1 b) 0.25)))
    (* rho (symmetric-twozero in h0 h1))))

(define-vug level-filter (x l freq)
  (with-samples ((l0 (expt (the non-negative-sample l) (incudine.util:sample 1/3)))
                 (lw (* pi freq *sample-duration*))
                 (lgain (/ lw (+ 1 lw)))
                 (lpole2 (/ (- 1 lw) (+ 1 lw)))
                 (s0 (* l l0 x))
                 (s1 (- 1 l)))
    (+ s0 (* s1 (~ (+ (* x lgain) (* lpole2 it)))))))

(defvar *env1* nil)
(setf *env1* (make-perc 0 .25))

;; (define-vug excitation (gain p)
;;    (* (white-noise gain) (trigger p (incudine.util:sample (incudine.vug:mouse-button)))))
;; (define-vug excitation (gain p)
;;   (* (white-noise gain)
;;      (trigger p (incudine.util:sample 1.))))
(define-vug excitation (gain p)
  (* (white-noise gain)
     (cond ((eq (trigger p (incudine.util:sample 1.)) 1.) 1) (t (envelope *env1* 1 1. #'incudine:stop)))))
;; (define-vug excitation (gain p)
;;   (* (white-noise gain)
;;      (envelope *env1* 1 1. #'incudine:stop)))
;;  ;    (trigger p (incudine.util:sample (incudine.vug:mouse-button)))))
;;(defvar *env1* (make-envelope '(0 1 0) '(.2 .8)))
;; (setf *env1* (make-envelope '(0 1 1 0) '( .1 .3 .31) :curve :step))
;; (setf *env1* (make-envelope '(0 1 0) '(.39999 .3999999 )))
;; (setf *env1* (make-envelope '(0 1 0) '(.1 .5) :curve :step))
;; (setf *env1* (make-envelope '(0 1 0) '(.1 .11) :curve :step))

(define-vug filtered-excitation (gain freq pickangle beta l)
  (with-samples ((p (/ *sample-rate* freq)))
    (level-filter (pickpos (pickdir (excitation gain p) pickangle) beta p)
                  l freq)))

(define-vug stringloop (in freq t60 b)
  (with-samples ((dt (/ freq)))
    (~ (damping-filter2 (vdelay (+ in it) 0.05 dt :cubic) freq t60 b))))

(dsp! pluck-test (gain freq pickangle beta l t60 b)
  (foreach-frame
    (stereo
     (stringloop
      (filtered-excitation gain freq pickangle beta l)
      freq t60 b))))

(set-rt-block-size 64)

(rt-start)

;; -----------------------

(defvar *myscale* nil)
(setf *myscale* '(0 2 3 5 7 8 10) )
(setf *myscale* '(0 1 3 5 6 8 10) )
(setf *myscale* (scale 0 'phrygian))

(pluck-test .5 440 .7 .1 .3 4 .5)

(defun right ()
  ;;  (play-lsample-f (qcosr *myscale* 60 7 3/4) 1. .2)
  ;; (pluck-test .7 440 .7 .1 .3 4 .5)
  (pluck-test .01
              (midihz (qcosr *myscale* 60 7 3/4))
              (cosr .7 .2 .3) .1 .3 4 .5)
  (at (+ (now) #[.6 b]) #'right)
  )

(right)
;; ***** Press the left button of the mouse to trigger a note *****

(flush-pending)
(incudine:free 0)
(rt-stop)
