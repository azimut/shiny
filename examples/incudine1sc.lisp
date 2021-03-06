(in-package :shiny)
(defparameter s1env (make-envelope '(0 1 0) '(.2 1)))

(define-vug s1 (f a e g d q)
  (* a
     (envgen e g d #'incudine:free)
     (bpf (+ (- (phasor f (coerce (random 1.) 'double-float)) .5)
             (- (phasor f (coerce (random 1.) 'double-float)) .5))
          f
          q)))

(dsp! synth1 (f a (e envelope) g d q)
  (let ((s (s1 f a e g d q)))
    (out s s)))


(defparameter minor #(0 2 3 5 7 8 10 12))
(defun minor (n)
  (+ n (aref minor (random (length minor)))))

(defun xx (n b a time)
  (let ((b2 (1+ (random b))))
    (synth1 (minor n)
            (+ .9 (random a))
            s1env
            .3
            b2
            7)
    (at (+ time #[b2 beat])
        #'xx n b a (+ time #[b2 beat]))))

(defun incudine1 ()
  (setf (bpm *tempo*) 120)
  (xx 200 8 .5 (now))
  (at (+ (now) #[4 beat])
      #'xx 260 8 .5 (+ (now) #[4 beat])))

(incudine1)
(flush-pending)
(incudine:free 0)
