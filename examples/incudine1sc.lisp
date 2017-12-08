(in-package :somecepl)
(defparameter s1env (make-envelope '(0 1 0) '(.2 1)))

(define-vug s1 (f a e g d q)
  (* a
     (envgen e g d #'free)
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

(defun x (n b a time)
  (let ((b2 (1+ (random b))))
    (synth1 (minor n)
            (+ .9 (random a))
            s1env
            .3
            b2
            7)
    (at (+ time #[b2 beat])
        #'x n b a (+ time #[b2 beat]))))

(defun incudine1 ()
  (setf (bpm *tempo*) 120)
  (x 8 8 .5 (now))
  (at (+ (now) #[8 beat])
      #'x 30 8 .5 (+ (now) #[8 beat]))))
