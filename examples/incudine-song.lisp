(in-package :somecepl)

; (cl-patterns::scale :MINOR)

(defvar scl-arch_sept '(28/27 16/15 9/8 32/27 4/3 112/81 3/2 14/9 8/5 27/16 16/9 2/1 ))

(defvar env2 (make-perc .001 .4))

(setf (bpm *tempo*) 120)

; ----------------

(dsp! env-test-3 (freq amp pos (env envelope) gate)
  (foreach-channel
    (cout (pan2 (* (envelope env gate 1 #'stop)
                   (sine freq amp 0))
                pos))))

; ----------------

(defun seq-test (rep freq amp pos scale)
  (when (plusp rep)
    (dsp-seq (env-test-3 freq amp pos env2 1)
             (env-test-3 (* freq (nth 5 scale)) amp pos env2 1)
             (env-test-3 (* 2 freq) amp pos env2 1)
             (seq-test (1- rep) freq amp pos scale))))

(defun seq-test (rep freq amp pos scale)
  (when (plusp rep)
    (print "running")
    (dsp-seq (env-test-3 freq amp pos env2 1)
             (env-test-3 (* freq (scl-nth (random (length scale)) scale)) amp pos env2 1)
             (env-test-3 (* freq (scl-nth (random (length scale)) scale)) amp pos env2 1)
             (env-test-3 (* freq (scl-nth (random (length scale)) scale)) amp pos env2 1)
             (seq-test (1- rep) freq amp pos scale))))

(defun seq-test (rep freq amp pos scale)
  (when (plusp rep)
    (dsp-seq (env-test-3 freq amp pos env2 1)
             (env-test-3 (* freq (nth 1 scale)) amp pos env2 1)
             (env-test-3 (* freq (nth 2 scale)) amp pos env2 1)
             (env-test-3 (* freq (nth 3 scale)) amp pos env2 1)
             (env-test-3 (* freq (nth 4 scale)) amp pos env2 1)
             (env-test-3 (* freq (nth 5 scale)) amp pos env2 1)
             (env-test-3 (* freq (nth 6 scale)) amp pos env2 1)
             (env-test-3 (* freq (nth 6 scale)) amp pos env2 1)
             (env-test-3 (* freq (nth 7 scale)) amp pos env2 1)
             (env-test-3 (* freq (nth 8 scale)) amp pos env2 1)
             (env-test-3 (* freq (nth 9 scale)) amp pos env2 1)
             (env-test-3 (* freq (nth 10 scale)) amp pos env2 1)
             (seq-test (1- rep) freq amp pos scale))))

(defun seq-test (rep freq amp pos)
  (when (plusp rep)
    (dsp-seq (env-test-3 freq amp pos env2 1)
             (env-test-3 (* freq (nth 5 scl-arch_sept)) amp pos env2 1)
             (env-test-3 (* freq (nth 7 scl-arch_sept)) amp pos env2 1)
             (seq-test (1- rep) freq amp pos))))

(defun seq-test (rep freq amp pos)
  (when (plusp rep)
    (dsp-seq (env-test-3 freq amp pos env2 1)
             (env-test-3 (* freq 7/4) amp pos env2 1)
             (env-test-3 (* freq 2) amp pos env2 1)
             (seq-test (1- rep) freq amp pos))))

; ----------------

(defun phr1 (time scale)
  (at time #'seq-test 8 200 .4 .5 scale)
  (at (+ time #[2 b]) #'seq-test 6 400 .4 .4 scale)
  (at (+ time #[4 b]) #'seq-test 4 600 .4 .6 scale))

(defun phr1 (time)
  (at time #'seq-test 8 200 .4 .5)
  (at (+ time #[2 b]) #'seq-test 6 400 .4 .4)
  (at (+ time #[4 b]) #'seq-test 4 600 .4 .6))
