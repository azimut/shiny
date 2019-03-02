(in-package #:shiny)

;; https://www.youtube.com/watch?v=k-v1Ibt1pW4
(freverb-toggle)
(freverb-preset 5)
(fg 1f0)
;; PxRand
;; shape
;; mix
;; pshift
;; lpf
(let ((scale (ov-scale :c4 :pentatonic)))
  (defun f (time)
    (fpan 0 (between 0 127))
    (p time (nth (between 0 8) scale) 40 4 0)
    (aat (+ time #[4 b]) #'f it)))

(aat (tempo-sync #[4 b]) #'f it)
(defun f ())

(fx-load "/x/lower/0_kick_drum.wav" 'x)
(fx-load "/x/lower/1_kick_drum.wav" 'x)
(fx-load "/x/lower/2_kick_drum.wav" 'x)
(fx-load "/x/lower/3_kick_drum.wav" 'x)
(fx-load "/_/hyphen/0_hihat_closed.wav" '-)
(fx-load "/_/hyphen/1_hihat_closed.wav" '-)
(fx-load "/_/hyphen/2_hihat_closed.wav" '-)
(fx-load "/_/hyphen/3_hihat_closed.wav" '-)

(fx-load "/d/lower/0_wood.wav" 'd)

(bbplay (fx-buf 'd))

(let ((pat (make-cycle
            (list
             (make-cycle '(x -) 1)
             '-
             (make-weighting '(- x) 1)
             (make-weighting '(x -) 1)
             'd
             '-
             '-
             '-))))
  (defun g (time)
    (bbplay (print (fx-buf (print (next pat)) 0)) :amp .2)
    (aat (+ time #[1 b]) #'g it)))
(aat (tempo-sync #[1 b]) #'g it)
(defun g ())
(setf (bpm *tempo*) 220d0)
