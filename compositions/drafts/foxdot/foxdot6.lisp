(in-package #:shiny)
;;
;; glow.py
;;

(setf *fx-path* "/home/sendai/.local/lib64/python3.4/site-packages/FoxDot/snd/")
(shiny::clean-buffers)
(fx-load-simple "x/lower/0_kick_drum.wav"     "x")
(fx-load-simple "o/lower/0_snare_drum.wav"    "o")
(fx-load-simple "_/hyphen/0_hihat_closed.wav" "-")
(fx-load-simple "_/equals/0_hihat_open.wav"   "=")

;; d1 >> play("x o x - o = x - - = o x ")
(let ((p1 (fx-pat "x o x - o = x - - = o x ")))
  (defun d1 (time)
    (fx-play (next p1) :amp .5)
    (aat (+ time #[1 b]) #'d1 it)))
(aat (tempo-sync #[1 b]) #'d1 it)
(defun d1 ())

;; d2 >> play("--   -- ---")
(let ((p1 (fx-pat "--   -- ---")))
  (defun d2 (time)
    (fx-play (next p1) :amp .5)
    (aat (+ time #[1 b]) #'d2 it)))
(aat (tempo-sync #[1 b]) #'d2 it)
(defun d2 ())

;; b1 >> pluck([0, 3, 2], dur=[1/2, 1], oct=4)
(let ((dur   (make-cycle '(1/2 1)))
      (notes (make-cycle '(0 3 2))))
  (defun b1 (time)
    (let ((d (next dur)))
      (clc 23 (transpose (next notes) 60) 30 d)
      (aat (+ time #[d b]) #'b1 it))))
(aat (tempo-sync #[1/2 b]) #'b1 it)
(defun b1 ())

;; p1 >> pads([0, 3, 7, 8, -2], dur=[4, 8], oct=5, amp=0.7)
(let ((dur   (make-cycle '(4 8)))
      (notes (make-cycle '(0 3 7 8 -2))))
  (defun p1 (time)
    (let ((d (next dur)))
      (clc 23
           (+ -12 (clc 23 (pc-relative (note :c5) (next notes) (scale 0 'minor)) 70 d))
           50 d)
      (aat (+ time #[d b]) #'p1 it))))

(fp 2 100)
(fp 4 9)

(aat (tempo-sync #[4 b]) #'p1 it)
(defun p1 ())
