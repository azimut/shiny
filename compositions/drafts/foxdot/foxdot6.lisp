(in-package #:shiny)
;;
;; glow.py
;;

(setf *fx-path* "/home/sendai/.local/lib64/python3.4/site-packages/FoxDot/snd/")

(fx-load-simple "x/lower/0_kick_drum.wav"     "x")
(fx-load-simple "o/lower/0_snare_drum.wav"    "o")
(fx-load-simple "_/hyphen/0_hihat_closed.wav" "-")
(fx-load-simple "_/equals/0_hihat_open.wav"   "=")

(let ((p1 (fx-pat "x o x - o = x - - = o x ")))
  (defun d1 (time)
    (bbplay (next p1) :amp .5)
    (aat (+ time #[1 b]) #'d1 it)))

(aat (tempo-sync #[1 b]) #'d1 it)
(defun d1 ())


(let ((p1 (fx-pat "--   -- ---")))
  (defun d2 (time)
    (bbplay (next p1) :amp .5)
    (aat (+ time #[1 b]) #'d2 it)))

(aat (tempo-sync #[1 b]) #'d2 it)
(defun d2 ())

(let ((dur (cm:new cm:cycle :of '(1/2 1)))
      (scale (ov-scale :c4 :minor))
      (notes (cm:new cm:cycle :of '(0 3 2))))
  (defun b1 (time)
    (let ((d (next dur)))
      (p time (nth (next notes) scale) 80 d 0)
      (p time (nth (next notes) scale) 80 d 1)
      (aat (+ time #[d b]) #'b1 it))))

;; spread
(let ((pg 0))
  (fspread 0 t)
  (fspread 1 nil)
  (fp 1 pg)
  (fp 0 pg))

(aat (tempo-sync #[1/2 b]) #'b1 it)
(defun b1 ())

(let ((dur (cm:new cm:cycle :of '(4 8)))
      (notes (cm:new cm:cycle :of '(0 3 7 8 -2))))
  (defun p1 (time)
    (let ((d (next dur)))
      (p time
         (+ -12 (p time (pc-relative (note :c5) (next notes) (scale 0 'minor)) 70 d 2))
         50 d 4)
      (aat (+ time #[d b]) #'p1 it))))

(fp 2 100)
(fp 4 9)

(aat (tempo-sync #[4 b]) #'p1 it)
(defun p1 ())
