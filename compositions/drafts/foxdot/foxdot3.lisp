(in-package #:shiny)
;; Scuffer version of "ghost-in-the-machine.py"
;; From "Qirky/ten-lines-or-less"
;;--------------------------------------------------
;; Scale.default="minor"; Clock.bpm=120
(setf (bpm *tempo*) 120f0)
;;--------------------------------------------------
;; d1 >> play("G(:-)", rate=-1/2,
;;                     pshift=var([0,3],[6,2])+(0.1,0),
;;                     pan=(-1,1),
;;                     room=1,
;;                     amp=2)
(bbuffer-load "/home/sendai/projects/FoxDot/FoxDot/snd/g/upper/0_Stab.wav" 'G)
(bbuffer-load "/home/sendai/projects/FoxDot/FoxDot/snd/_/colon/hh01.wav" 'C)
(bbuffer-load "/home/sendai/projects/FoxDot/FoxDot/snd/_/hyphen/0_hihat_closed.wav" '-)
(let ((pan (make-cycle '(0f0 1f0)))
      (shift (make-cycle
              (make-var '(6 2) '(0 3))))
      (notes (make-cycle
              (make-var 1 '(G (C -))))))
  (defun d1 (time)
    (bbplay (next notes) :amp .3 :rate -.5 :pan (next pan) :rpitch (next shift))
    (aat (+ time #[1 b]) #'d1 it)))
(aat (tempo-sync #[4 b]) #'d1 it)
(defun d1 ())
;;--------------------------------------------------
;; d2 >> play("x-", sample=2).sometimes("stutter", 4, dur=3)
(defun d2 ())
(bbuffer-load "/home/sendai/projects/FoxDot/FoxDot/snd/x/lower/2_kick_drum.wav" 'x)
(bbuffer-load "/home/sendai/projects/FoxDot/FoxDot/snd/_/hyphen/2_hihat_closed.wav" '--)
(let ((notes (make-cycle
              (list
               (make-cycle '(x --) (make-weighting '(4 5 6 7 8 9 10 12)))
               (make-cycle '(x --) 4)))))
  (defun d2 (time)
    (bbplay (next notes) :amp .5)
    (aat (+ time #[1 b]) #'d2 it)))
(aat (tempo-sync #[4 b]) #'d2 it)
;;--------------------------------------------------
;; d3 >> play("  I ", sample=2, hpf=(0,2000), lpf=(300,0), hpr=0.5)
(bbuffer-load "/home/sendai/projects/FoxDot/FoxDot/snd/i/upper/2_rock_snare.wav" 'II)
(defun d3 ())
(let ((notes (make-cycle '(NIL NIL II NIL)))
      (lpf (make-cycle '(300 0)))
      (hpf (make-cycle '(0 2000))))
  (defun d3 (time)
    (when-let ((n (next notes)))
      (bbplay n :amp .5 :lpf (next lpf) :hpf (next hpf) :lpr .5 :hpr 1))
   (aat (+ time #[1 b]) #'d3 it)))
(aat (tempo-sync #[4 b]) #'d3 it)
;;--------------------------------------------------
;; b1 >> dbass(var([0,6,5,2],[6,2]),
;;                dur=PDur(3,8,[0,2]),
;;                sus=2,
;;                chop=4,
;;                rate=4)
(defun b1 ())
(let ((scale (ov-scale :C5 :minor))
      (dur (make-cycle
            (list (make-cycle (pdur 3 8 0))
                  (make-cycle (pdur 3 8 2)))))
      (notes (make-cycle
              (make-var '(6 2 6 2) '(0 6 5 2)))))
  (defun b1 (time)
    (let ((d (next dur)))
      (p time (nth (next notes) scale) 30 d 0)
      (aat (+ time #[d b]) #'b1 it))))
(aat (tempo-sync #[4 b]) #'b1 it)
;;--------------------------------------------------
;; p2 >> blip([0,1,[[3,4],2]], dur=[4,3,1],
;;                             drive=PWhite(0.2,0.7),
;;                             oct=6,
;;                             lpf=2000,
;;                             room=1/2,
;;                             echo=0.75,
;;                             echotime=6,
;;                             sus=1).penta().spread()
(let* ((dur   (make-cycle '(4 3 1)))
       (scale (ov-scale :C6 :pentatonic))
       ;; NOTE: cannot use (make-var) as this is a sub-sub pattern
       (notes (make-cycle
               (list 0 1 (make-cycle
                          (list (make-cycle '(3 4) 1)
                                2)
                          1)))))
  (defun p2 (time)
    (let ((d (next dur)))
      (p time (print (nth (next notes) scale)) 40 d 10)
      (aat (+ time #[d b]) #'p2 it))))
(aat (tempo-sync #[4 b]) #'p2 it)
(fp 10 10)
(defun p2 ())
;;--------------------------------------------------
;;k1 >> klank(oct=5, lpf=200, lpr=0.5)
(defun k1 ())
(incudine:free (node 25))
(let* ((scale (ov-scale :C5 :minor))
       (notes (nths '(0 1 2 3) scale))
       (midinotes (mapcar #'midihz notes)))
  (defun k1 (time)
    (destructuring-bind (a b c d) midinotes
      (meniere::dsp-klank a b c d  :amp .001 :lpf 200 :lpr .5 :id 100))
    ;;(aat (+ time #[1 b]) #'k1 it)
    ))
(aat (tempo-sync #[4 b]) #'k1 it)
(set-control 100 :amp .002)
(incudine:free (node 100))
