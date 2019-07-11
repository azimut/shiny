(in-package #:shiny)
;; "ghost-in-the-machine.lisp"
;; scuffer version of FoxDot's song "ghost-in-the-machine.py"[1]
;; using lisp's "Common Music" patterns. And some custom helpers[2].
;;
;; [1]: github.com/Qirky/ten-lines-or-less
;; [2]: github.com/azimut/shiny
;;--------------------------------------------------
;; Scale.default="minor"; Clock.bpm=120
(setf (bpm *tempo*) 120f0)
;;--------------------------------------------------
;; d1 >> play("G(:-)", rate=-1/2,
;;                     pshift=var([0,3],[6,2])+(0.1,0),
;;                     pan=(-1,1),
;;                     room=1,
;;                     amp=2)
(bbuffer-load "/home/sendai/.local/lib64/python3.4/site-packages/FoxDot/snd/g/upper/0_Stab.wav" 'G)
(bbuffer-load "/home/sendai/.local/lib64/python3.4/site-packages/FoxDot/snd/_/colon/hh01.wav" 'C)
(bbuffer-load "/home/sendai/.local/lib64/python3.4/site-packages/FoxDot/snd/_/hyphen/0_hihat_closed.wav" '-)
;; TODO: pshift, room
(let ((notes (fx-pat "G(:-)")))
  (defun d1 (time)
    (fx-play (next notes)
             :rate -.5
             :rpitch (ivar '(0 3) '(6 2))
             :pan    (ivar '(.1 .9) 2)
             :amp .3)
    (aat (+ time #[1 b]) #'d1 it)))
(aat (tempo-sync #[1 b]) #'d1 it)
(defun d1 ())
;;--------------------------------------------------
;; d2 >> play("x-", sample=2).sometimes("stutter", 4, dur=3)
(bbuffer-load "/home/sendai/projects/FoxDot/FoxDot/snd/x/lower/2_kick_drum.wav" 'x)o
(bbuffer-load "/home/sendai/projects/FoxDot/FoxDot/snd/_/hyphen/2_hihat_closed.wav" '-)
;; TODO: usage of make-weighting is WRONG there
(let ((notes (fx-pat "x-")))
  (defun d2 (time)
    (if (sometimes)
        (fx-play (fx-stutter (ensure-list (next notes)) 4) :amp .4 :dur 3)
        (fx-play (next notes) :amp .4))
    (aat (+ time #[1 b]) #'d2 it)))
(aat (tempo-sync #[4 b]) #'d2 it)
(defun d2 ())
;;--------------------------------------------------
;; d3 >> play("  I ", sample=2,
;;                    hpf=(0,2000),
;;                    lpf=(300,0),
;;                    hpr=0.5)
(bbuffer-load "/home/sendai/projects/FoxDot/FoxDot/snd/i/upper/2_rock_snare.wav" 'I)
(let ((notes (fx-pat "  I ")))
  (defun d3 (time)
    (fx-play (next notes)
             :amp .5
             :lpf (ivar '(300 0)  2) :lpr .5
             :hpf (ivar '(0 2000) 2) :hpr .5)
    (aat (+ time #[1 b]) #'d3 it)))
(aat (tempo-sync #[4 b]) #'d3 it)
(defun d3 ())
;;--------------------------------------------------
;; b1 >> dbass(var([0,6,5,2],[6,2]),
;;                dur=PDur(3,8,[0,2]),
;;                sus=2,
;;                chop=4,
;;                rate=4)
(let ((scale (ov-scale :C5 :minor))
      (dur (make-cycle
            (list (make-cycle (pdur 3 8 0))
                  (make-cycle (pdur 3 8 2)))))
      (notes (var '(0 6 5 2) '(6 2 6 2)) ;;(make-cycle (make-var '(0 6 5 2) '(6 2 6 2)))
             ))
  (defun b1 (time)
    (let ((d (next dur)))
      (clc 23 (nth (next notes) scale) (rcosr 30 5 5) d)
      (aat (+ time #[d b]) #'b1 it))))
(aat (tempo-sync #[4 b]) #'b1 it)
(defun b1 ())
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
      (p time (nth (next notes) scale) 40 d 0)
      (aat (+ time #[d b]) #'p2 it))))
(aat (tempo-sync #[4 b]) #'p2 it)
(defun p2 ())
(fp 0 0)
;;--------------------------------------------------
;;k1 >> klank(oct=5, lpf=200, lpr=0.5)
(dsp! dsp-klank
    (freq1 freq2 freq3 freq4 freq5 freq6 freq7 freq8  amp lpf lpr)
  (:defaults 523.2503 587.32855 622.253
             698.4553 783.98944 830.608
             932.3258 1046.5002
             .01 200 .5)
  (with-samples ((n (white-noise))
                 ;;(n (* n (impulse 2 1 .1)))
                 ;;(n 523.2503)
                 (in (+ (incudine.vug:ringz n freq1 1)
                        (incudine.vug:ringz n freq2 1)
                        (incudine.vug:ringz n freq3 1)
                        (incudine.vug:ringz n freq4 1)))
                 ;;(in (downsamp 2 in))
                 (in (lpf in lpf lpr))
                 (in (* amp in)))
    (stereo in)))
(defun k1 ())
(incudine:free (node 25))
(let* ((scale (ov-scale :C5 :minor))
       (notes (nths '(0 1 2 3) scale))
       (midinotes (mapcar #'midihz notes)))
  (defun k1 (time)
    (setf *instances* (between 10 100))
    (setf *mul* 1f0)
    (destructuring-bind (a b c d) midinotes
      (dsp-klank a b c d  :amp .001 :lpf 200 :lpr .5 :id 100))
    ;;(aat (+ time #[1 b]) #'k1 it)
    ))
(aat (tempo-sync #[4 b]) #'k1 it)
(set-control 100 :amp .0001)
(incudine:free (node 100))
