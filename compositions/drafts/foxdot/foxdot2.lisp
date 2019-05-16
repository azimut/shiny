(in-package #:shiny)

;; Scuffed version of "crushed-dreams.py"
;; From "Qirky/ten-lines-or-less"

(setf (bpm *tempo*) 144f0)
(freverb-toggle 1)
(freverb :roomsize .25d0 :level 40d0 :width 10d0)
(freset)
(fg 2f0)
(fp 0 18);; (fp 0 0)
(fpan 0 60)
;; C1 >> blip(((0,2,4,6) + var([0,3],[24,8])) % 7,
;;            dur=8,
;;            sus=2,
;;            echo=0.75,
;;            echotime=8,
;;            lpf=3000, lpr=0.2,
;;            room=0.25).spread()
(let* ((scale (ov-scale :C5 :harmonic-major))
       (e (cm:new cm:transposer
            :of '(0 2 4 6)
            :stepping
            (make-cycle
             (make-var '(0 3) '(3 1))))))
  (defun f (time)
    (p time (nths (mapcar (lambda (x) (mod x 7))
                          (next e))
                  scale)
       50 8 0)
    (aat (+ time #[8 b]) #'f it)))

(defun f ())
(aat (tempo-sync #[1 b]) #'f it)

;; p1 >> sinepad([0,4,[6,[8,9]],7],
;;               dur=1/2,
;;               sus=1,
;;               drive=0.1,
;;               room=1,
;;               lpf=expvar([20,2000],32))
;;       .sometimes("rotate") + var([0,2],[12,4])
;; p2 >> pads(p1.pitch,
;;            dur=PDur(3,8)*4,
;;            pan=PWhite(-1,1),
;;            chop=4,
;;            oct=4)

(fg 2f0)

(let ((scale (ov-scale :C5 :harmonic-major))
      (r (make-cycle
          (list (make-cycle
                 '(0)
                 (make-weighting '(4 5 6 7 8 9 10 11 12)))
                (make-cycle '(0 1 2 3) 1))))
      (e (make-cycle
          (list
           (list 0 4 6 7)
           (list 0 4 8 7)
           (list 0 4 6 7)
           (list 0 4 9 7)))))
  (defun fff (time)
    (p time (cm:transpose (nths (cm:pattern-value e) scale) -12) 40 2.5 0)
    (aat (+ time #[2.5 b]) #'fff it))
  (defun ff (time)
    (let* ((c (next e))
           (notes (nths (copy-list c) scale)))
      (if (odds .5)
          (pa time (cm:transpose (alexandria:rotate
                                  notes
                                  (next r))
                                 -24)
              (rcosr 30 5 2) .25 1 .25)
          (p time notes (rcosr 50 5 2) .5 3))
      )
    (aat (+ time #[.5 b]) #'ff it)))

(defun ff ())
(defun fff ())
(aat (tempo-sync #[1 b]) #'ff it)
(aat (tempo-sync #[1 b]) #'fff it)

;;--------------------------------------------------
;;--------------------------------------------------
;;--------------------------------------------------
(clean-buffers)

(bbuffer-load "/home/sendai/projects/FoxDot/FoxDot/snd/x/lower/1_kick_drum.wav")
(bbuffer-load "/home/sendai/projects/FoxDot/FoxDot/snd/x/lower/2_kick_drum.wav")
(bbuffer-load "/home/sendai/projects/FoxDot/FoxDot/snd/s/lower/1_shaker.wav")
(bbuffer-load "/home/sendai/projects/FoxDot/FoxDot/snd/s/lower/2_shaker.wav")
(bbuffer-load "/home/sendai/projects/FoxDot/FoxDot/snd/d/lower/1_wood.wav")
(bbuffer-load "/home/sendai/projects/FoxDot/FoxDot/snd/d/lower/2_wood.wav")
(bbuffer-load "/home/sendai/projects/FoxDot/FoxDot/snd/_/at/gb_noise (1).wav")
(bbuffer-load "/home/sendai/projects/FoxDot/FoxDot/snd/_/at/gb_noise (2).wav")
(bbuffer-load "/home/sendai/projects/FoxDot/FoxDot/snd/h/lower/snap_1.wav")
(bbuffer-load "/home/sendai/projects/FoxDot/FoxDot/snd/h/lower/snap_2.wav")
(bbuffer-load "/home/sendai/projects/FoxDot/FoxDot/snd/b/lower/dot (1).wav")
(bbuffer-load "/home/sendai/projects/FoxDot/FoxDot/snd/b/lower/dot (2).wav")
(bbuffer-load "/home/sendai/projects/FoxDot/FoxDot/snd/_/plus/wb_1.wav")
(bbuffer-load "/home/sendai/projects/FoxDot/FoxDot/snd/_/plus/wb_2.wav")

(defparameter *fx-instruments* (make-hash-table :test #'equal))
(defun fx-get (instrument &optional (sample 0))
  (gethash (gethash (list instrument sample) *fx-instruments*)
           *buffers*))

(setf (gethash (list '+ 1) *fx-instruments*) "wb_1.wav")
(setf (gethash (list '+ 2) *fx-instruments*) "wb_2.wav")
(setf (gethash (list 'x 1) *fx-instruments*) "1_kick_drum.wav")
(setf (gethash (list 'x 2) *fx-instruments*) "2_kick_drum.wav")
(setf (gethash (list 's 1) *fx-instruments*) "1_shaker.wav")
(setf (gethash (list 's 2) *fx-instruments*) "2_shaker.wav")
(setf (gethash (list 'd 1) *fx-instruments*) "1_wood.wav")
(setf (gethash (list 'd 2) *fx-instruments*) "2_wood.wav")
(setf (gethash (list '@ 1) *fx-instruments*) "gb_noise (1).wav")
(setf (gethash (list '@ 2) *fx-instruments*) "gb_noise (2).wav")
(setf (gethash (list 'h 1) *fx-instruments*) "snap_1.wav")
(setf (gethash (list 'h 2) *fx-instruments*) "snap_2.wav")
(setf (gethash (list 'b 1) *fx-instruments*) "dot (1).wav")
(setf (gethash (list 'b 2) *fx-instruments*) "dot (2).wav")

;; This one actually, plays the 4 stutter in the time slot of 1 (?, every 3 cycles
;; d1 >> play("x",sample=1,crush=16,amp=1).every(3, "stutter", 4, pan=[-1,1])
;;
;; d1 >> play("<(x )(sx)d(@hb)>< + +( [ +])>",
;;            sample=var([1,2]),
;;            crush=var([16,32],2),
;;            amp=var([1,0],[60,4]))
;; .every(3, "stutter", 4, dur=var([3,1],[7,3]), rate=2, pan=[-1,1])
;;


(let ((crush (make-cycle '(16 16 32 32)))
      (sam   (make-cycle '(1 2)))
      (other (make-cycle
              (list (make-cycle
                     (make-var '(NIL + NIL + (NIL ++)) 1)
                     3)
                    (make-cycle
                     (make-var '(NIL + NIL + (NIL ++)) 4)
                     (make-cycle '(3 1))))))
      (notes (make-cycle
              (list (make-cycle
                     (make-var '((x NIL)(s x)d(@ h b)) 1)
                     3)
                    (make-cycle
                     (make-var '((x NIL)(s x)d(@ h b)) 4)
                     (make-cycle '(3 1)))))))
  (defun d1 (time)
    (let ((pan (pick 0f0 1f0))
          (s (next sam))
          (crushed (next crush)))
      (when-let ((n (next other)))
        (case n
          (+ (bbplay (fx-get n s) :rate 1 :amp .3 :pan pan :downsamp crushed))
          (++ (at (+ time #[.15 b])
                  #'bbplay (fx-get '+ s) :rate 2 :amp .3 :pan pan :downsamp crushed))))
      (when-let ((n (next notes)))
        (bbplay (fx-get n s)
                :amp .2 :rate 1
                :downsamp crushed :pan pan)))
    (aat (+ time #[.5 b]) #'d1 it)))

(defun d1 ())
(aat (tempo-sync #[1 b]) #'d1 it)


