(in-package :somecepl)

(ql:quickload :cl-gme/incudine)

;; .00001 needed for gme...
;; (dsp! bplay ((buf buffer) rate start-pos (loop-p boolean) attenuation)
;;   (:defaults 0d0 1 0 nil .00001)
;;   (foreach-channel
;;     (cout (* attenuation
;;              (incudine.vug:buffer-play buf rate start-pos loop-p #'stop)))))

(define-vug rms (in hp)
  (:defaults 0 60)
  (with-samples ((b   (- 2 (cos (* hp *twopi-div-sr*))))
                 (c2  (- b (sqrt (the non-negative-sample (1- (* b b))))))
                 (c1  (- 1 c2))
                 (in2 (* in in))
                 (q   +sample-zero+))
    (sqrt (the non-negative-sample (~ (+ (* c1 in2) (* c2 it)))))))

(define-vug crossfade (in1 in2 pos)
  (with-samples ((alpha (* incudine.util:+half-pi+
                           pos)))
    (+ (* (cos alpha) in1)
       (* (sin alpha) in2))))

(dsp! bplay ((buf buffer) rate start-pos (loop-p boolean) attenuation rms cfreq)
  (:defaults 0d0 -1 0 nil .00001 0 100)
  (with-samples ((in (incudine.vug:buffer-play
                      buf rate start-pos loop-p #'stop))
                 (inn (* in attenuation))
                 (inn (+ inn
;;                        (incudine.vug:lpf inn cfreq 1)
;;                         (incudine.vug:hpf inn cfreq 2)
;;                        (incudine.vug:butter-hp inn 10)
                         )))    
    (out inn
         inn)))

(defun cfreqp ())
(let ((c (make-palindrome '(300d0 200d0 100d0)))
      (r (make-palindrome '(.5 10))))
  (defun cfreqp (time)
    (set-control 2 :cfreq (cm:next c))
    (aat (+ time #[(cm:next r) b]) #'cfreqp it)))
(cfreqp (now))

;; (lin->db (control-value 2 'rms))

(incudine:free (node 0))

(gmeplay "/home/sendai/Downloads/sf2/ff3.nsf" 2 15
         :attenuation .0001
         :length 20
         :offset 10
         :voices '(2)
         :rate 1
         :fade-time 1
         :fade-curve 2)

(gmeplay "/home/sendai/Downloads/chrono/108 A Strange Happening.spc" 3 0
         :length 18
         :offset 89
         :rate 1
         :attenuation .00004
         :voices '(0 7 5)
         :fade-time 10
         :fade-curve 2
;;         :load-only t
         )

(gmeplay "/home/sendai/Downloads/chrono/108 A Strange Happening.spc" 5 0
         :length 9
         :offset 30
         :rate 1.
         :attenuation .00001
         :voices '(1 0 7)
         :fade-time 20f0
         :fade-curve 2
;;         :load-only t
         )

(set-control 2 :cfreq 100)
(set-control 4 :attenuation .00001)
(set-control 2 :rate .2)
(set-control 2 :fade-curve 2)

(gmeclean)
(incudine:free (node 0))

(let ((upper (make-cycle '(t nil)))
      (pan   (make-cycle '(0 127)))
      (pro   (make-cycle '(i iv v))))
  (defun bf (time)
    (let ((buf (gethash
                 "/home/sendai/Downloads/chrono/108 A Strange Happening.spc3"
                 *playing*)))
      (bbplay buf :beat-length 4 :attenuation .0001)
;;      (p time 48 80 3 2)
;;      (p (+ #[3 b] time) 48 80 3 2)
      ;; ?
      ;; (pa time (nths (pick '(0) '(0 1) '(0 0))
      ;;           (make-chord 60 70 3 (pc-diatonic 0 'minor (next pro))))
      ;;     3
      ;;     (rcosr 62 2 5) 1 3)
      (aat (+ time #[4 b])
           #'bf it))))

(fg 1f0)
(fp 0 20)
(fp 2 20)
(fp 1 20)
(freverb-toggle 1)
(freverb-preset 6)

(defun bf ())
(bf (tempo-sync #[4 b]))
(flush-pending)
;;--------------------------------------------------

(defvar *status* nil)
(defvar *status2* nil)
(defvar *status3* nil)
(defvar *status4* nil)

(defun status (time)
  (let ((cvalue (control-value 2 'rms))
        (ccvalue (control-value 3 'rms))
        (cccvalue (control-value 4 'rms)))


    (when (numberp cvalue)
      (if (> (abs (lin->db cvalue)) 90)
          (setf *status* 1)
          (setf *status* nil)))
    
    (when (numberp ccvalue)
      (if (> (abs (lin->db ccvalue)) 90)
          (setf *status2* (list (list (between 20 300) (between 40 200))
                                (list (between 20 300) (between 80 200))))
          (setf *status2* nil)))    

    (when (numberp cccvalue)
      (if (> (abs (lin->db cvalue)) 10)
          (progn
            (setf *status3* (list (between 20 300) (between 40 200)))
            (setf *status4* (* 30 (pick 10 20 30 40))))
          (setf *status3* nil)))
    ) 
  (at (+ time #[.1 b]) #'status (+ time #[.1 b])))

;;(defun status ())
;;(status (now))
