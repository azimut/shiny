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
  (:defaults 0d0 1 0 nil .00001 0 100)
  (with-samples ((in (incudine.vug:buffer-play
                      buf rate start-pos loop-p #'stop))
                 (inn (* in attenuation))
                 (inn (+ inn
;;                        (incudine.vug:lpf inn 500 2)
                        (incudine.vug:hpf inn 100 2)
;;                        (incudine.vug:butter-hp inn 10)
                         )))    
    (out inn
         inn)))

(defun cfreqp ())
(let ((c (make-palindrome '(500d0 100d0)))
      (r (make-palindrome '(.5 10))))
  (defun cfreqp (time)
    (set-control 2 :cfreq (cm:next c))
    (aat (+ time #[(cm:next r) b]) #'cfreqp it)))
(cfreqp (now))

;; (lin->db (control-value 2 'rms))

(defparameter *old* (make-hash-table :test #'equal))
(defparameter *new* (make-hash-table :test #'equal))

(defun gme-clean ()
  (loop for key in (alexandria:hash-table-keys *new*)
       :do (incudine:free (gethash key *new*)))
  (loop for key in (alexandria:hash-table-keys *old*)
       :do (incudine:free (gethash key *old*))))

(gme-clean)

(defun gmebuffer (filename &key (len 1) (track-number 0)
                             (rate 44100) (offset 0)
                             (voices '()))
  "returns a buffer"
  (let* ((frames (* len    10 4410)) ;; need to be multiple of 4410...
         (offset (* offset 10 4410))
         (buf    (make-buffer (/ frames 2)
                              :channels 2
                              :sample-rate rate)))
    (cl-gme:with-track
        (gmefile filename track-number :rate rate :voices voices)
      (cl-gme::gmefile-to-buffer (buffer-data buf)
                                 (cffi:mem-ref gmefile :pointer)
                                 frames
                                 offset))
    buf))

(defun gmeplay (filename node track-number
                &key (attenuation .00001) (rate 1f0) (start-pos 0)
                  (fade-curve 3) (fade-time 0f0)
                  (length 1) (offset 0) (voices '()))
  (declare (type integer node track-number length offset))
  (let ((alive   (node-id (node node)))
        (hashkey (concatenate 'string filename (write-to-string node))))
    (setf (gethash hashkey *old*)
          (if voices
              (gmebuffer filename
                      :track-number track-number
                      :len length
                      :voices voices
                      :offset offset)
              (gmebuffer filename
                      :track-number track-number
                      :len length
                      :offset offset)))
    (if alive
        (set-controls
         node
         :buf (gethash hashkey *old*)
         :rate rate
         :fade-curve fade-curve
         :fade-time fade-time
         :attenuation attenuation)
        (bplay (gethash hashkey *old*) rate 0 t
               :id node
               :fade-curve fade-curve
               :fade-time fade-time
               :attenuation attenuation))
    (rotatef (gethash hashkey *old*)
             (gethash hashkey *new*))
    (incudine:free (gethash hashkey *old*))))

(gmeplay "/home/sendai/Downloads/chrono/111 Secret of the Forest.spc" 2 0
         :length 40
         :offset 10
         :rate .7
         :voices '(1 2 0 3)
         :fade-time 20f0
         :fade-curve 2)

(gmeplay "/home/sendai/Downloads/chrono/111 Secret of the Forest.spc" 3 0
         :length 30
         :offset 43
         :rate 1
         :voices '(7 5 4 6 0)
         :fade-time 20f0
         :fade-curve 2)

(set-control 2 :cfreq 100)
(set-control 4 :attenuation .00001)
(set-control 2 :rate .7)
(set-control 2 :fade-curve 2)

(gme-clean)
(incudine:free (node 3))

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
