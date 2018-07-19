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

(dsp! bplay ((buf buffer) rate start-pos (loop-p boolean) attenuation rms)
  (:defaults 0d0 1 0 nil .00002 0)
  (with-samples ((in (incudine.vug:buffer-play buf rate start-pos loop-p #'stop))
                 (in (* attenuation in)))
    (setf rms (rms in))
    (out in
         in)))

;; (lin->db (control-value 2 'rms))



(defun gmebuffer (filename &key (len 1) (track-number 0) (rate 44100)
                             (voices '()))
  (let* ((frames (* len 10 4096)) ;; need to be multiple of 4096...
         (buf    (make-buffer (/ frames 2)
                              :channels 2
                              :sample-rate rate)))
    (cl-gme:with-track
        (gmefile filename track-number :rate rate :voices voices)
      (cl-gme::gmefile-to-buffer (buffer-data buf)
                                 (cffi:mem-ref gmefile :pointer)
                                  frames))
    buf))

;;--------------------------------------------------

;; FINAL FORM WITH FREE!! ... needs macro
(let* ((node 2)
       (alive (node-id (node node))))
  
  (defvar *tmp1* nil)
  (defvar *tmp2* nil)
  
  (setf *tmp1*
        (gmebuffer "/home/sendai/Downloads/sf2/mother.nsf"
                   :voices '(3)
                   :track-number 32
                   :len 200))
  
  ;; reference of the old sound is at the control
  (set-control node :buf *tmp1*)
  
  (unless alive
    (bplay *tmp1* 1 0 t
           :id node
           :fade-curve 3
           :attenuation .00001))
  
  (rotatef *tmp1* *tmp2*)
  (incudine:free *tmp1*))

(let* ((node 3)
       (alive (node-id (node node))))
  (defvar *tmp3* nil)
  (defvar *tmp4* nil)  
  (setf *tmp3*
        (gmebuffer "/home/sendai/Downloads/sf2/mother.nsf"
                   :voices '(2)
                   :track-number 32
                   :len 200))
  (set-control node :buf *tmp3*)
  
  (unless alive
    (bplay *tmp3* 1 0 t :id node :fade-curve 3))
  
  (rotatef *tmp3* *tmp4*)
  (incudine:free *tmp3*))


(let* ((node 4)
       (alive (node-id (node node))))
  (defvar *tmp5* nil)
  (defvar *tmp6* nil)  
  (setf *tmp5*
        (gmebuffer "/home/sendai/Downloads/sf2/mother.nsf"
                   :voices '(0 1)
                   :track-number 32
                   :len 250))
  (set-control node :buf *tmp5*)
  
  (unless alive
    (bplay *tmp5* 1 0 t :id node :fade-curve 3))
  
  (rotatef *tmp5* *tmp6*)
  (incudine:free *tmp5*))

(set-control 4 :attenuation .00001)
(set-control 4 :rate 1.4)
(set-control 2 :fade-curve 2)

(setf *status3* nil)
(incudine:free (node 0))


;;--------------------------------------------------

