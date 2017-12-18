(in-package :somecepl)

;; From Music V family.
(define-vug rms (in hp)
  (:defaults 0 60)
  (with-samples ((b   (- 2 (cos (* hp *twopi-div-sr*))))
                 (c2  (- b (sqrt (the non-negative-sample (1- (* b b))))))
                 (c1  (- 1 c2))
                 (in2 (* in in))
                 (q   +sample-zero+))
    (sqrt (the non-negative-sample (~ (+ (* c1 in2) (* c2 it)))))))

(dsp! rms-test (gain freq rms)
  "Get the RMS amplitude by using a control parameter with side effect."
  (:defaults -14 440 0)
  (with-samples ((ma (* .5 (db->lin gain)))
                 (in (sine freq (+ ma (sine 4 ma 0)) 0)))
    (setf rms (rms in))
    (out in)))

(rt-start)

(rms-test :id 123)

(lin->db (control-value 123 'rms))

(incudine:free 123)

;;;
;;;
;;;

(dsp! rms-master-out-test (rms)
  (:defaults 0)
  (setf rms (rms (audio-out 0))))

(defun rms-value (id)
  (lin->db (control-value id 'rms)))

(rms-test -20 440 :id 1)
(rms-test -20 555 :id 2)
(rms-test -20 666 :id 3)

;; The last DSP to get the RMS amp of the mix.
(rms-master-out-test :id 100 :tail 0)

;; Mix
(rms-value 100)

;; Get all the values from rt-thread
(rt-eval (:return-value-p t) (mapcar #'rms-value '(1 2 3 100)))

(incudine:free 0)

;;;
;;; Alternative 1: a bus for master out
;;;

(dsp! rms-master-out-test2 ()
  (setf (bus 100) (rms (audio-out 0))))

(defun rms-master ()
  (lin->db (incudine.util:barrier (:memory) (bus 100))))

(rms-test -20 440 :id 1)
(rms-test -20 555 :id 2)
(rms-test -20 666 :id 3)
(rms-master-out-test2 :id 100 :tail 0)

(rms-master)

(incudine:free 0)

;;;
;;; Alternative 2: lock-free call from DSP
;;;

(dsp! rms-master-out-test3 ()
  (with ((i 0)
         (rms +sample-zero+))
    (declare (sample rms))
    ;; Note: it is possible to optimize RMS here.
    (setf rms (rms (audio-out 0)))
    (when (= i 0)
      ;; or (nrt-funcall (lambda () (send-to-cepl rms)))
      (lock-free-send-to-cepl rms))
    ;; 256 samples are about 5ms if sr 48000
    (setf i (logand (1+ i) 255))))

;;; Copy buffer data

(dsp! copy-buffer-data-test ((buf buffer))
  [...]
  (with-follow (buf)
    ;; warning: no size check [buffer-to-copy size is >= buf size otherwise...]
    (incudine.external:foreign-copy-samples
      (buffer-data buf)
      ;; copy from BUFFER-DATA, ABUFFER-DATA, foreign data, etc..
      (buffer-data buffer-to-copy)
      (buffer-size buf)))
  [...])

;; to eval in rt-thread
(defun update-my-buffer (node buffer)
  ;; trigger the copy
  (set-control node :buf buffer)
  (lock-free-send-to-cepl show-buffer-data buffer))

(defvar *my-buffer* (make-buffer 1024))

(rt-eval () (update-my-buffer some-node *my-buffer*))
