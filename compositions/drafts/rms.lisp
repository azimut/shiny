(in-package #:shiny)

(define-vug rms (in hp)
  (:defaults 0 60)
  (with-samples ((b   (- 2 (cos (* hp *twopi-div-sr*))))
                 (c2  (- b (sqrt (the non-negative-sample (1- (* b b))))))
                 (c1  (- 1 c2))
                 (in2 (* in in))
                 (q   +sample-zero+))
    (incudine.util:linear->db (sqrt (the non-negative-sample (~ (+ (* c1 in2) (* c2 it))))))))

(dsp! rms-monitor (rms)
  (:defaults 0)
  (setf rms (rms (audio-out 0))))

(rms-monitor :id 99)

(defun get-db (&optional (id 99))
  (let ((rms (control-value id 'rms)))
    (when rms
      (let ((output (incudine.util:linear->db rms)))
        (cm:interp (- (realpart output))
                   -50 0f0 -10 1f0)))))
