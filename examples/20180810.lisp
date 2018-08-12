(in-package :somecepl)

(defvar *growth* 0f0)

(defun-g circle-sdf ((st :vec2))
  (* 4.0 (length (- st .5))))

(pixel-spirit-deck:poly-sdf)

(defun-g poly-sdf ((st :vec2) (V :float))
  (let* ((st (- (* 2.0 st) 1.0))
         (a  (+ 3.1415 (atan (x st) (y st))))
         (r  (length st))
         (v  (/ 6.283185 (float V))))
    (* r (cos (- (* v (floor (+ .5 (/ a v)))) a)))))

(defun-g frag ((uv :vec2)
               &uniform
               (trigger :int)
               (vtrigger :int)
               (growth :float)
               (resolution :vec2)
               (time :float))
  (let* ((gate1 (= vtrigger 0))
         (st    uv)
         (color (v3! 0 0 0))
         (color (v3! 1 1 1))
         (color (v3! (smoothstep (tan time) (cos time)
                                 (poly-sdf st (ceil (* 4 (+ .5 (* .5 (sin time)))))))))
         (time  (* time (if (= trigger 0) .5 1f0)))
         (st    (if gate1
                    (+ (v! (* .5 (sin time))
                           (* .5 (cos time)))
                       st)
                    (+ (v! (* .5 (cos time))
                           (* .5 (sin time)))
                       st)))
         (color (* color (v3! (circle-sdf st))))
         (color (if gate1 (- color) (-  color (v! .2 .7 1))))
         (color (+ (* 1 growth) color))
         )
    (v! color 1)))

(defvar *trigger* (make-trigger))
(defvar *vtrigger* (make-trigger))

;;--------------------------------------------------

(defpipeline-g pipe (:points)
  :fragment (frag :vec2))

(define-vug rms (in hp)
  (:defaults 0 60)
  (with-samples ((b   (- 2 (cos (* hp *twopi-div-sr*))))
                 (c2  (- b (sqrt (the non-negative-sample (1- (* b b))))))
                 (c1  (- 1 c2))
                 (in2 (* in in))
                 (q   +sample-zero+))
    (lin->db (sqrt (the non-negative-sample (~ (+ (* c1 in2) (* c2 it))))))))

(dsp! rms-monitor (rms)
  (:defaults 0)
  (setf rms (rms (audio-out 0))))

(rms-monitor :id 99)

(defun get-db (&optional (id 99))
  (let ((rms (control-value id 'rms)))
    (when rms
      (let ((output (lin->db rms)))
        (cm:interp (- (realpart output))
                   -50 0f0 -10 1f0)))))

;;--------------------------------------------------

(defvar *bs*
  (make-buffer-stream nil :primitive :points))

(defun draw! ()
  (let ((res (surface-resolution (current-surface))))
    (setf (viewport-resolution (current-viewport))
          res)
    (as-frame
      (map-g #'pipe *bs*
             :trigger (funcall *trigger* 'shoot)
             :vtrigger (funcall *vtrigger* 'shoot)
             :growth (coerce (get-db) 'single-float)
             :resolution res
             :time (/ (get-internal-real-time)
                      1000f0)))))

(def-simple-main-loop runplay ()
  (draw!))
