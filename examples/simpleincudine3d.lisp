;;;; somecepl.lisp

(in-package #:somecepl)

;; ----------------------------------
;; ----------------------------------

;; From Music V family.
(define-vug rms (in hp)
  (:defaults 0 60)
  (with-samples ((b   (- 2 (cos (* hp *twopi-div-sr*))))
                 (c2  (- b (sqrt (the non-negative-sample (1- (* b b))))))
                 (c1  (- 1 c2))
                 (in2 (* in in))
                 (q   +sample-zero+))
    (sqrt (the non-negative-sample (~ (+ (* c1 in2) (* c2 it)))))))

#|
(dsp! rms-master-out-test2 ()
   (setf (bus 100)
         (rms (audio-out 0))))
|#

(defvar *grms* nil)
(dsp! rms-master-out-test3 ()
  (incudine.vug:with ((i 0)
                      (rms +sample-zero+))
    (declare (incudine.util:sample rms))
    ;; Note: it is possible to optimize RMS here.
    (setf rms (rms (audio-out 0)))
    (when (= i 0)
      ;; or (nrt-funcall (lambda () (send-to-cepl rms)))
      ;;     (lock-free-send-to-cepl rms))
      (setf *grms* (coerce rms 'single-float)))
    ;; 256 samples are about 5ms if sr 48000
    (setf i (logand (1+ i) 255))))

(dsp! rms-master-out-test3 ()
  (with ((i 0)
         (rms +sample-zero+))
    (declare (sample rms))
    ;; Note: it is possible to optimize RMS here.
    (setf rms (rms (audio-out 0)))
    (when (= i 0)
      ;; or (nrt-funcall (lambda () (send-to-cepl rms)))
      (setf *grms* (coerce rms 'single-float) ) )
    ;; 256 samples are about 5ms if sr 48000
    (setf i (logand (1+ i) 255))))

#|
(defun rms-master ()
   (coerce (incudine.util:barrier (:memory) (bus 100)) 'single-float))
|#

(rms-master-out-test2 :id 100 :tail 0)

(rms-master-out-test3 :id 100 :tail 0)
(incudine:free 100)
;;(rms-master)

;; ----------------------------------
;; ----------------------------------

(defvar env2 (make-perc .001 .4))

(dsp! env-test-3 (freq amp pos (env envelope) gate)
  (foreach-channel
    (cout (pan2 (* (envelope env gate 1 #'stop)
                   (sine freq amp 0))
                pos))))
(defun seq-test (rep freq amp pos)
  (when (plusp rep)
    (dsp-seq (env-test-3 freq amp pos env2 1)
             (env-test-3 (* freq 7/4) amp pos env2 1)
             (env-test-3 (* freq 2) amp pos env2 1)
             (seq-test (1- rep) freq amp pos))))

(defun phr1 (time)
  (at time #'seq-test 8 200 .3 .5)
  (at (+ time #[2 b]) #'seq-test 6 400 .3 .4)
  (at (+ time #[4 b]) #'seq-test 4 600 .3 .6))

(setf (bpm *tempo*) 120)
(phr1 (now))

;; ----------------------------------
;; "somecepl" goes here. Hacks and glory await!
;; ----------------------------------

(defvar *buf-stream* nil)
(defvar *gpu-arr* nil)
(defvar *cam-pos* (v! 0 0 0))
(defvar *cam-rot* (q:identity))

(defun tt ()
    (coerce (get-internal-run-time) 'single-float))

(defun-g vert-stage ((vert g-pnt) &uniform
                     (time :float)
                     (rms :float)
                     (cam-pos :vec3)
                     (cam-rot :mat3)
                     (perspective :mat4))
  (let* ((pos   (pos vert))
         (id    gl-instance-id)
         ;;(id ( id))
         (rms   (* 200 rms))
         (time  (+ (/ time 50) id rms))
         (color (+ (v3!  (sin rms) ) (pos vert)))
         (pos   (+ pos (v! (cos (* 2 time)) (sin time) (+ -7 (sin time)) )))
         (pos   (+ pos cam-pos))
         (pos   (* cam-rot pos)))
    
    (values 
     (* perspective (v! pos 1))
     (:smooth color) )))

(defun-g frag-stage ((color :vec3) &uniform (resolution :vec2)
                              (time :float)
                              (rms :float))
  (v! color 0)
  )

(defpipeline-g draw-verts-pipeline ()
  :vertex   (vert-stage g-pnt)
  :fragment (frag-stage :vec3))

(defun draw! ()
   (step-host)
   (setf (resolution (current-viewport))
         (surface-resolution (current-surface (cepl-context))))

;   (setf *cam-rot* (q:from-axis-angle (v! 0 0 1) (radians (* 10 (sin (* .05 (tt)) ) ))))
   (clear)
   (with-instances 40
     (map-g #'draw-verts-pipeline *buf-stream*
            :resolution (viewport-resolution (current-viewport))
            :time (tt)
            :rms (rms-master)
            :cam-pos *cam-pos*
            :cam-rot (q:to-mat3 (q:inverse *cam-rot*))
            :perspective (rtg-math.projection:perspective
                          (x (viewport-resolution (current-viewport)))
                          (y (viewport-resolution (current-viewport)))
                          0.5
                          30f0
                          60f0)))
   (swap))

(defun runinit ()
  (unless *buf-stream*
    (destructuring-bind (vert index)
        (nineveh.mesh.data.primitives:cube-gpu-arrays)
      (setf *buf-stream*
            (make-buffer-stream vert :index-array index)))))

(def-simple-main-loop runplay (:on-start #'runinit)
  (draw!))
#|
(setf (depth-test-function (cepl-context)) nil)
(setf (depth-test-function (cepl-context)) #'<)
|#
