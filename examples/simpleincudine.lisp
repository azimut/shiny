;;;; somecepl.lisp

(in-package #:shiny)

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

(dsp! rms-master-out-test2 ()
   (setf (bus 100)
         (rms (audio-out 0))))

(defun rms-master ()
  (coerce (incudine.util:barrier (:memory) (bus 100)) 'single-float))

(rms-master-out-test2 :id 100 :tail 0)
(rms-master)

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

(defvar *gpu-verts-arr* nil)
(defvar *gpu-index-arr* nil)
(defvar *vert-stream* nil)

(defun-g draw-verts-vert-stage ((vert :vec2))
  (v! vert 0 1))

(defun-g draw-verts-frag-stage (&uniform (resolution :vec2)
                                         (time :float)
                                         (rms :float))
  (let* ((st    (v! (/ (x gl-frag-coord) (x resolution))
                    (/ (y gl-frag-coord) (y resolution))))
         (color (v! (step .5 (x st))
                    (step .5 (x st))
                    (step .5 (x st)))))
    (v! rms rms rms 1.0)
  ))

(defpipeline-g draw-verts-pipeline ()
  :vertex   (draw-verts-vert-stage :vec2)
  :fragment (draw-verts-frag-stage))

(defun draw! ()
   (step-host)
   (setf (resolution (current-viewport))
         (surface-resolution (current-surface (cepl-context))))
   (clear)
   (map-g #'draw-verts-pipeline *vert-stream*
          :resolution (viewport-resolution (current-viewport))
          :time (mynow)
          :rms .52)
   (swap))

(defun runinit ()

  (when *gpu-verts-arr*
    (free *gpu-verts-arr*))
  (when *gpu-index-arr*
    (free *gpu-index-arr*))
  (when *vert-stream*
    (free *vert-stream*))
  
  (setf *gpu-verts-arr*
        (make-gpu-array
         (list (v! -1.0  1.0)
               (v! -1.0 -1.0)
               (v!  1.0 -1.0)
               (v!  1.0  1.0))
         :element-type :vec2))
  
  (setf *gpu-index-arr*
        (make-gpu-array
         (list 0 1 2
               0 2 3)))

  (setf *vert-stream*
        (make-buffer-stream *gpu-verts-arr*
                            :index-array *gpu-index-arr*)))

(def-simple-main-loop runplay (:on-start #'runinit)
  (draw!))
