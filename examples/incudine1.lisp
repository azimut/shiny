;;;; somecepl.lisp

(in-package #:somecepl)

;;; "somecepl" goes here. Hacks and glory await!

(defvar *gpu-verts-arr* nil)
(defvar *gpu-index-arr* nil)
(defvar *vert-stream* nil)

;; (incudine.vug:define-vug rms (in hp
;;   (:defaults 0 30)
;;   (incudine.util:with-samples ((b    (- 2 (cos (* hp incudine.util:*twopi-div-sr*))))
;;                                (c2   (- b (sqrt (the non-negative-sample (1- (* b b))))))
;;                                (c1   (- 1 c2))
;;                                (in2  (* in in))
;;                                (q    incudine.util:+sample-zero+))
;;   (sqrt (the non-negative-same (incudine.vug:~ (+ (* c1 in2) (* c2 it)))))))
;;
;; (incudine.vug:dsp! rms-test (gain freq rms)
;;   "Get the RMS amplitude by using a control parameter with side effect."
;;   (:defaults -14 440 0)
;;   (incudine.util:with-samples ((ma (* .5 (incudine.util:db->lin gain)))
;;                                (in (incudine.vug:sine freq
;;                                                       (+ ma (incudine.vug:sine 4 ma 0))
;;                                                       0)))
;;
;;     (setf rms (rms in))
;;     (incudine.vug:out in)))

(defun-g draw-verts-vert-stage ((vert :vec2))
  (v! vert 0 1))

(defun-g draw-verts-frag-stage (&uniform
                                (resolution :vec2)
                                (time :float))
  (let* ((st    (v! (/ (x gl-frag-coord) (x resolution))
                    (/ (y gl-frag-coord) (y resolution))))
         (stepn     (abs (sin time)))
         (color (v! (step stepn (x st))
                    (step stepn (x st))
                    (step stepn (x st)))))
    (v! color 1.0)
  ))

(defpipeline-g draw-verts-pipeline ()
  :vertex   (draw-verts-vert-stage :vec2)
  :fragment (draw-verts-frag-stage))

(defun now ()
  (float (/ (get-internal-real-time) 1000 )))

(defun draw! ()
   (step-host)
   (setf (resolution (current-viewport))
         (surface-resolution (current-surface *cepl-context*)))
   (clear)
   (map-g #'draw-verts-pipeline *vert-stream*
          :resolution (viewport-resolution (current-viewport))
          :time (coerce (control-value 123 'rms) 'short-float))
   (swap))

(defun init ()

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

(def-simple-main-loop play (:on-start #'init)
  (draw!))
