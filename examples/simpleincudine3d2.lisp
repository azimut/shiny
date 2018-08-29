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
;;      (setf *grms* (coerce rms 'single-float) ) )
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
;; "somecepl" goes here. Hacks and glory await!
;; ----------------------------------

(defvar *buf-stream* nil)
(defvar *gpu-arr* nil)
(defvar *light-pos* (v! 0 30 -5))

(defclass camera ()
  ((pos :initform (v! 0 0 0) :accessor pos)
   (rot :initform (q:identity) :accessor rot)))

(defvar *camera* (make-instance 'camera))

(defclass thing ()
  ((pos :initform (v! 0 0 0) :accessor pos)
   (rot :initform (q:identity) :accessor rot)))

(defvar *things* (loop :for i :below 40 :collect
                    (make-instance 'thing)))

(defun tt ()
    (coerce (get-internal-run-time) 'single-float))
 
(defun-g vert-stage ((vert g-pnt) &uniform
                     ;; (time :float)
                     ;; (rms :float)
                     (model->world :mat4)
                     (world->view :mat4)
                     (view->clip :mat4))
  (let* ((pos   (pos vert))
         (color (+ (pos vert) (v! .5 .5 .5)))
         ;; (id    gl-instance-id)
         ;; (rms   (* 150 rms))
         ;; (time  (+ (/ time 50)
         ;;           id
         ;;           rms))
         ;; model space -> world space
         (pos (v! pos 1))
         (pos (* model->world pos))
         ;; world space -> view/camera/eye space
         (pos   (* world->view pos)))
    ;; view space -> clip space
    (values (* view->clip pos) color)))

(defun-g frag-stage ((color :vec3)&uniform
                     (resolution :vec2)
                     (time :float)
                     (light-pos :vec3)
                     (rms :float))
  (let* ((object-color (v! 1 1 0 0))
         (ambient 0.4)
         (diffuse 0.0)
         (light-amount (+ ambient diffuse)))
    (* object-color light-amount)
    color))

(defun get-world->view-space (camera)
  (m4:*
   (m4:translation (v3:negate (pos camera)))
   (q:to-mat4 (q:inverse (rot camera)))))

(defun get-model->world-space (thing)
  (m4:*
   (m4:translation (pos thing)))
   (q:to-mat4 (rot thing)))

(defpipeline-g draw-verts-pipeline ()
  :vertex   (vert-stage g-pnt)
  :fragment (frag-stage :vec3))

(defun draw! ()
   (step-host)
   (setf (resolution (current-viewport))
         (surface-resolution (current-surface (cepl-context))))
   ;; (setf *cam-rot*
   ;;       (q:from-axis-angle
   ;;        (v! 0 0 1)
   ;;        (radians (* 10 (sin (* .05 (tt)) ) ))))   
   (clear)
   (loop :for thing :in *things* :do
      (map-g #'draw-verts-pipeline *buf-stream*
             :resolution (viewport-resolution (current-viewport))
;;             :time (tt)
             :light-pos *light-pos*
;;             :rms (rms-master)
             :model->world (get-model->world-space thing)
             :world->view (get-world->view-space *camera*)
             :view->clip (rtg-math.projection:perspective
                          (x (viewport-resolution (current-viewport)))
                          (y (viewport-resolution (current-viewport)))
                          0.4
                          200f0
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
