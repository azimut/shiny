(in-package :somecepl)

(defparameter *step* (make-stepper (seconds .05) (seconds 1)))

(defvar *car*
  (make-c-array nil :dimensions 512 :element-type :float))

(defvar *bs*  nil)
(defvar *tex* nil)
(defvar *sam* nil)

(defun-g frag
    ((uv :vec2) &uniform
     (time :float) (texture :sampler-1d))
  (let* ((tx (* 512 (x uv)))
         ;; (wave (texel-fetch texture ))
         (something (texture texture tx))
         (other (- 1f0 (smoothstep (v4! 0f0) (v4! .03) (abs (- something (+ -.5 (y uv)))))))
         )
    other))

(defpipeline-g pipe (:points)
  :fragment (frag :vec2))

(defun draw! ()
  (let ((res (surface-resolution (current-surface))))
    (setf (viewport-resolution (current-viewport))
          res)
    (when (funcall *step*)
      ;; Test to upload random data to the c array
      (loop :for i :upto 100
         :do (setf (cepl:aref-c *c-arr* (+ (random 400) i)) (random 1f0)))
      (setf *tex*   (make-texture *c-arr*))
      (setf *s-tex* (cepl:sample *tex*)))
    (as-frame
      (map-g #'pipe *bs*
             :texture *s-tex*
             :time (/ (get-internal-real-time)
                      1000f0)))))

(defun initialize ()
  (unless *bs*
    (setf *bs*  (make-buffer-stream nil :primitive :points))
    (setf *tex* (make-texture *car*))
    (setf *sam* (cepl:sample *tex*))))

(def-simple-main-loop runplay (:on-start #'initialize)
  (draw!))
