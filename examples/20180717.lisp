(in-package :somecepl)

(defparameter *step* (make-stepper (seconds .05) (seconds 1)))

(defparameter *trigger* (make-trigger))

(defvar *buf* (make-buffer 512))

(defvar *bs*  nil)
(defvar *mar* nil)
(defvar *ubo* nil)
(defvar *scene-fbo* nil)
(defvar *scene-sample* nil)

(dsp! monitor-master ((buf buffer))
  (buffer-write
   buf
   (counter 0 511 :loop-p t)
   (audio-out 0)))

;; (monitor-master *buf* :id 100)
;;--------------------------------------------------

(defstruct-g (music :layout :std-140)
  (samples (:double 512)))

(defun-g g-rand ((x :float))
  (fract (* (sin x)
            10000f0)))

(defun-g cyn ((x :float))
  (+ .5 (* .5 (cos x))))

(defun-g syn ((x :float))
  (+ .5 (* .5 (sin x))))

(defun-g g-random ((st :vec2))
  (fract (* (sin (dot st
                      (v! 12.9898 ;; moves noise
                          78.233
                          )))
           43758.543123
            )))

(defun-g frag
    ((uv :vec2) &uniform (sound music :ubo) (time :float)
     (rms :float))
  (with-slots (samples) sound
    (let* (;;(wide (+ 100 (* 100 (+ .5 (* .5 (sin time))))))
           (wide 250)
           (wave (float (aref samples (int (ceil (* wide  (x uv)))))))
           (wave (- 1   (smoothstep 0f0 .01 (abs (- wave (- (y uv) .5))))))
           (wave (v3! wave))
           ;; (uv    (v! (length (- (x uv) .5))
           ;;            (y uv)))
           ;; !
;;          (color (mix c1 c2 (+ (- rms) 1 (y uv))))
           ;;          (color (+ color wave))
           )
      wave)))

(defpipeline-g pipe (:points)
  :fragment (frag :vec2))

;;--------------------------------------------------
(defun-g frag-last ((uv :vec2) &uniform (waves :sampler-2d) (time :float))
  (let ((color (texture waves (* (+ 2 (* 12 (cyn time))) uv))))
    color))

(defun-g vert-last ((vert :vec2))
  (values (v! vert 0 1)
          (+ .5 (* .5 vert))))

(defpipeline-g lastpipe ()
  (vert-last :vec2)
  (frag-last :vec2))
;;--------------------------------------------------

(defun initialize ()
  (when *scene-fbo*
    (cepl:free *scene-fbo*))
  (setf *scene-fbo* (make-fbo 0))
  (setf *scene-sample* (cepl:sample (attachment-tex *scene-fbo* 0)))
  (unless *bs*
    (setf *bs*  (make-buffer-stream nil :primitive :points))
    (setf *mar* (make-c-array nil :dimensions 1 :element-type 'music))
    (setf *ubo* (make-ubo *mar*))))

(defun draw! ()  
  (when (funcall *step*)
    ;; NOTE: ?
    (with-gpu-array-as-c-array (m (ubo-data *ubo*) :access-type :write-only)
      (incudine.external:foreign-copy-samples
       (c-array-pointer m)
       (buffer-data *buf*)
       512)))  
  (let ((res (surface-resolution (current-surface))))
    (setf (viewport-resolution (current-viewport))
          res)
    
    (with-fbo-bound (*scene-fbo*)
      (clear-fbo *scene-fbo*)
      (map-g #'pipe *bs*
             :rms (float (funcall *trigger* 'bleed))
             :time (* .001 (get-internal-real-time))
             :sound *ubo*))
    (as-frame
      (clear)
      (map-g #'lastpipe (get-quad-stream-v2)
             :waves *scene-sample*
             :time (* .001 (get-internal-real-time))))))

(def-simple-main-loop runplay (:on-start #'initialize)
  (draw!))

