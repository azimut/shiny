(in-package :somecepl)

(defparameter *step* (make-stepper (seconds .05) (seconds 1)))

(defparameter *trigger* (make-trigger))

(defvar *buf* (make-buffer 512))

(defvar *light* (v! .2 .2 .2))

(defvar *mar* nil)
(defvar *ubo* nil)

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

(defclass actor ()
  ((pos :initarg :pos :initform (v! 0 0 0))
   (rot :initarg :rot :initform (v! 0 0 0))
   (buf :initarg :buf :initform (box))))

(defvar *actors* nil)

(defun-g vert
    ((vert g-pnt) &uniform
     (time :float) (sound music :ubo) (model-world :mat4))
  (let* ((pos (pos vert))
         (pos (* (v! pos 1) model-world)))
    (values pos
            (v! (+ .5 (* .5 (s~ pos :xy)))))))

(bbuffer-load "/home/sendai/music/Furi.wav")
(bbplay "Furi.wav" :id 100 :loop-p t)
(incudine:free (node 100))

(defpipeline-g pipe ()
  :vertex (vert g-pnt)
  :fragment (frag :vec2))

(defun initialize ()
  (setf (clear-color) (v! 1 1 1 1))
  (setf *actors* nil)
  (push (make-instance 'actor
                       :buf (box .5)
                       :pos (v! .2 .2 .2))
        *actors*)
  (unless *ubo*
    (setf *mar* (make-c-array nil :dimensions 1 :element-type 'music))
    (setf *ubo* (make-ubo *mar*))))

(defgeneric update (actor))

(defmethod update ((actor actor))
  (with-slots (pos rot) actor
    (setf pos (v! 0 0 .2))
    (setf rot (v! 0 (sin (get-universal-time)) 0))))

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
    (as-frame
      (loop :for a :in *actors*
         :do
;;         (update a)
         (with-slots (pos rot buf) a
           (map-g #'pipe buf
                  ;;               :rms (float (funcall *trigger* 'bleed))
                  :time (* .001 (get-internal-real-time))
                  :sound *ubo*
                  :model-world (m4:* (m4:translation pos)
                                     (q:to-mat4 rot))
                  ))))))

(def-simple-main-loop runplay (:on-start #'initialize)
  (draw!))
