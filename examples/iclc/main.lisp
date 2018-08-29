(in-package :shiny)

(defparameter *step* (make-stepper (seconds .05) (seconds 1)))
(defparameter *trigger* (make-trigger))
(defvar *buf* (make-buffer 512 :channels 2))
(defvar *mar* nil)
(defvar *ubo* nil)
(defvar *actors* nil)

(dsp! monitor-master ((buf buffer))
  (buffer-write
   buf
   (counter 0 511 :loop-p t)
   (audio-out 0)))

;; (monitor-master *buf* :id 100)
;;--------------------------------------------------

;;(bbuffer-load "/home/sendai/music/Furi.wav")
;;(bbplay "Furi.wav" :id 10 :loop-p t)

;;(incudine:free (node 100))
;;(initialize)

(defun initialize ()
  (setf (clear-color) (v! .2 .2 .2 0))
  (setf *actors* nil)
  (push (make-instance 'dolly)
        *actors*)
  (push (make-instance 'piso
                       :buf (sphere 100))
        *actors*)
  ;; (push (make-instance 'piso
  ;;                      :buf (box 25 .1 25)
  ;;                      :pos (v! 0 -5 0))
  ;;       *actors*)
  (push (make-instance 'voz
                       :buf (box))
        *actors*)
  (unless *ubo*
    (setf *mar* (make-c-array nil :dimensions 1 :element-type 'music))
    (setf *ubo* (make-ubo *mar*))))

(defun draw! ()  
  (when (funcall *step*)
    ;; NOTE: ?
    (with-gpu-array-as-c-array (m (ubo-data *ubo*) :access-type :write-only)
      (foreign-copy-samples (c-array-pointer m)
                            (buffer-data *buf*)
                            1024)))
  
  (let ((res (surface-resolution (current-surface))))
    (setf (resolution (current-viewport))
          res)
    (as-frame
      (update *currentcamera*)
      (loop :for a :in *actors*
         :do
         (update a)
         (draw a res)))))

(def-simple-main-loop runplay (:on-start #'initialize)
  (draw!))
