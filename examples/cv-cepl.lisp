(in-package :shiny)

(defvar *bs* NIL)

(defvar *capture* (cv:create-file-capture "/home/sendai/testfield/hila.mp4"))
(defvar *frame* (cv:query-frame *capture*))
(defvar *data* (cv:ipl-data *frame*))
(defvar *props* (get-props *capture*))
(defparameter *c-arr* (make-c-array-from-pointer '(426 240) :char *data*))

(defun-g frag ((uv :vec2))
  (v! 1 0 0 1))

(defpipeline-g pipe (:points)
  :fragment (frag :vec2))
 
(defun initialize ()
  (unless *bs*
    (setf *bs* (make-buffer-stream NIL :primitive :points))))

(defun draw! ()
  (let ((res (surface-resolution (current-surface))))
    (setf (viewport-resolution (current-viewport)) res)
    (as-frame
      (map-g #'pipe *bs*))))

(def-simple-main-loop runplay (:on-start #'initialize)
  (draw!))
