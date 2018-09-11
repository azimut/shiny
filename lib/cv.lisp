(in-package :cv)

(cffi:defcstruct ipl-image
  (n-size :int)
  (id :int)
  (n-channels :int)
  (alpha-channel :int)
  (depth :int)
  (color-model :char :count 4)
  (channel-seq :char :count 4)
  (data-order :int)
  (origin :int)
  (align :int)
  (width :int)
  (height :int)
  (roi :pointer)
  (mask-roi :pointer)
  (image-id :pointer)
  (tile-info :pointer)
  (image-size :int)
  (image-data :pointer)
  (width-step :int)  
  (border-mode :int :count 4)
  (border-const :int :count 4)
  (image-data-origin :pointer))

(in-package :shiny)

(defconstant +window-freeratio+ #x00000100)
(defconstant +window-gui-normal+ #x00000010)

(defmacro with-captured-file ((capture filename) &body body)
  `(let ((,capture (cv:create-file-capture ,filename)))
     (unwind-protect (progn ,@body)
       (cv:release-capture ,capture))))

(defmacro with-captured-files (captures &body body)
  (let ((vars (mapcar #'car captures)))
    `(let ,(mapcar (lambda (x) `(,(car x) (cv:create-file-capture ,@(cdr x))))
                   captures)
       (unwind-protect (progn ,@body)
         ,@(mapcar (lambda (x) `(cv:release-capture ,x))
                   vars)))))

(defmacro with-mem-storage ((mem-storage) &body body)
  `(let ((,mem-storage (cv:create-mem-storage)))
     (unwind-protect (progn ,@body)
       (cv:release-mem-storage ,mem-storage))))

;; (defmacro with-gui-thread (&body body)
;;   "Wraps BODY in code which masks float traps.
;;    This is needed in SBCL on OSX because native code often
;;    generate :inexact traps and land you in the debugger.
;;    For non SBCL this wraps body in a progn."
;;   `(trivial-main-thread:call-in-main-thread
;;     (lambda () 
;;       #+sbcl (sb-int:with-float-traps-masked (:invalid :divide-by-zero :overflow)
;; 	       ,@body)
;;       #+ccl (unwind-protect (progn
;; 			       (ccl:set-fpu-mode :invalid nil)
;; 			       ,@body)
;; 	       (ccl:set-fpu-mode :invalid t))
;;       #-(or sbcl ccl)
;;       ,@body)))

;;--------------------------------------------------

(defun skip-to (capture seconds)
  "skip video capture to seconds"
  (declare (type alexandria:non-negative-integer seconds)
           (type cffi:foreign-pointer capture))
  (cv:set-capture-property
   capture
   cv:+cap-prop-pos-msec+
   (round (* 1000 seconds))))

(defun current-pos (capture)
  "current pos in seconds"
  (* 1000f0
     (cv:get-capture-property
      capture
      cv:+cap-prop-pos-msec+)))

(defun make-point (l)
  "cv:point wrapper from l"
  (let* ((x (car  l))
         (y (cadr l))
         (point (cv:point x y)))
    point))

;;--------------------------------------------------
(defun make-tri ()
  (list (cv:point2d-32f 0 0)
        (cv:point2d-32f 0 0)
        (cv:point2d-32f 0 0)))

(defun make-quad ()
  (list (cv:point2d-32f 0 0)
        (cv:point2d-32f 0 0)
        (cv:point2d-32f 0 0)
        (cv:point2d-32f 0 0)))
;;--------------------------------------------------

(defun make-line (img x1 y1 x2 y2
                  &key (blue 0) (green 0) (red 0))
  "cv:line wrapper"
  (declare (type integer x1 y1 x2 y2 blue green red))
  (cv:line img
           (cv:point x1 y1)
           (cv:point x2 y2)
           (cv:scalar blue green red)))

(defun make-circle (img x y radius
                    &key (blue 0) (green 0) (red 0))
  "cv:circle wrapper"
  (declare (type integer x y radius blue green red))
  (cv:circle img
             (cv:point x y)
             radius
             (cv:scalar blue green red)))

(defun make-ellipse (img x y width height angle
                     &key (start-angle 360) (end-angle 0)
                       (blue 0) (green 0) (red 0))
  "cv:ellipse wrapper"
  (declare (type integer x y width height
                 angle start-angle end-angle
                 blue green red))
  (cv:ellipse img
              (cv:point x y)
              (cv:size width height)
              angle
              start-angle
              end-angle
              (cv:scalar blue green red 255)))

(defun make-rectangle
    (img x1 y1 x2 y2
     &key (blue 0) (green 0) (red 0))
  "cv:rectangle wrapper"
  (declare (type integer x1 y1 x2 y2 blue green red))
  (cv:rectangle img
                (cv:point x1 y1) (cv:point x2 y2)
                (cv:scalar blue green red)))

(defun make-text (img font text x y
                  &key (red 0) (green 0) (blue 0))
  "cv:put-text wrapper"
  (declare (type integer red green blue x y)
           (type string text))
  (cffi:with-foreign-string (s text)
    (cv:put-text img
                 s
                 (cv:point x y)
                 font
                 (cv:scalar red green blue))))

;;--------------------------------------------------

(defstruct cprops
  nframes
  fps
  seconds
  size
  width
  height)

(defun get-props (capture)
  (declare (cffi:foreign-pointer capture))
  "get all possible values from capture, for convenience"
  (let* ((nframes (round (cv:get-capture-property
                          capture
                          cv:+cap-prop-frame-count+)))
         (fps     (cv:get-capture-property
                   capture
                   cv:+cap-prop-fps+))
         (seconds (/ nframes fps))
         (frame   (cv:query-frame capture))
         (size    (cv:get-size frame))
         (width   (cv:width  size))
         (height  (cv:height size)))
    (make-cprops :nframes nframes
                 :fps fps
                 :seconds seconds
                 :size size
                 :height height :width width)))

(defun print-struct (struct)
  (declare (type cprops struct))
  (format t "~a~&" struct))

;;--------------------------------------------------

(defun 2d-rotate (mat center-x center-y &optional (angle 0f0) (scale 1f0))
  (declare (type integer center-x center-y)
           (type float angle scale))
  (cv:2d-rotation-matrix (cv:point2d-32f center-x center-y)
                         angle scale
                         mat))

(defun update-swank ()
  (let ((connection (or swank::*emacs-connection*
                        (swank::default-connection))))
    (when connection
      (swank::handle-requests connection t))))

(defun get-frame (capture &optional (restart-frame 0))
  (declare (type cffi:foreign-pointer capture)
           (type alexandria:non-negative-integer restart-frame))
  (let ((frame (cv:query-frame capture)))
    (if (cffi:null-pointer-p frame)
        (progn
          (skip-to capture restart-frame)
          (cv:query-frame capture))
        frame)))

(defvar *capture-index* 0)
(defun get-frame-captures (&rest captures)
  (let* ((capture (nth *capture-index* captures))
         (frame   (cv:query-frame capture)))
    (if (cffi:null-pointer-p frame)
        (cv:query-frame capture)
        frame)))
