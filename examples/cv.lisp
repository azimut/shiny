(in-package :shiny)

(defvar *hsv* nil)
(defvar *rgb* nil)
(defvar *h* nil)
(defvar *w* nil)
(defvar *p* nil)
(defvar *fh* nil)

(defparameter *vevents* '())
(defclass to-sec ()
  ((sec :initarg :sec :initform 0f0)))
(defclass to-hsv ()
  ((secs :initarg :secs :initform 0f0)))

;; This needs (callback) from scheduler/cl-collider
(defgeneric update (obj capture fps seconds)
  (:method ((obj to-sec) capture fps seconds)
    (with-slots (sec) obj
      (cv:set-capture-property capture
                               cv:+cap-prop-pos-frame+
                               (* fps (min sec seconds)))))
  (:method ((obj to-hsv) capture fps seconds)
    (with-slots (secs) obj
      (setf *hsv* 1)
      (callback (+ (now) secs) #'(lambda () (setq *hsv* nil))))))


;; made a separate function instead of directly on the loop to be able to recompile on the fly. Using an infinite recursive function doesn't work either as a SBCL stack get's eventually overflow on these type of funtions.
(defun render (frame filename mat)
  (let ((mysize (cv:size 100 100)))
    (if (= (cv:wait-key 30) 27)
        'done
        (cv:with-ipl-images
            ((img mysize cv:+ipl-depth-8u+ 3)
             (cimg mysize cv:+ipl-depth-8u+ 1))
          ;;(cv:copy frame img)
          (cv:resize frame img)
          (cv:repeat img frame)
          ;;(cv:flip img img 1)
          ;;(cv:cvt-color img cimg cv:+bgr-2-gray+)
          ;; (cv:cvt-color img img cv:+bgr-2-rgb+)
          ;; (cv:cvt-color img img cv:+bgr-2-hsv+)
          ;; (and (= 0 (mod (get-universal-time) 10))
          ;;      (cv:flip img img 1))
;;          (2d-rotate mat 20 50 0f0 .7)
;;          (cv:warp-affine img img mat)
          (cv:show-image filename frame)))))

(defun show-video (filename)
  "Show the video in FILENAME in a window."
  (cv:with-named-window
      (filename (+ +window-freeratio+ +window-gui-normal+))
    (with-captured-file (capture filename)
      (let ((mat (cv:create-mat 2 3 cv:+32fc1+)))
        (loop
           (let ((frame (cv:query-frame capture)))
             ;; livecoding
             (update-swank)
             ;; check if we got a valid frame
             (if (not (cffi:null-pointer-p frame))
                 (when (eq 'done (render frame filename mat))
                   (return))
                 ;; repeat video ad-infinitum
                 (skip-to capture 0))))))))
