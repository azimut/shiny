(in-package :somecepl)

(defconstant +window-freeratio+ #x00000100)
(defconstant +window-gui-normal+ #x00000010)
(defvar *hsv* nil)
(defvar *rgb* nil)
(defvar *h* nil)
(defvar *w* nil)
(defvar *p* nil)

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

(defmacro with-captured-file ((capture filename) &body body)
  `(let* ((,capture (cv:create-file-capture ,filename)))
     (unwind-protect (progn ,@body)
       (cv:release-capture ,capture))))

;; made a separate function instead of directly on the loop to be able to recompile on the fly. Using an infinite recursive function doesn't work either as a SBCL stack get's eventually overflow on these type of funtions.
(defun render (frame filename nframes fps size w h)
  (let ((mysize (cv:size (/ (if *h* *h* h) 2) (/ (if *w* *w* w) 2))))
    (if (= (cv:wait-key 30) 27)
        'done
        (when *p*
          (cv:with-ipl-images ((img
                                mysize
                                cv:+ipl-depth-8u+
                                3)
                               (cimg
                                mysize
                                cv:+ipl-depth-8u+
                                1))
            ;;            (cv:copy frame img)
            (cv:resize frame img)
            (cv:sobel img img 1 0 -3)
          
            ;;(cv:cvt-color img cimg cv:+bgr-2-gray+)
            (and *hsv* (cv:cvt-color img img cv:+bgr-2-hsv+))
            ;;(and *rgb* (cv:cvt-color img img cv:+bgr-2-rgb+))
          
            ;;(cv:canny img cimg 100.0d0 200.0d0 3)            
            (cv:show-image filename img)
            ;;              (render capture filename nframes fps size w h)
            )))))


(defun show-video (filename)
  "Show the video in FILENAME in a window."
  (setf *hsv* nil
        *rgb* nil
        *p* nil
        *h* nil *w* nil
        *vevents* nil)
  (cv:with-named-window (filename (+ +window-freeratio+
                                     +window-gui-normal+))
    (with-captured-file (capture filename)
      (let* ((nframes (round (cv:get-capture-property
                              capture
                              cv:+cap-prop-frame-count+)))
             (fps    (cv:get-capture-property
                      capture
                      cv:+cap-prop-fps+))
             (seconds (/ nframes fps))
             (tframe  (cv:query-frame capture))
             (size   (cv:get-size tframe))
             (width  (cv:width size))
             (height (cv:height size))
             (connection (or swank::*emacs-connection*
                         (swank::default-connection))))
        (format t "Frames:~a FPS:~a Resolution:~ax~a"
                nframes fps width height)
        (loop
           (let ((frame (cv:query-frame capture)))
             (when connection
               (swank::handle-requests connection t))
             (when *vevents*
               (update (pop *vevents*) capture fps seconds))
             (if (not (cffi:null-pointer-p frame))
                 (when (eq 'done (render frame filename nframes
                                         fps size width height))
                   (return))
                 (cv:set-capture-property capture
                                          cv:+cap-prop-pos-frame+
                                          0))))))))
