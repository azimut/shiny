(in-package :somecepl)

(defconstant +window-freeratio+ #x00000100)
(defconstant +window-gui-normal+ #x00000010)

(defvar *status* nil)
(defvar *status2* nil)
(defvar *status3* nil)
(defvar *status4* nil)

(ql:quickload :trivial-main-thread)

(defmacro with-gui-thread (&body body)
  "Wraps BODY in code which masks float traps.
   This is needed in SBCL on OSX because native code often
   generate :inexact traps and land you in the debugger.
   For non SBCL this wraps body in a progn."
  `(trivial-main-thread:call-in-main-thread
    (lambda () 
      #+sbcl (sb-int:with-float-traps-masked (:invalid :divide-by-zero :overflow)
	       ,@body)
      #+ccl (unwind-protect (progn
			       (ccl:set-fpu-mode :invalid nil)
			       ,@body)
	       (ccl:set-fpu-mode :invalid t))
      #-(or sbcl ccl)
      ,@body)))

(defun status (time)
  (let ((cvalue (control-value 2 'rms))
        (ccvalue (control-value 3 'rms))
        (cccvalue (control-value 4 'rms)))


    (when (numberp cvalue)
      (if (> (abs (lin->db cvalue)) 90)
          (setf *status* 1)
          (setf *status* nil)))
    
    (when (numberp ccvalue)
      (if (> (abs (lin->db ccvalue)) 90)
          (setf *status2* (list (list (between 20 300) (between 40 200))
                                (list (between 20 300) (between 80 200))))
          (setf *status2* nil)))    

    (when (numberp cccvalue)
      (if (> (abs (lin->db cvalue)) 10)
          (progn
            (setf *status3* (list (between 20 300) (between 40 200)))
            (setf *status4* (* 30 (pick 10 20 30 40))))
          (setf *status3* nil)))
    ) 
  (at (+ time #[.1 b]) #'status (+ time #[.1 b])))

(defun status ())
(status (now))

(defmacro with-captured-files (captures &body body)
  (let ((vars (mapcar #'car captures)))
    `(let ,(mapcar (lambda (x) `(,(car x) (cv:create-file-capture ,@(cdr x))))
                   captures)
       (unwind-protect (progn ,@body)
         ,@(mapcar (lambda (x) `(cv:release-capture ,x))
                   vars)))))

(defun make-point (l)
  (let* ((x (car  l))
         (y (cadr l))
         (point (cv:point x y)))
    point))

(defun render (frame1 frame2 x y font capture1 capture2)
  (let ((mysize (cv:size 300 300)))
    (if (= (cv:wait-key 50) 27)
        'done
        (cv:with-ipl-images ((img1
                              mysize
                              cv:+ipl-depth-8u+
                              3)
                             (img2
                              mysize
                              cv:+ipl-depth-8u+
                              3)
                             (img3
                              mysize
                              cv:+ipl-depth-8u+
                              3)
                             (cimg
                              mysize
                              cv:+ipl-depth-8u+
                              1))
          ;;(cv:cvt-color img cimg cv:+bgr-2-gray+)
          (cv:resize frame1 img1)
          (cv:resize frame2 img2)
          ;; (and *status*
          ;;      (cv:flip img1 img1 1))          

          ;; (when *status4*
          ;;   (cv:set-capture-property
          ;;                 capture1
          ;;                 cv:+cap-prop-pos-frame+
          ;;                 *status4*))

          (unless *status*
            (cv:cvt-color img1 img1 cv:+bgr-2-hsv+))

          ;; (when *status*
          ;;   (cv:cvt-color img1 img1 cv:+bgr-2-hls+))
          
          (when *status3*
            (cffi:with-foreign-string (s "(PARENS)")
              (cv:put-text img2 s (make-point *status3*)
                           font (cv:scalar 20))))
          (when *status2*
            (let ((s *status2*))
              (cv:rectangle img1
                            (make-point (cadr s))
                            (make-point (car s))
                            (cv:scalar 1)))
            )
;;          (cv:cvt-color img1 img1 cv:+bgr-2-hsv+)
          (cv:add-weighted img1 .8 img2 (- 1 .5) 0f0 img3)
          (cv:show-image "multi" img1)
          ;;              (render capture filename nframes fps size w h)
          ))))




(defstruct cprops
  nframes
  fps
  seconds
  size
  width
  height)

(defun getprops (capture)
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


(defun show-videos ()
  "Show the video in FILENAME in a window."
  (with-gui-thread
    (cv:with-named-window ("multi" (+ +window-freeratio+
                                      +window-gui-normal+))
      (with-captured-files ((capture1 "/home/sendai/testfield/spacecop.mp4")
                            (capture2 "/home/sendai/testfield/spacecop.mp4"))
        (let* ((cprops1 (getprops capture1))
               ;;(cprops2 (getprops capture2))
               (connection (or swank::*emacs-connection*
                               (swank::default-connection))))
        
          (cv:set-capture-property
           capture2
           cv:+cap-prop-pos-frame+
           (* 30 30))

          (cffi:with-foreign-object (font :pointer)
            (cv:init-font font cv:+font-hershey-simplex+ 1f0 1f0)            
            (loop
               (block continue
                 (let ((frame1 (cv:query-frame capture1))
                       (frame2 (cv:query-frame capture2))
                       )
                   (when connection
                     (swank::handle-requests connection t))
                   (if (cffi:null-pointer-p frame1)
                       (progn
                         (cv:set-capture-property
                          capture1
                          cv:+cap-prop-pos-frame+
                          0)
                         (return-from continue)))

                   (if (cffi:null-pointer-p frame2)
                       (progn
                         (cv:set-capture-property
                          capture2
                          cv:+cap-prop-pos-frame+
                          (* 30 30))
                         (return-from continue)))
                   
                   (when (eq 'done (render frame1 frame2
                                           350
                                           350
                                           font
                                           capture1
                                           capture2))
                     (return))
                   ;; repeat video ad-infinitum
                   )))))))))
