(in-package :shiny)

(defparameter *factor* 1)
(defparameter *single* 50)
(defparameter *hsv* nil)
(defparameter *size* .01)
(defparameter *stop* nil)

;; Simple show grid of 1st video
(defun render
    (frame1 frame2 capture1 capture2 mat)
  (let* ((single *single*)
         (factor *factor*)         
         (doble  (* single factor))
         )
    (if (= (cv:wait-key 30) 27)
        'done
        (cv:with-ipl-images
            ((small (cv:size single single) cv:+ipl-depth-8u+ 3)
             (big   (cv:size doble doble) cv:+ipl-depth-8u+ 3))
          ;; grid
          (cv:resize frame1 small)
          (and *hsv* (cv:cvt-color small small cv:+bgr-2-hsv+))
          (cv:repeat small big)
          (cv:show-image "multi" big)))))

(defun render
    (frame1 frame2 capture1 capture2 mat)
  (let* ((single *single*)
         (factor *factor*)         
         (doble  (* single factor)))
    (if (= (cv:wait-key 30) 27)
        'done
        (cv:with-ipl-images
            ((small (cv:size single single) cv:+ipl-depth-8u+ 3)
             (big   (cv:size doble doble) cv:+ipl-depth-8u+ 3)
             (big2  (cv:size doble doble) cv:+ipl-depth-8u+ 3)
             (big3  (cv:size doble doble) cv:+ipl-depth-8u+ 3))
          ;; grid
          (cv:resize frame1 small)
          (and *hsv* (cv:cvt-color small small cv:+bgr-2-hsv+))
          (cv:repeat small big)
          ;; center
          (2d-rotate mat 50 50 0f0 *size*)
          (cv:resize frame2 big2)
          (cv:warp-affine big2 big2 mat)
          (cv:add-weighted big .7 big2 1 1 big3)
          (when *stop*
            (skip-to capture2 0)
            (skip-to capture1 0))
;;          (cv:add-weighted big .1 big2 .1 .2 big3)

;;          (cv:add-weighted big .1 big2 1f0 .1 big3)
          (cv:show-image "multi" big3)))))

(defun show-videos ()
  "Show the video in FILENAME in a window."
  (cv:with-named-window ("multi" (+ +window-freeratio+
                                    +window-gui-normal+))
    (with-captured-files
        ((capture1 "/home/sendai/abyss.mkv")
         (capture2 "/home/sendai/chelsea.mp4"))
      (let ((mat (cv:create-mat 2 3 5)))
        (skip-to capture2 30)
        (loop
           (update-swank)
           (block continue
             (let ((frame1 (cv:query-frame capture1))
                   (frame2 (cv:query-frame capture2)))
               
               (if (cffi:null-pointer-p frame1)
                   (progn
                     (skip-to capture1 0)
                     (return-from continue)))

               (if (cffi:null-pointer-p frame2)
                   (progn
                     (skip-to capture2 30)
                     (return-from continue)))
             
               (when (eq 'done (render frame1 frame2 capture1 capture2 mat))
                 (return)))))))))
