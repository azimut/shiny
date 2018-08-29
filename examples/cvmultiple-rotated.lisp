(in-package :shiny)

(defparameter *phrase*
  (make-cycle '("don't" "you" "ever" "let" "me" "go")))

(defun render
    (frame1 frame2 mat)
  (let ((mysize (cv:size 200 200)))
    (if (= (cv:wait-key 30) 27)
        'done
        (cv:with-ipl-images
            ((img1 mysize cv:+ipl-depth-8u+ 3)
             (img2 mysize cv:+ipl-depth-8u+ 3)
             (img3 mysize cv:+ipl-depth-8u+ 3)
             (cimg mysize cv:+ipl-depth-8u+ 1))
          (cv:resize frame1 img1)
          (cv:resize frame2 img2)
          (2d-rotate mat 30 90 0d0 .5d0)
          (cv:warp-affine img1 img1 mat)
          (2d-rotate mat 70 120 0d0 .5d0)
          (cv:warp-affine img2 img2 mat)
          (cv:add-weighted img1 .8 img2 (- 1 .5) 0f0 img3)          
          (cv:show-image "multi" img3))
        )))

(defun show-videos ()
  "Show the video in FILENAME in a window."
  (cv:with-named-window ("multi" (+ +window-freeratio+
                                    +window-gui-normal+))
    (with-captured-files
        ((capture1 "/home/sendai/testfield/spacecop.mp4")
         (capture2 "/home/sendai/testfield/spacecop.mp4"))
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
             
               (when (eq 'done (render frame1 frame2 mat))
                 (return))
               ;; repeat video ad-infinitum
               )))))))
