(in-package :shiny)

(defun render (frame1 frame2)
  (let ((mysize (cv:size 200 200)))
    (if (= (cv:wait-key 30) 27)
        'done
        (cv:with-ipl-images
            ((img1 mysize cv:+ipl-depth-8u+ 3)
             (img2 mysize cv:+ipl-depth-8u+ 3)
             (img3 mysize cv:+ipl-depth-8u+ 3))
          (cv:resize frame1 img1)
          (cv:resize frame2 img2)
          (cv:add-weighted img1 .8 img2 (- 1 .5) 0f0 img3)          
          (cv:show-image "multi" img3)))))

(defun show-videos ()
  "Show the video in FILENAME in a window."
  (cv:with-named-window ("multi" (+ +window-freeratio+
                                    +window-gui-normal+))
    (with-captured-files
        ((capture1 "/home/sendai/testfield/spacecop.mp4")
         (capture2 "/home/sendai/testfield/spacecop.mp4"))
      (skip-to capture2 30)
      (loop
        (update-swank)
        (let ((frame1 (get-frame capture1))
              (frame2 (get-frame capture2)))
          (when (eq 'done (render frame1 frame2))
            (return)))))))
