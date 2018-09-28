(in-package :shiny)

(defun render (frame)
  (declare (type cffi:foreign-pointer frame))
  (let ((mysize (cv:size 50 50)))
    (if (= (cv:wait-key 30) 27)
        t
        (cv:with-ipl-images
            ((img1 mysize cv:+ipl-depth-8u+ 3))
          (cv:resize frame img1)
          (cv:show-image "multi" img1)))))

(defun show-videos ()
  "Show the video in FILENAME in a window."
  (cv:with-named-window
      ("multi" (+ +window-freeratio+ +window-gui-normal+))
    (with-captured-files
        ((capture1 "/home/sendai/testfield/spacecop.mp4")
         (capture2 "/home/sendai/testfield/plinkett.mp4"))
      (loop
         (update-swank)
         (when (render (get-frame-captures capture1 capture2))
           (return))))))

;;--------------------------------------------------
(fluidsynth:sfload *synth* "/home/sendai/Downloads/sf2/CTK-230_SoundFont.sf2" 1)

(let ((frame (make-cycle '(0 1))))
  (defun f (time)
    (setf *capture-index* (next frame))
    (p time (pickl (ov-scale :c4 :minor)) 10 2 0)
    (aat (+ time #[2 b]) #'f it)))

(fg 1f0)
(f (now))
(defun f ())


