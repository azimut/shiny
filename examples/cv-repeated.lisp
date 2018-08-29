(in-package :shiny)

(defvar *factor* 2)
(defvar *single* 50)
;; made a separate function instead of directly on the loop to be able to recompile on the fly. Using an infinite recursive function doesn't work either as a SBCL stack get's eventually overflow on these type of funtions.
(defun render (frame filename mat)
  (let* ((mysize (cv:size 100 100))
         (factor *factor*)
         (single *single*)
         (doble  (* single factor)))
    (if (= (cv:wait-key 30) 27)
        'done
        (cv:with-ipl-images
            ((img1 (cv:size single single) cv:+ipl-depth-8u+ 3)
             (img2 (cv:size doble doble) cv:+ipl-depth-8u+ 3)
             (img3 mysize cv:+ipl-depth-8u+ 3)
             (cimg mysize cv:+ipl-depth-8u+ 1))
          ;;(cv:copy frame img)
          (cv:resize frame img1)
          (and (zmodt 2) (cv:cvt-color img1 img1 cv:+bgr-2-hsv+))
          (cv:repeat img1 img2)
          ;;(cv:flip img img 1)
          ;;(cv:cvt-color img cimg cv:+bgr-2-gray+)
          ;; (cv:cvt-color img img cv:+bgr-2-rgb+)
          ;; (and (= 0 (mod (get-universal-time) 10))
          ;;      (cv:flip img img 1))
;;          (2d-rotate mat 20 50 0f0 .7)
;;          (cv:warp-affine img img mat)
          (cv:show-image filename img2)))))

(defun show-video (filename)
  "Show the video in FILENAME in a window."
  (cv:with-named-window
      (filename (+ +window-freeratio+ +window-gui-normal+))
    (with-captured-file (capture filename)
      (let ((mat (cv:create-mat 2 3 cv:+32fc1+)))
        (loop
           (let ((frame (cv:query-frame capture))
;;                 (cprop (get-props capture))
                 )
             ;; livecoding
             (update-swank)
             ;; check if we got a valid frame
             (if (not (cffi:null-pointer-p frame))
                 (when (eq 'done (render frame filename mat))
                   (return))
                 ;; repeat video ad-infinitum
                 (skip-to capture 10))))))))
