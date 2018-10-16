(in-package :shiny)

;; sunken

(gmeplay-beat
 "/home/sendai/Downloads/chrono/111 Secret of the Forest.spc" 2 1
 :amp .5
 :aubio-p t
 :control-only t
;; :load-only t
 :length 20
 :offset 80
 :lpf 200
;; :right .2
;; :left .3
;; :voices '(4 5 6 7)
 :voices '(0 1 2 3)
 :rate .5)

(viseq:reset)
(viseq:push-cvideo
 :gondola
 "/home/sendai/clips/psy.mkv"
 :glitch nil
 :hsv nil
;; :stop-pos 1
:stop-pos most-positive-fixnum
:is-negative nil
:scale 1f0
 :rotation 0f0)

(progn
  
  (defparameter *beats*
   (remove-sides
    (test-onset (gethash
                 "/home/sendai/Downloads/chrono/111 Secret of the Forest.spc3"
                 *playing*))))

  (let* ((hsv (make-cycle '(t nil)))
         (buf
          (gethash
           "/home/sendai/Downloads/chrono/111 Secret of the Forest.spc3"
           *playing*))
         (sample-rate (buffer-sample-rate buf))
         (rate 1)
         (frames (buffer-frames buf))
         (dur (abs (* (/ 1 rate) (/ frames sample-rate)))))
    (defun f (time)
      (bplay buf :id 10 :amp .2 :rate rate :left .3 :lpf 400)
      (in-times *beats*
                (lambda () (viseq:push-cvideo
                       :gondola "/home/sendai/clips/psy.mkv"
                       :scale 1f0
                       :xpos 0
                       :ypos 0
                       :hsv (next hsv))))
      (aat (+ time #[dur b]) #'f it ))))

(f (now))

(let ((left (make-palindrome (iota 20 :start 0 :step .05))))
  (defun panning (time)
    (set-control 10 :left (next left))
    (set-control 10 :right (abs (1- (next left))))
    (aat (+ time #[.5 b]) #'panning it)))

(panning (now))
(defun panning ())

(defun f ())ff
(incudine:free (node 0))
(flush-pending)

(f (now))
