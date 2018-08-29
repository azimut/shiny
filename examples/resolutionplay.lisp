(in-package :shiny)

(defun-g vert ((vert :vec2))
  (v! vert 0 1))

(defun-g frag (&uniform
               (resolution :vec2)
               (time :float))
  (let* ((c (s~ gl-frag-coord :xy))
         (cc (/ time 100))
         (c (* c (mod cc 10)))
         (r (/ c resolution)))
    (v3! (* (x r) (y r)))))

(defpipeline-g mario ()
  :vertex   (vert :vec2)
  :fragment (frag))

(defun reset ())

(defun mynow ()
  (* .1 (get-internal-real-time)))-

(defun game-step ()
  (clear)
  (map-g #'mario (get-quad-stream-v2)
         :time (mynow)
         :resolution (viewport-resolution
                      (current-viewport)))
  (swap))

(def-simple-main-loop runplay (:on-start #'reset)
  (game-step))
