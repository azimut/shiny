(in-package :somecepl)

(defun-g vert ((vert :vec2))
  (v! vert 0 1))

(defun-g frag ()
  (v! 1 0 0 0))

(defpipeline-g mario ()
  :vertex   (vert :vec2)
  :fragment (frag))

(defun reset ())

(defun game-step ()
  (clear)
  (map-g #'mario (get-quad-stream-v2))
  (swap))

(def-simple-main-loop runplay (:on-start #'reset)
  (game-step))
