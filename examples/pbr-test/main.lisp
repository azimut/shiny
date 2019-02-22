(in-package :shiny)

(defvar *fbo* nil)
(defvar *sam* nil)
(defvar *bs* nil)
(defvar *blend* (make-blending-params))

(defun initialize ()
  ;;--------------------------------------------------
  ;; Buffer stream for single stage pipelines
  (unless *bs* (setf *bs* (make-buffer-stream nil :primitive :points)))
  ;;--------------------------------------------------
  ;; HDR fbo(s)
  (when *fbo*
    (free *fbo*))
  (setf *fbo* (make-fbo (list 0 :element-type :rgb16f)
                        :d))
  (setf *sam* (cepl:sample (attachment-tex *fbo* 0)
                           :wrap :clamp-to-edge))
  (setf (clear-color) (v! 0 0 0 1))
  (setf *actors* nil)
  (make-pbr)
  ;;(make-celestial-sphere)
  ;; (dotimes (i 40)
  ;;   (make-pbr (v! (+ -10 (random 15f0))
  ;;                 (+ -2 (random 4f0))
  ;;                 (+ -10 (random 20f0)))))
  ;;(make-piso (v! 0 -2 0))
  ;; (make-piso (v! 0  2 0)
  ;;            (q:from-axis-angle (v! 1 0 0) (radians 180)))
  (%gl::sampler-parameter-f
   (%cepl.types::%sampler-id (get-tex "static/checker.dds"))
   :texture-max-anisotropy-ext 1f0)
  NIL)

(defun draw! ()
  (let ((res (surface-resolution (current-surface)))
        (time (mynow)))
    (setf (resolution (current-viewport)) res)
    (setf *pointlight-pos* (v! (* 4 (sin time))
                               (* 4 (cos time))
                               0))
    (update *currentcamera*)
    (update-all-the-things *actors*)
    
    (with-fbo-bound (*fbo*)
      (clear-fbo *fbo*)
      (loop :for actor :in *actors* :do
         (draw actor *currentcamera*))
      ;; (with-blending *blend*
      ;;   (map-g #'billboard-pipe *bs*
      ;;          :camera-pos (pos *currentcamera*)
      ;;          :tex (get-tex "static/float-alpha.png")
      ;;          :time (mynow)
      ;;          :world-view (world->view *currentcamera*)
      ;;          :view-clip (projection *currentcamera*)))
      )

    (as-frame
      (with-setf* ((depth-mask) nil
                   (cull-face) nil
                   (clear-color) (v! 0 0 0 1)
                   (depth-test-function) #'always)
        (map-g #'generic-2d-pipe *bs*
           :sam *sam*)))
    ))
(def-simple-main-loop runplay
    (:on-start #'initialize)
  (draw!))

