(in-package :shiny)

(defvar *samd* nil)
(defvar *fbo* nil)
(defvar *sam* nil)
(defvar *bs* nil)
(defvar *res* nil)
(defvar *res4* nil)

(defvar *fbo4-ping* nil)
(defvar *sam4-ping* nil)
(defvar *fbo4-pong* nil)
(defvar *sam4-pong* nil)

(defun initialize ()
  ;;--------------------------------------------------
  ;; Buffer stream for single stage pipelines
  (unless *bs* (setf *bs* (make-buffer-stream nil :primitive :points)))
  ;;--------------------------------------------------
  ;; HDR fbo(s)
  (when *fbo* (free *fbo*))
  (when *fbo4-ping* (free *fbo4-ping*) (free *fbo4-pong*))
  (setf *fbo* (make-fbo (list 0 :element-type :rgb16f)
                        :d))
  (setf *sam* (cepl:sample (attachment-tex *fbo* 0)
                           :wrap :clamp-to-edge))
  (setf *samd* (cepl:sample (attachment-tex *fbo* :d)
                            :wrap :clamp-to-edge))
  ;;--------------------------------------------------
  (setf *res*  (dimensions (attachment-tex *fbo* 0)))
  (setf *res4* (mapcar (lambda (x) (round (/ x 4))) *res*))
  (setf *fbo4-ping* (make-fbo (list 0 :dimensions *res4*)))
  (setf *sam4-ping* (cepl:sample (attachment-tex *fbo4-ping* 0)
                                 :wrap :clamp-to-edge))
  (setf *fbo4-pong* (make-fbo (list 0 :dimensions *res4*)))
  (setf *sam4-pong* (cepl:sample (attachment-tex *fbo4-pong* 0)
                                 :wrap :clamp-to-edge))
  (setf (clear-color) (v! 0 0 0 1))
  ;;--------------------------------------------------
  (setf *actors* nil)
  (make-box (v! 0 0 -10))
  ;; (dotimes (i 20)
  ;;   (make-box (v! (+ -10 (random 20f0))
  ;;                 (random 1f0)
  ;;                 (+ -10 (random 20f0)))
  ;;             (random 1f0)))
  (make-pbr (v! 0 -2 0))
  ;;(make-clouds)
  (make-celestial-sphere)
  NIL)

(defun draw! ()
  (let* ((res (surface-resolution (current-surface)))
         (time (mynow))
         ;;(sun-position (v2:*s res .5))
         (sun-position
          (v! (* (x res)
                 (+ .5 (* .5 (sin time))))
              (* (y res)
                 (+ .5 (* .5 (cos time)))))))
    
    (setf (resolution (current-viewport)) res)
    (setf *pointlight-pos* (v! (* 4 (sin time))
                               (* 4 (cos time))
                               0))
    (update *currentcamera*)
    (update-all-the-things *actors*)
    
    (with-fbo-bound (*fbo*)
      (clear-fbo *fbo*)
      (loop :for actor :in *actors* :do
           (draw actor *currentcamera*)))

    (with-fbo-bound (*fbo4-ping*)
      (clear-fbo *fbo4-ping*)
      (map-g #'god-rays-pipe *bs*
             :t-input *samd*
             :pass 3f0
             :sun-position sun-position))
    ;; (with-fbo-bound (*fbo4-pong*)
    ;;   (clear-fbo *fbo4-pong*)
    ;;   (map-g #'god-rays-pipe *bs*
    ;;          :t-input *sam4-ping*
    ;;          :pass 2f0
    ;;          :sun-position sun-position))
    ;; (with-fbo-bound (*fbo4-ping*)
    ;;   (clear-fbo *fbo4-ping*)
    ;;   (map-g #'god-rays-pipe *bs*
    ;;          :t-input *sam4-pong*
    ;;          :pass 3f0
    ;;          :sun-position sun-position))

    ;;var stepLen = filterLen * Math.pow( TAPS_PER_PASS, - pass );
    ;;    stepLen = filterLen * Math.pow( TAPS_PER_PASS, - pass );
    (as-frame
      (with-setf* ((depth-mask) nil
                   (cull-face) nil
                   (clear-color) (v! 0 0 0 1)
                   (depth-test-function) #'always)        
        (map-g #'generic-2d-pipe *bs*
               :sam *sam4-ping*)))))

(def-simple-main-loop runplay
    (:on-start #'initialize)
  (draw!))

