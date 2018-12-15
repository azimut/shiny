(in-package :shiny)

(defvar *fbo* nil)
(defvar *sam* nil)
(defvar *sam1* nil)
(defvar *bs* nil)
(defvar *res* nil)
(defvar *blend* (make-blending-params))

(defvar *god-fbo* nil)
(defvar *god-sam* nil)

(defun initialize ()
  ;;--------------------------------------------------
  ;; Buffer stream for single stage pipelines
  (unless *bs* (setf *bs* (make-buffer-stream nil :primitive :points)))
  ;;--------------------------------------------------
  ;; HDR fbo(s)
  (when *fbo* (free *fbo*))
  (setf *fbo* (make-fbo (list 0 :element-type :rgb16f)
                        (list 1 :element-type :rgb16f)
                        :d))
  (setf *sam* (cepl:sample (attachment-tex *fbo* 0)
                           :wrap :clamp-to-edge))
  (setf *sam1* (cepl:sample (attachment-tex *fbo* 1)
                            :wrap :clamp-to-edge))
  ;;--------------------------------------------------
  (when *god-fbo* (free *god-fbo*))
  (setf *god-fbo* (make-fbo (list 0 :element-type :rgb16f)))
  (setf *god-sam* (cepl:sample (attachment-tex *god-fbo* 0)
                               :wrap :clamp-to-edge))
  (setf *res*  (dimensions (attachment-tex *fbo* 0)))
  (setf (clear-color) (v! 0 0 0 1))
  ;;--------------------------------------------------
  (setf *actors* nil)
  (make-box (v! 0 0 -10))
  (make-sphere (v! 0 10 -40) 1f0)
  ;; (dotimes (i 20)
  ;;   (make-box (v! (+ -10 (random 20f0))
  ;;                 (random 1f0)
  ;;                 (+ -10 (random 20f0)))
  ;;             (random 1f0)))
  (make-pbr (v! 0 -2 0))
  ;;(make-clouds)
  ;;(make-celestial-sphere)
  NIL)

(defun draw! ()
  (let* ((res (surface-resolution (current-surface)))
         (time (mynow))
         )
    
    (setf (resolution (current-viewport)) res)

    (update *currentcamera*)
    (update-all-the-things *actors*)
    
    (with-fbo-bound (*fbo*)
      (clear-fbo *fbo*)
      (loop :for actor :in *actors* :do
           (draw actor *currentcamera*)))
    (with-fbo-bound (*god-fbo*)
      (map-g #'god-rays-pipe *bs*
             :res res
             :time time
             :sam *sam1*
             ;; ... well FUCK IT!!! +30 sure
             :sun-pos (screen-coord res (v! 0 10 -40))))
    (as-frame
      (with-setf* ((depth-mask) nil
                   (cull-face) nil
                   (clear-color) (v! 0 0 0 1))        
        (map-g #'combine-god-pipe *bs*
               :sam *sam*
               :sam-god *god-sam*)))))

(def-simple-main-loop runplay
    (:on-start #'initialize)
  (draw!))

