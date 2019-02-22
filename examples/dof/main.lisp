(in-package :shiny)

(defvar *fbo* nil)
(defvar *sam* nil)
(defvar *bs* nil)
(defvar *sam-depth* nil)
(defvar *coc-fbo* nil)
(defvar *coc-sam* nil)
(defvar *dimensions* nil)
(defvar *texel-size* nil)

(defun initialize ()
  (when *coc-fbo*
    (free *coc-fbo*))
  ;;(setf *coc-fbo* (make-fbo '(0 :element-type :float)))
  (setf *coc-fbo* (make-fbo '(0 :element-type :r8)))
  (setf *coc-sam* (cepl:sample (attachment-tex *coc-fbo* 0)))
  ;;--------------------------------------------------
  ;; Buffer stream for single stage pipelines
  (unless *bs*
    (setf *bs* (make-buffer-stream nil :primitive :points)))
  ;;--------------------------------------------------
  ;; HDR fbo(s)
  (when *fbo*
    (free *fbo*))
  (setf *fbo* (make-fbo (list 0 :element-type :rgb16f)
                        :d))
  (setf *sam*
        (cepl:sample (attachment-tex *fbo* 0) :wrap :clamp-to-edge))
  (setf *sam-depth*
        (cepl:sample (attachment-tex *fbo* :d)))
  (setf (clear-color) (v! 0 0 0 1))
  (setf *dimensions* (dimensions (attachment-tex *fbo* 0)))
  ;; https://docs.unity3d.com/Manual/SL-PropertiesInPrograms.html
  (setf *texel-size* (v! (/ 1 (car *dimensions*))
                         (/ 1 (lastcar *dimensions*))))
  ;;--------------------------------------------------
  (setf *actors* nil)
  (make-celestial-sphere)
  (dotimes (i 20)
    (make-box (v! (+ -5 (random 10f0)) (+ -5 (random 10f0)) i)))
  (make-piso (v! 0 -2 0) (q:from-axis-angle (v! 1 0 0) (radians 30)))
  NIL)

(defun draw! ()  
  (let ((res (surface-resolution (current-surface))))
    (setf (resolution (current-viewport)) res)
    
    (update *currentcamera*)
    (update-all-the-things *actors*)
    
    (with-fbo-bound (*fbo*)
      (clear-fbo *fbo*)
      (loop :for actor :in *actors* :do
         (draw actor *currentcamera*)))
    
    (with-setf* ((depth-mask) nil
                 (cull-face) nil
                 (depth-test-function) #'always)
      (with-fbo-bound (*coc-fbo*)
        (map-g #'dof-pipe *bs*
               :tex *sam-depth*))
      (as-frame
        (map-g #'bokeh-pass *bs*
               :texel-size *texel-size*
               :scene *sam*)))
    
    
    ;; (as-frame
      
;;       ;; (with-setf* ((depth-mask) nil
;; ;;                    (cull-face) nil
;; ;;                    (clear-color) (v! 0 0 0 1)
;; ;;                    (depth-test-function) #'always)
;;         (map-g #'generic-2d-pipe *bs*
;;            :sam *sam*)
;; ;; )

;;         )
    ))
(def-simple-main-loop runplay (:on-start #'initialize)
  (draw!))

