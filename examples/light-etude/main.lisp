(in-package :shiny)

(defvar *dimensions-fbo* nil)
(defvar *fbo* nil)
(defvar *sam* nil)
(defvar *sam1* nil)
(defvar *mesh* nil)
(defvar *mesh-light* nil)
(defvar *ass* nil)
(defvar *ass-light* nil)
(defvar *bs* nil)

(defvar *fbo-secondary* nil)
(defvar *sam-secondary* nil)

(defvar *fbo-terciary* nil)
(defvar *sam-terciary* nil)

(defvar *half-fbo* nil)
(defvar *fourth-fbo* nil)
(defvar *eighth-fbo* nil)
(defvar *sixteen-fbo* nil)

(defvar *sam-half-fbo* nil)
(defvar *sam-fourth-fbo* nil)
(defvar *sam-eighth-fbo* nil)
(defvar *sam-sixteen-fbo* nil)

(defparameter *blend*
  (make-blending-params :source-rgb :one
                        :destination-rgb :one))

(defun initialize ()
  ;; Buffer stream for single stage pipelines
  (unless *bs* (setf *bs* (make-buffer-stream nil :primitive :points)))
  (setf *mesh*
        (elt
         (slot-value
          (ai:import-into-lisp
           "/home/sendai/untitled.3ds"
           :processing-flags '(:ai-process-triangulate
                               :ai-process-calc-tangent-space))
          'ai:meshes)
         0))
  (setf *mesh-light*
        (elt
         (slot-value
          (ai:import-into-lisp
           "/home/sendai/untitled-light.3ds"
           :processing-flags '(:ai-process-triangulate
                               :ai-process-calc-tangent-space))
          'ai:meshes)
         0))
  (when *ass* (cepl:free (slot-value *ass* 'buf)))
  (when *ass-light* (cepl:free (slot-value *ass-light* 'buf)))
  (setf *ass*
        (make-instance 'assimp-thing
;;                       :pos (v! 4 0 4)
                       :rot (q:from-axis-angle (v! 1 0 0)
                                       (radians -90))
                       :buf (assimp-mesh-to-stream *mesh*)))
  (setf *ass-light*
        (make-instance 'assimp-bloom
;;                       :pos (v! 4 0 4)
                       :rot (q:from-axis-angle (v! 1 0 0)
                                       (radians -90))
                       :buf (assimp-mesh-to-stream *mesh-light*)))
  ;; HDR fbo
  (when *fbo* (free *fbo*))
  (setf *fbo* (make-fbo (list 0 :element-type :rgba16f)
                        (list 1 :element-type :rgba16f)
                        :d))
  (setf *dimensions-fbo* (dimensions (attachment-tex *fbo* 0)))
  (setf *sam* (cepl:sample (attachment-tex *fbo* 0)))
  (setf *sam1* (cepl:sample (attachment-tex *fbo* 1)))
  ;; Sec FBO
  (setf *fbo-secondary*
        (make-fbo (list 0 :element-type :rgba16f)))
  (setf *sam-secondary*
        (cepl:sample (attachment-tex *fbo-secondary* 0)))
  (setf *fbo-terciary*
        (make-fbo (list 0 :element-type :rgba16f)))
  (setf *sam-terciary*
        (cepl:sample (attachment-tex *fbo-terciary* 0)))
  ;;--------------------------------------------------
  ;; BLUR
  (when *half-fbo*
    (free *half-fbo*)
    (free *fourth-fbo*)
    (free *eighth-fbo*)
    (free *sixteen-fbo*)
    (free *sixteen-fbo-2*))
  (flet ((f (d) (mapcar (lambda (x) (round (/ x d)))
                        *dimensions-fbo*)))
    (setf *half-fbo* (make-fbo
                      (list 0 :element-type :rgba16f :dimensions (f 2))))
    (setf *sam-half-fbo* (cepl:sample (attachment-tex *half-fbo* 0)))
    (setf *fourth-fbo* (make-fbo
                      (list 0 :element-type :rgba16f :dimensions (f 4))))
    (setf *sam-fourth-fbo* (cepl:sample (attachment-tex *fourth-fbo* 0)))
    (setf *eighth-fbo* (make-fbo
                      (list 0 :element-type :rgba16f :dimensions (f 8))))
    (setf *sam-eighth-fbo* (cepl:sample (attachment-tex *eighth-fbo* 0)))
    (setf *sixteen-fbo* (make-fbo
                      (list 0 :element-type :rgba16f :dimensions (f 16))))
    (setf *sam-sixteen-fbo* (cepl:sample (attachment-tex *sixteen-fbo* 0))))
  ;;--------------------------------------------------
  ;;(setf (clear-color) (v! .2 .2 .9 0))
  (setf (clear-color) (v! 1 1 1 1))
  (setf *actors* nil)
  ;;(make-box)
  (make-cement)
  (push *ass* *actors*)
  (push *ass-light* *actors*)
  ;;(make-sphere)
  ;;(make-piso)
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
                 (clear-color) (v! 0 0 0 1)
                 (depth-test-function) #'always)
      (with-fbo-bound (*half-fbo*)
        (clear-fbo *half-fbo*)
        (map-g #'bloom-pipe *bs*
               :sam *sam1*
               :x (/ 1f0 (car *dimensions-fbo*))
               :y (/ 1f0 (car (last *dimensions-fbo*)))
               :delta 1f0))
      (with-fbo-bound (*fourth-fbo*)
        (clear-fbo *fourth-fbo*)
        (map-g #'bloom-pipe *bs*
               :sam *sam-half-fbo*
               :x (/ 1f0 (* .5 (car *dimensions-fbo*)))
               :y (/ 1f0 (* .5 (car (last *dimensions-fbo*))))
               :delta 1f0))
      (with-fbo-bound (*eighth-fbo*)
        (clear-fbo *eighth-fbo*)
        (map-g #'bloom-pipe *bs*
               :sam *sam-fourth-fbo*
               :x (/ 1f0 (* .5 (* .5 (car *dimensions-fbo*))))
               :y (/ 1f0 (* .5 (* .5 (car (last *dimensions-fbo*)))))
               :delta 1f0))
      (with-fbo-bound (*sixteen-fbo*)
        (clear-fbo *sixteen-fbo*)
        (map-g #'bloom-pipe *bs*
               :sam *sam-eighth-fbo*
               :x (/ 1f0 (* .5 (* .5 (* .5 (car *dimensions-fbo*)))))
               :y (/ 1f0 (* .5 (* .5 (* .5 (car (last *dimensions-fbo*))))))
               :delta 1f0))
      ;; END downscale
      (with-blending *blend*
        (with-fbo-bound (*eighth-fbo*)
          (clear-fbo *eighth-fbo*)
          (map-g #'bloom-pipe *bs*
                 :sam *sam-sixteen-fbo*
                 :x (/ 1f0 (* .5 (* .5 (* .5 (car *dimensions-fbo*)))))
                 :y (/ 1f0 (* .5 (* .5 (* .5 (car (last *dimensions-fbo*))))))
                 :delta .5)))
      (with-blending *blend*
        (with-fbo-bound (*fourth-fbo*)
          (clear-fbo *fourth-fbo*)
          (map-g #'bloom-pipe *bs*
                 :sam *sam-eighth-fbo*
                 :x (/ 1f0 (* .5 (* .5 (car *dimensions-fbo*))))
                 :y (/ 1f0 (* .5 (* .5 (car (last *dimensions-fbo*)))))
                 :delta .5)))
      (with-blending *blend*
        (with-fbo-bound (*half-fbo*)
          (clear-fbo *half-fbo*)
          (map-g #'bloom-pipe *bs*
                 :sam *sam-fourth-fbo*
                 :x (/ 1f0 (* .5 (car *dimensions-fbo*)))
                 :y (/ 1f0 (* .5 (car (last *dimensions-fbo*))))
                 :delta .5)))
      (with-blending *blend*
        (with-fbo-bound (*fbo-secondary*)
          (clear-fbo *fbo-secondary*)
          (map-g #'bloom-pipe *bs*
                 :sam *sam-half-fbo*
                 :x (/ 1f0 (car *dimensions-fbo*))
                 :y (/ 1f0 (car (last *dimensions-fbo*)))
                 :delta .5)))
      (with-fbo-bound (*fbo-terciary*)
        (map-g #'dobloom-pipe *bs*
               :sam *sam*
               :light-sam *sam-secondary*
               :delta .5
               :x (/ 1f0 (car *dimensions-fbo*))
               :y (/ 1f0 (car (last *dimensions-fbo*))))))
    (as-frame

      (map-g #'generic-2d-pipe *bs*
             :sam *sam-terciary*)
      )))

(def-simple-main-loop runplay
    (:on-start #'initialize)
  (draw!))

