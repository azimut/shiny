(in-package #:shiny)

;;--------------------------------------------------
;; PARTICLE LOGIC
;;--------------------------------------------------
;; Uses TTFS to delegate to the gpu all the movement logic.
;; Needs *bs* for the single stage pipeline.
;; TODO:
;; - Instacing for geomtry rendending of particles
;; - Create CL class to contain all the components nicely
;;   and more importantly easily allow multiple particle systems.

(defvar *gar-src* NIL)
(defvar *str-src* NIL)
(defvar *tfs-src* NIL)

(defvar *gar-dst* NIL)
(defvar *str-dst* NIL)
(defvar *tfs-dst* NIL)

(defstruct-g pdata
  (pos :vec3)
  (dir :vec3)
  (life :float))

;;--------------------------------------------------
;; Init particles

(defun-g pinit-vert (&uniform (time :float))
  (values (v! 0 0 0 0)
          (:feedback (v! 0 0 0))
          (:feedback (v! 0 0 0))
          (:feedback (nineveh.random:rand
                      (v! gl-vertex-id
                          (* time gl-vertex-id))))))

(defpipeline-g pinit-pipe (:points) :vertex (pinit-vert))
(defun init-particles (&optional (n-particles 1000))
  (unless *gar-src*
    (setf *gar-src* (make-gpu-array
                     nil
                     :dimensions n-particles
                     :element-type 'pdata)
          *gar-dst* (make-gpu-array
                     nil
                     :dimensions n-particles
                     :element-type 'pdata))
    (setf *str-src* (make-buffer-stream *gar-src* :primitive :points)
          *str-dst* (make-buffer-stream *gar-dst* :primitive :points))
    (setf *tfs-src* (make-transform-feedback-stream *gar-src*)
          *tfs-dst* (make-transform-feedback-stream *gar-dst*))
    (reset-particles)))

;;--------------------------------------------------
;; Free & Reset

(defun free-particles ()
  (free *tfs-src*)
  (free *tfs-dst*)
  (free *str-src*)
  (free *str-dst*)
  (free *gar-src*)
  (free *gar-dst*))

(defun reset-particles ()
  (with-transform-feedback (*tfs-src*)
    (map-g #'pinit-pipe *bs*))
  (with-transform-feedback (*tfs-dst*)
    (map-g #'pinit-pipe *bs*))
  (values))

;;--------------------------------------------------
;; Update particles

(defparameter *distance* 0f0)
(defparameter *offset* 1f0)
(defun-g pupdate-vert ((pdata pdata)
                       &uniform
                       (time :float))
  (with-slots (pos dir life) pdata
    (let* ((time    (* time (* *offset* gl-vertex-id)))
           (newlife (+ life .01)))
      (values (v! 0 0 0 0)
              (:feedback (+ (* .9 pos)
                            (v! (cos time) (sin time) 0f0)))
              (:feedback dir)
              (:feedback (if (>= newlife 3f0)
                             0f0
                             newlife))))))
(defpipeline-g pupdate-pipe (:points) :vertex (pupdate-vert pdata))
(defun update-particles ()
  (with-transform-feedback (*tfs-dst*)
    (map-g #'pupdate-pipe *str-src*
           :time (mynow))))
(defun swap-particles ()
  (rotatef *tfs-src* *tfs-dst*)
  (rotatef *str-src* *str-dst*)
  (rotatef *gar-src* *gar-dst*))

;;--------------------------------------------------
;; PARTICLE DRAW/RENDER
;;--------------------------------------------------
;; All I tried so far is either A) drawing the particles as points B)
;; drawing the particles as points and then have a geometry shader
;; convert them to billboards. It is possible draw
;; triangles(geometries) using instancing. But might be the code above
;; needs change too.

;; A) POINTS

(defun-g prender-points-vert ((pdata pdata)
                              &uniform
                              (world-clip :mat4))
  (with-slots (pos) pdata
    (let* ((world-pos (v! pos 1))
           (clip-pos  (* world-clip world-pos)))
      clip-pos)))

(defun-g prender-points-frag ()
  (v! 1 1 1 1))

(defpipeline-g prender-points-pipe (:points)
  :vertex   (prender-points-vert pdata)
  :fragment (prender-points-frag))

(gl:point-size 4)
(defun draw-particles-points ()
  (map-g #'prender-points-pipe *str-src*
         :world-clip (world->clip *currentcamera*)))

;; B) POINTS -> BILLBOARDS

(defun-g billboard-vert ((pdata pdata)
                         &uniform
                         (world-view :mat4))
  (with-slots (pos life) pdata
    (values (* world-view (v! pos 1))
            life)))

(defun-g billboard-geom ((life (:float 1))
                         &uniform
                         (camera-pos :vec3)
                         (view-clip :mat4))
  (declare (output-primitive :kind :triangle-strip :max-vertices 4))
  (when (< (aref life 0) 1f0)
    (let* ((p (s~ (gl-position (aref gl-in 0)) :xyz))
           (to-camera (normalize (- camera-pos p)))
           ;;(to-camera (v! (x to-camera) 0 (z to-camera)))
           (up (v! 0 1 0))
           (right (cross to-camera up)))
      ;;
      (decf p (* .5 right))
      (emit ()
            (* view-clip (v! p 1))
            (v! 0 0))
      ;;
      (incf (y p) 1f0)
      (emit ()
            (* view-clip (v! p 1))
            (v! 0 1))
      ;;
      (decf (y p) 1f0)
      (incf p right)
      (emit ()
            (* view-clip (v! p 1))
            (v! 1 0))
      ;;
      (incf (y p) 1f0)
      (emit ()
            (* view-clip (v! p 1))
            (v! 1 1))
      (end-primitive)
      (values))))

(defun-g billboard-frag ((uv :vec2))
  (v! 1 1 1 1))

(defpipeline-g billboard-pipe (:points)
  :vertex   (billboard-vert pdata)
  :geometry (billboard-geom (:float 1))
  :fragment (billboard-frag :vec2))

(defun draw-particles ()
  (map-g #'billboard-pipe *str-src*
         :world-view (world->view *currentcamera*)
         :view-clip  (projection *currentcamera*)))
