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

(defvar *blend* (make-blending-params))

(defstruct-g pdata
  (pos :vec3)
  (dir :vec3)
  (life :float))

;;--------------------------------------------------
;; Init particles

(defun-g pinit-vert (&uniform (time :float))
  (let ((id (* .001 (+ time gl-vertex-id))))
    (values (v! 0 0 0 0)
            (:feedback (v! 0 0 0))
            (:feedback (v! 0 0 0))
            (:feedback 0f0))))

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
  ;; (free *tfs-src*)
  ;; (free *tfs-dst*)
  (free *str-src*)
  (free *str-dst*)
  (free *gar-src*)
  (free *gar-dst*)
  (setf *str-src* NIL
        *str-dst* NIL
        *gar-src* NIL
        *gar-dst* NIL))

(defun reset-particles ()
  (with-transform-feedback (*tfs-src*)
    (map-g #'pinit-pipe *bs*))
  (with-transform-feedback (*tfs-dst*)
    (map-g #'pinit-pipe *bs*))
  (values))

;;--------------------------------------------------
;; Update particles

;; - Fudge initial alpha
;; - Color overtime to fade in/out with the alpha
;; - random init ROTATION
;; - ROTATION over lifetime
;; - random init SPRITE (static per life)
;; - change SPRITE over lifetime
;; - move over lifetime


;; - Scale X
(defparameter *distance* 0f0)
(defparameter *offset* 1f0)
(defun-g pupdate-vert ((pdata pdata)
                       &uniform
                       (time :float))
  (with-slots (pos dir life) pdata
    (let* ((time (* time .2 (* 2f0 gl-vertex-id)))
           (life life)
           (new-life (+ life .001))
           (dir dir)
           (pos pos)
           (r (rand (vec2 time))))
      (if (>= new-life 1f0)
          (progn ;; Reset
            (setf dir  (v! (* 360 r) ;; rot
                           (+ 4 (* 5 r)) ;; scale
                           0))
            (setf life (* .5 r))
            (setf pos  (v! (+ -2.5 (* 15 (rand (vec2 (* 3 time)))))
                           -4f0
                           (+ -8 (- (* 5 r))))))
          (progn
            (setf life new-life)
            (incf (x pos) .05)
            (decf (z pos) -.05)))
      (values (v! 0 0 0 0)
              (:feedback pos)
              (:feedback dir)
              (:feedback life)))))
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
  (with-slots (pos life dir) pdata
    (values (* world-view (v! pos 1))
            life
            dir)))

(defun-g billboard-geom ((life (:float 1))
                         (rot  (:vec3 1))
                         &uniform
                         (camera-pos :vec3)
                         (view-clip :mat4))
  (declare (output-primitive :kind :triangle-strip :max-vertices 4))
  (when (< (aref life 0) .5f0)
    (let* ((p (s~ (gl-position (aref gl-in 0)) :xyz))
           (to-camera (normalize (- camera-pos p)))
           (up (v! 0 1 0))
           (right (cross to-camera up))
           (life (aref life 0))
           (scale (y (aref rot 0))))
      ;;
      (decf p (/ right (* scale 2)))
      (emit ()
            (* view-clip (v! p 1))
            (v! 0 0)
            life)
      ;;
      (incf (y p) scale)
      (emit ()
            (* view-clip (v! p 1))
            (v! 0 1)
            life)
      ;;
      (decf (y p) scale)
      (incf p (* scale right))
      (emit ()
            (* view-clip (v! p 1))
            (v! 1 0)
            life)
      ;;
      (incf (y p) scale)
      (emit ()
            (* view-clip (v! p 1))
            (v! 1 1)
            life)
      (end-primitive)
      (values))))

(defun-g billboard-frag ((uv :vec2)
                         (life :float)
                         &uniform
                         (time :float)
                         (tex :sampler-2d))
  (let* ((sprites 8)
         (uv (/ uv sprites))
         (color (texture tex uv)))
    (v! (* ;;(s~ color :xyz)
         ;;(v! .18 .17843138 .1552941)
         (* .9 (v! 0.6392157 0.54901963 0.34509805)))
        (saturate (* (- .5 life) (w color))))))

(defpipeline-g billboard-pipe (:points)
  :vertex   (billboard-vert pdata)
  :geometry (billboard-geom (:float 1) (:vec3 1))
  :fragment (billboard-frag :vec2 :float))

;; https://forum.processing.org/two/discussion/3955/textured-billboard-and-transparency-problem
(defun draw-particles ()
  "textured particles blended into the scene"
  (with-setf (depth-mask) nil
    (with-blending *blend*
      (map-g #'billboard-pipe *str-src*
             :tex *cloud-tex*
             :world-view (world->view *currentcamera*)
             :view-clip  (projection *currentcamera*)))))

