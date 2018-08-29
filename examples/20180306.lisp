(in-package :shiny)

;; https://ansimuz.itch.io/parallax-forest
;; http://terraria.wikia.com/wiki/Bunny
;; https://konbe.deviantart.com/art/GS-Homura-running-sprite-sheet-393600955
;; https://konbe.deviantart.com/art/GS-Kyubey-running-sprite-sheet-395294156

(defclass actor ()
  ((name :initform (error "set a name") :initarg :name :accessor debug-name)
   (pos :initform (v! 0 0 0) :initarg :pos)
   (rot :initform 0f0 :initarg :rot)
   (scale :initform 1f0 :initarg :scale)
   (rows :initform 1f0 :initarg :rows)
   (speed :initform 1f0 :initarg :speed)
   (direction :initform 0 :initarg :direction)
   (width :initform 1f0 :initarg :width)
   (columns :initform 1f0 :initarg :columns)
   (visual :initarg :visual)))

(defclass walking (actor) ())


(defvar *actors* nil)


(defgeneric update (actor)
  (:method ((actor actor)))
  (:method ((actor walking))
    (with-slots (pos speed) actor
      (when (or (< (x pos) -2.1)
                (> (x pos)  2.1))
        (setf *actors*
              (remove-if (lambda (x) (equal x actor)) *actors*)))
      (v3:incf pos (v3! (* .0001 speed) 0 0)))))

(defvar *screen-height* 1f0)
(defvar *screen-width* 1f0)

(defun mynow ()
  ;; Just some number that increases over time that we use
  ;; in a bunch of places
  (/ (get-internal-real-time)
     1000f0))
(defvar *gpu-verts-arr* nil)
(defvar *gpu-index-arr* nil)
(defvar *vert-stream* nil)
;; --------------------------------------------------
;;(defvar *cube-stream* nil)
(defvar *cam-pos* (v! 0 0 0))

(defvar *blend* (make-blending-params))

(defvar *triggered* 0)
(defvar *triggered2* 0)

;; --------------------------------------------------
(defvar *samplers* (make-hash-table :test #'equal))
(defun load-tex (rel-path)
  (or (gethash rel-path *samplers*)
      (let* ((path (asdf:system-relative-pathname :somecepl rel-path))
             (tex  (load-image-to-texture path)))
        (setf (gethash rel-path *samplers*)
              (sample tex)))))
;; --------------------------------------------------

(defun-g rain-frag ((vert :vec2)
                    &uniform
                    (trigger :bool)
                    (resolution :vec2)
                    (time :float))
  (let* ((time (* time .05))
         (vert (+ vert (v! (- time) 0)))
         (vert (* vert 10))
         (pe   (perlin-noise vert)))
    (if trigger
        (v! (v3! (+ .9 pe)) 0)
        (v! (v3! pe) 0))
;;    (v! (v3! (y (/ (s~ gl-frag-coord :xy) resolution))) 0)
    
    ))

(defun-g rain-vert ((pos :vec2))
  (values (v! pos 0 1)
          (* .5 (+ .5 pos))))
;; --------------------------------------------------
(defun-g vert ((vert :vec2) &uniform
               (scale :float)
               (pos :vec3)
               (width :float)
               (screen-height :float)
               (screen-ratio :float))
  (values (v! (/ (+ (s~ pos :xy)
                    (* scale vert))
                 (v! (* screen-ratio (/ 1f0 width))
                     screen-height))
              0 1)
          (+ (v2! .5) (* vert .5))))

(defun-g frag ((uv :vec2)
               &uniform
               (direction :bool)
               (speed :float)
               (rows :float)
               (columns :float)
               (time :float)
               (trigger :bool)
               (resolution :vec2)
               (sam :sampler-2d)
               (cam-pos :vec3))
  ;; 8 = width size
  ;; time ...mod does not matter...
  ;; .125 = 1/8
  ;; -1 ...flip vertically
  ;; /2 ... pick one row of the two
  (let* (
         (time (* speed time))
         (mtime (if (= columns 1d0)
                    (mod time columns)
                    (floor (mod time columns))))
         (vx (+ (* (* (if direction 1 -1) mtime)
                   (/ 1f0 columns))
                (/ (x uv)
                   columns)))
         (vy (/ (* -1 (y uv))
                rows))
;;;         (vx (* (cos time) vx))
;;;         (vy (* (cos time) vy))
         ;;; wibble
         (t-uv (v! vx vy))
         (color (texture sam t-uv))
         )
    (if trigger
        (* (+ (v! (v3! (expt (y (/ (s~ gl-frag-coord :xy) resolution)) 3)) 0) color) .8)
        (*  color .1))
    ))
;; --------------------------------------------------

(defpipeline-g mario ()
  :vertex (vert :vec2)
  :fragment (frag :vec2))

(defpipeline-g luigi ()
  :vertex (rain-vert :vec2)
  :fragment (rain-frag :vec2))

(defun init ()
  (setf *screen-width* 1f0)
  ;;  (setf (clear-color) (v! .46 .57 .69 1))
  (setf (clear-color) (v! 0 0 0 1))
  (setf *actors* nil)
  (unless *actors*
    (push
     (make-instance
      'actor
      :name "scott"
      :visual (load-tex "homura_running_by_konbe-d6ic8fv.png")
      :columns 8f0
      :scale .3f0
      :speed 7f0
      :pos (v! 0 -.6 0)
      :rows 1f0) *actors*)    
    (push
     (make-instance
      'actor
      :name "front"
      :visual (load-tex "parallax-forest-front-trees.png")
      :columns 1f0
      :speed .5f0
      :width 2f0
      :rows 1f0) *actors*)
    (push
     (make-instance
      'actor
      :name "front"
      :visual (load-tex "parallax-forest-middle-trees.png")
      :columns 1f0
      :speed .2f0
      :width 2f0
      :rows 1f0) *actors*))
  (when *gpu-verts-arr*
    (free *gpu-verts-arr*))
  (when *gpu-index-arr*
    (free *gpu-index-arr*))
  (when *vert-stream*
    (free *vert-stream*))
  
  (setf *gpu-verts-arr*
        (make-gpu-array
         (list (v! -1.0  1.0)
               (v! -1.0 -1.0)
               (v!  1.0 -1.0)
               (v!  1.0  1.0))
         :element-type :vec2))
  
  (setf *gpu-index-arr*
        (make-gpu-array
         (list 0 1 2
               0 2 3)))
  
  (setf *vert-stream*
        (make-buffer-stream *gpu-verts-arr*
                            :index-array *gpu-index-arr*)))

(defun draw! ()
  (step-host)
  (clear)
  (let ((res (surface-resolution (current-surface)))
        (trigger (funcall *triggered* 'shoot)))
    (setf (resolution (current-viewport))
          res)

    (when (= 1 (funcall *triggered2* 'shoot))
      (push
       (make-instance
        'walking
        :name "walkingthing"
        :visual (load-tex "homy_by_konbe-d6jcix8.png")
        :columns 6f0
        :direction 1
        :scale (ran :from .1 :below .4)
        :speed (ran :from 6f0 :below 15f0)
        :pos (v! (ran :from .2 :below 1) (ran :below -.1 :from -.6) 0)
        :rows 1f0)
       *actors*))

    
    (map-g #'luigi *vert-stream*
           :resolution res
           :trigger trigger
           :time (mynow))
    (with-setf* ((depth-test-function) nil)
      (with-blending *blend*
        (loop :for x :in *actors* :do
           (update x)
           (with-slots (rows speed width columns pos scale visual direction) x
             (map-g #'mario *vert-stream*
                    :time (mynow)
                    :rows rows
                    :direction direction
                    :speed speed
                    :columns columns
                    :scale scale
                    :screen-height *screen-height*
                    :width width
                    :screen-ratio (/ (x res) (y res))
                    :pos pos
                    :trigger trigger
                    :resolution res
                    :sam visual
                    ;;:cam-pos *cam-pos*
                    ))))
      )
    (swap)))  

(def-simple-main-loop runplay (:on-start #'init)
  (draw!))
