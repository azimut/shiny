(in-package :shiny)

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
(defvar *scott-tex* nil)
;; --------------------------------------------------
(defvar *samplers* (make-hash-table :test #'equal))
(defun load-tex (rel-path)
  (or (gethash rel-path *samplers*)
      (let* ((path (asdf:system-relative-pathname :somecepl rel-path))
             (tex  (load-image-to-texture path)))
        (setf (gethash rel-path *samplers*)
              (sample tex)))))
;; --------------------------------------------------
(defun-g vert ((vert :vec2))
  (values (v! vert 0 1)
          (+ (v2! .5) (* vert .5))))

(defun-g frag ((uv :vec2)
               &uniform
               (time :float)
               (resolution :vec2)
               (sam :sampler-2d)
               (cam-pos :vec3))
  ;; 8 = width size
  ;; time ...mod does not matter...
  ;; .125 = 1/8
  ;; -1 ...flip vertically
  ;; /2 ... pick one row of the two
  (let* ((frames 8)
         (rows 2)
         (time (* 15 time))
         (mtime (floor (mod time frames)))
         (vx (+ (* (* -1 mtime)
                   .125)
                (/ (x uv)
                   frames)))
         (vy (/ (* -1 (y uv))
                rows))
;;         (vx (* (cos time) vx))
;;         (vy (* (cos time) vy))
         ;;; wibble
         (color (texture sam
                         (v! vx
                             vy))))
    color))
;; --------------------------------------------------
(defpipeline-g mario ()
  :vertex (vert :vec2)
  :fragment (frag :vec2))

(defun init ()
  (unless *scott-tex*
    (setf *scott-tex*
          (load-tex "scottpilgrim_multiple.png")))
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
  (setf (resolution (current-viewport))
        (surface-resolution (current-surface)))
  (map-g #'mario *vert-stream*
         :time (mynow)
         :resolution (surface-resolution (current-surface))
         :sam *scott-tex*
         :cam-pos *cam-pos*)
  (swap))

(def-simple-main-loop runplay (:on-start #'init)
  (draw!))
