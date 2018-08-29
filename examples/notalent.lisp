(in-package :shiny)

(defvar *notes* nil)

(defclass circle-note ()
  ((pitch :initarg :pitch :initform nil)
   (die-at :initarg :die-at :initform -1)
   (life :initarg :life :initform 0f0)
   (growth :initarg :growth :initform nil)
   (pencil :initarg :pencil :initform 1)
   (pos :initarg :pos :initform (v! 0 0))
   (color :initarg :color :initform (v! 0 0 0))
   (selfcolor :initarg :selfcolor :initform (v! 1 1 1))
   (shape :initarg :shape :initform 0)
   (fade :initarg :fade :initform 0f0)
   (rotation :initarg :rotation :initform 0f0)))

(defun mynow ()
  ;; Just some number that increases over time that we use
  ;; in a bunch of places
  (/ (get-internal-real-time)
     1000f0))

;; --------------------------------------------------
(defun-g vert ((vert :vec2))
  (values (v! vert 0 1)
          (* (+ vert .5) .5)))

(defun-g frag ((uv :vec2) &uniform
               (resolution :vec2) (time :float) (instance :float))
  (v! .3 .3 .3 1))

(defpipeline-g pipe ()
  :vertex (vert :vec2)
  :fragment (frag :vec2))

(defmacro defcard (() &body body)
  `(defun-g frag ((uv :vec2) &uniform
                  (resolution :vec2) (time :float)
                  (life :float)
                  (shape :int)
                  (pencil :int)
                  (mycolor :vec3)
                  (rotation :float)
                  (selfcolor :vec3)
                  (fade :float)
                  (pos :vec2) (pitch :int))
     (let* ((st  (/ (s~ gl-frag-coord :xy)
                    resolution))
            (color  (v! 0 0 0))
            ;; this helps keeping things inside
            (st (+ .5 (* 1.1912 (- st .5)))))
       ;; this removes some artifacts on bridges AND
       ;; keeps the aspect ratio
       (if (> (y resolution) (x resolution))
           (progn
             (multf (y st) (/ (y resolution) (x resolution)))
             (decf  (y st) (/ (- (* .5 (y resolution))
                                 (* .5 (x resolution)))
                              (x resolution))))
           (progn
             (multf (x st) (/ (x resolution) (y resolution)))
             (decf  (x st) (/ (- (* .5 (x resolution))
                                 (* .5 (y resolution)))
                              (y resolution)))))
       ,@body)))

;;--------------------------------------------------

(defparameter *blend*
  (make-blending-params
;;   :mode-rgb :func-add
;;   :mode-alpha :func-add
   :source-rgb :one
   :destination-rgb :one
;;   :source-alpha :one
;;   :destination-alpha :one
   ))


(defun draw! ()
  (let ((res (surface-resolution (current-surface)))
        )
    (setf (viewport-resolution (current-viewport))
          res)
    (as-frame
      (loop :for x :in *notes* :do

         ;; reduce or increase life if growth is defined
         (case (slot-value x 'growth)
           (+ (incf (slot-value x 'life) .05))
           (- (decf (slot-value x 'life) .05)))

         ;; ...
         (when (> (slot-value x 'fade) 0)
           (decf (slot-value x 'fade) .02))
         
         ;; kill when life implodes or explodes
         (when (or (< (slot-value x 'life) 0)
                   (> (slot-value x 'life) 9))
           (setf *notes* (remove x *notes*)))

         ;; kill when time expires
         (when (> (get-universal-time) (slot-value x 'die-at))
           (setf *notes* (remove x *notes*)))
         
         
         (with-setf (depth-test-function) nil
           (with-blending *blend*
             (with-slots (color rotation fade pos
                                pencil pitch
                                life shape selfcolor)
                 x
               (map-g #'pipe (get-quad-stream-v2)
                      :resolution res
                      :time (mynow)
                      :rotation rotation
                      :mycolor color
                      :pos pos
                      :fade fade
                      :pencil pencil
                      :selfcolor selfcolor
                      :pitch pitch
                      :shape shape
                      :life (float life)))))))))

(defun init ()
  (setf *notes* nil)
  (push (make-instance 'circle-note
                       :life 1f0
                       :pitch 60
                       :die-at (+ (get-universal-time) 2))
        *notes*))

(def-simple-main-loop runplay (:on-start #'init)
  (draw!))

;;--------------------------------------------------


(defcard ()
  (let* (;;(color (v! .3 .1 .2))
         (color (+ color (stroke (circle-sdf st)
                                 (* .015 pitch)
                                 .01)))
         (color (+ color (* (v! .9 .2 .3) (g-fill (circle-sdf
                                        st
                                        )
                                       .5)))))
    (v! color)))

(defcard ()
  (let* ((ost st)
         (st (pixel-spirit-deck:rotate st (radians rotation)))
         (ship (cond
                 ((= shape 0) (circle-sdf (+ pos st)))
                 ((= shape 3) (* 1.2 life (tri-sdf (+ (v! 0 -.1) st))))
                 ((= shape 4) (rays-sdf (pixel-spirit-deck:rotate
                                         st
                                         (radians (* pitch (sin time))))
                                        10))
                 (t (rect-sdf st (v2! (* pitch .01))))))
         (vshape   (if (= pencil 0)
                       (g-fill ship (* pitch life .002))
                       (stroke ship
                               (* pitch life .01)
                               .01)))
         (newcolor (v3! vshape))
         (newcolor (if (<= fade -1) newcolor (* newcolor fade)))
         (newcolor (+ newcolor mycolor))
         (newcolor (* newcolor selfcolor))
         (newcolor (if (not (= rotation 0))
                       (if (= shape 1)
                           (+ newcolor (* .2 (y ost)))
                           newcolor)
                       (+ newcolor
                             (step (v3! (the-book-of-shaders:snoise
                                         (* (* .09 pitch)  (+ time st))))
                                   (+ -.9 newcolor)))))
         ;; (newcolor (+ newcolor (v! (exp (* 100 (tan time) (y st) (x st)))
         ;;                           0
         ;;                           0)))
         )
    (v! newcolor 0)))
