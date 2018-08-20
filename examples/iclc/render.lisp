(in-package :somecepl)

(defstruct-g (music :layout :std-140)
  (samples (:double 512)))

(defun-g g-rand ((x :float))
  (fract (* (sin x)
            10000f0)))

(defun-g cyn ((x :float))
  (+ .5 (* .5 (cos x))))

(defun-g syn ((x :float))
  (+ .5 (* .5 (sin x))))

(defun-g white-frag
    ((uv :vec2) (color :vec3) &uniform
     (time :float) (trigger :float))
  (let* (;;(c2 (v4! (syn time)))
        ;;(c1 (v4! (cyn time)))
        (c2 (v! 0 .3 0 1))
        (c1 (v! .7 .7 .7 1))
         (final (mix c1 c2 (* (tan time) (y uv))))
         ;;(final (smoothstep 0 1  (y uv)))
         )
    final))

(defun-g frag
    ((uv :vec2) (color :vec3) &uniform
     (sound music :ubo) (time :float) (rms :float))
  (with-slots (samples) sound
    (let* ((wide 312)
           (wave (float (aref samples (abs (int (ceil (* wide (x uv))))))))
           (wave (- 1 (smoothstep 0f0 .05 (abs (- wave (- (y uv) .5))))))
           (wave (v! 0 wave 0)))
      wave)))

;; Este V shader pasa las cosas asi como las encuentra
(defun-g vert
    ((vert g-pnt) &uniform
     (model-world :mat4) (world-view :mat4) (view-clip :mat4))
  (let* ((pos       (pos vert))
         (norm      (norm vert))
         (world-pos (* model-world (v! pos 1)))
         (view-pos  (* world-view  world-pos))
         (clip-pos  (* view-clip   view-pos)))
    (values clip-pos
            (v! (+ .5 (* .5 (s~ pos :xy))))
            norm)))

;; Este shader modifica cosas en base a gl-instance-id
(defun-g dolly-frag
    ((uv :vec2) (frag-pos :vec3) (frag-norm :vec3) &uniform
     (sound music :ubo) (time :float) (rms :float))
  (with-slots (samples) sound
    (let* ((wide 102)
           (wave (float (aref samples (abs (int (ceil (* wide (x uv))))))))
           (wave (- 1 (smoothstep 0f0 .05 (abs (- wave (- (y uv) .5))))))
           (wave (v3! wave ))
           (vec-to-light (- (v! 0 5 0)
                            frag-pos))
           (dir-to-light (normalize vec-to-light))
           (wave (+ wave (* .2 (dot dir-to-light frag-norm)))))
      wave)))

(defun-g dolly-vert
    ((vert g-pnt) &uniform (time :float)
     (model-world :mat4) (world-view :mat4) (view-clip :mat4))
  (let* ((time (+ (* (* .01
                        (syn time)) time) gl-instance-id))
         (myid gl-instance-id)
         (p   (pos vert))
         (pos       (cond ((< myid 50)
                           (+ (v! (+ -15 (* 30 (syn time)))
                                  (* 10 (syn time))
                                  (+ -15 (* 30 (cyn time))))
                              p))
                          ((< myid 100)
                           (+ (v! (+ -13 (* 26 (cyn time)))
                                  (* 10 (syn time))
                                  (+ -13 (* 26 (syn time))))
                              p))
                          ((< myid 145)
                           (+ (v! (* 10 (syn time))
                                  (+ -20 (* 40 (syn time)))
                                  (+ -15 (* 30 (cyn time))))
                              p))
                          (t
                           (+ (v! (* 10 (syn time))
                                  (+ -10 (* 20 (cyn time)))
                                  (+ -5 (* 10 (syn time))))
                              p))))
         (pos (* pos (m3:rotation-z (radians (* 90 (syn time))))))
         (norm (* (m4:to-mat3 model-world) (norm vert)))
         (world-pos (* model-world (v! pos 1)))
         (view-pos  (* world-view  world-pos))
         (clip-pos  (* view-clip   view-pos)))
    (values clip-pos
            (v! (+ .5 (s~ p :xy)))
            (s~ world-pos :xyz)
            norm)))



;;--------------------------------------------------

(defpipeline-g pipe ()
  :vertex (vert g-pnt)
  :fragment (frag :vec2 :vec3))

(defpipeline-g white ()
  :vertex (vert g-pnt)
  :fragment (white-frag :vec2 :vec3))

(defpipeline-g dollypipe ()
  :vertex (dolly-vert g-pnt)
  :fragment (dolly-frag :vec2 :vec3 :vec3))
