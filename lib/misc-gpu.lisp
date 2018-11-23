(in-package :shiny)

;;--------------------------------------------------
;; https://learnopengl.com/Advanced-Lighting/Normal-Mapping
;; "Pushing pixels" code
(defun-g norm-from-map ((normal-map :sampler-2d)
                        (uv :vec2))
  (let* ((norm-from-map
          (s~ (texture normal-map uv) :xyz))
         (norm-from-map
          (normalize (- (* norm-from-map 2.0) 1.0))))
    (v! (x norm-from-map)
        (- (y norm-from-map))
        (z norm-from-map))))

;;--------------------------------------------------
;; https://learnopengl.com/Advanced-Lighting/Parallax-Mapping
;; vec3 viewDir   = normalize(fs_in.TangentViewPos - fs_in.TangentFragPos);
(defun-g parallax-mapping ((uv :vec2)
                           (view-dir :vec3)
                           (depth-map :sampler-2d)
                           (height-scale :float))
  (let* ((height (x (texture depth-map uv)))
         (p      (* (/ (s~ view-dir :xy) (z view-dir))
                    (* height height-scale))))
    (- uv p)))

;;--------------------------------------------------
;; https://catlikecoding.com/unity/tutorials/rendering/part-14/
(defun-g fog-linear ((frag-pos :vec3)
                     (cam-pos :vec3)
                     (start :float)
                     (end :float))
  (let* ((view-distance (length (- frag-pos cam-pos)))
         (fog-factor
          (+ (* view-distance (/ -1 (- end start)))
             (/ end (- end start)))))
    fog-factor))

(defun-g fog-exp ((frag-pos :vec3)
                  (cam-pos :vec3)
                  (density :float))
  (let ((view-distance (length (- frag-pos cam-pos))))
    (exp2 (- (* view-distance (/ density (log 2)))))))

(defun-g fog-exp2 ((frag-pos :vec3)
                   (cam-pos :vec3)
                   (density :float))
  (let* ((view-distance (length (- frag-pos cam-pos)))
         (fog-density
          (* (/ density (sqrt (log 2))) view-distance))
         (fog-factor
          (exp2 (- (* fog-density fog-density)))))
    fog-factor))

;;--------------------------------------------------
(defun-g billboard-geom (&uniform (camera-pos :vec3)
                                  (view-clip :mat4))
  (declare (output-primitive :kind :triangle-strip :max-vertices 4))
  (let* ((p (s~ (gl-position (aref gl-in 0)) :xyz))
         (to-camera (normalize (- camera-pos p)))
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
    (values)))
