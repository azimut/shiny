(in-package :shiny)

(defvar *lamppost* nil)
(defvar *lamp-thing* nil)
(defvar *fbo* nil)
(defvar *lfbo* nil)

(defun initialize ()
  (unless *lamppost*
    (setf *lamppost*
          (classimp:import-into-lisp
           "/home/sendai/Downloads/assets/AEL_245_Contempo_3ds.3DS"
           :processing-flags
           '(:ai-process-calc-tangent-space
             ;;:ai-process-sort-by-p-type
             ;;:ai-process-make-left-handed
             ;;:ai-process-gen-smooth-normals
             :ai-process-triangulate)))
    (setf *lamp-thing*
          (assimp-mesh-to-thing *lamppost*)))
  (when *fbo*
    (free *fbo*))
  ;; HDR fbo
  (setf *fbo* (make-fbo (list 0 :element-type :rgba16f)))
  (setf *sam* (cepl:sample (attachment-tex *fbo* 0)))
  (unless *lfbo*
    (setf *lfbo* (make-fbo '(:d :dimensions (1024 1024))))
    (setf *sfbo* (cepl:sample (attachment-tex *lfbo* :d))))
  (setf (clear-color) (v! .2 .2 .9 0))
  (setf *actors* nil)
  ;;-------------------------------------
;;  (make-box)
;;  (make-piso)
  (push *lamp-thing* *actors*)
  NIL)

(defun draw! ()  
 (let ((res (surface-resolution (current-surface))))
    (setf (resolution (current-viewport))
          res)
    (update *currentcamera*)
    (update-all-the-things *actors*)
    (with-fbo-bound (*fbo*)
      (clear-fbo *fbo*)
      (loop :for actor :in *actors* :do
         (draw actor *currentcamera*)))
    ;; Draw fbo/postprocess
    (as-frame
      (map-g #'generic-2d-pipe (get-quad-stream-v2)
             :sam *sam*))))

(def-simple-main-loop runplay
    (:on-start #'initialize)
  (draw!))

(defstruct-g assimp-mesh
  (pos :vec3)
  (normal :vec3)
  (tangent :vec3)
  (bitangent :vec3)
  (uv :vec2))

(defun assimp-mesh-to-thing (scene)
  (with-slots ((mesh ai:meshes)) scene
    (let ((mesh (elt mesh 0)))
      (with-slots ((vertices ai:vertices)
                   (normals ai:normals)
                   (tangents ai:tangents)
                   (bitangents ai:bitangents)
                   (texture-coords ai:texture-coords)
                   (faces ai:faces))
          mesh
        (let* ((texture-coords (elt texture-coords 0)))
          (assert (= (length bitangents)
                     (length tangents)
                     (length normals)
                     (length vertices)
                     (length texture-coords)))
          (let ((v-arr (make-gpu-array nil :dimensions (length vertices)
                                       :element-type 'assimp-mesh))
                (i-arr (make-gpu-array nil :dimensions (* 3 (length faces))
                                       :element-type :ushort)))
            (with-gpu-array-as-c-array (c-arr i-arr)
              (loop
                 :for indices :across faces
                 :for i :from 0 :by 3
                 :do (setf (aref-c c-arr i) (aref indices 0)
                           (aref-c c-arr (+ i 1)) (aref indices 1)
                           (aref-c c-arr (+ i 2)) (aref indices 2))))
            (with-gpu-array-as-c-array (c-arr v-arr)
              (loop
                 :for v :across vertices
                 :for n :across normals
                 :for ta :across tangents
                 :for bt :across bitangents
                 :for tc :across texture-coords
                 :for i :from 0
                 :for a := (aref-c c-arr i)
                 :do (setf (assimp-mesh-pos a) v
                           (assimp-mesh-normal a) n
                           (assimp-mesh-tangent a) ta
                           (assimp-mesh-bitangent a) bt
                           (assimp-mesh-uv a) (v! (x tc) (y tc)))))
            (make-instance
             'assimp-thing
             :stream (make-buffer-stream v-arr :index-array i-arr))))))))
