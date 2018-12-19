(in-package #:shiny)

;; CEPL HELPERS

(defun mynow ()
  ;; Just some number that increases over time that we use
  ;; in a bunch of places
  (/ (get-internal-real-time)
     1000f0))

;;--------------------------------------------------
;; From cepl.tests

(defmacro with-free (name thing &body body)
  (let ((name (or name (gensym "thing"))))
    `(let ((,name ,thing))
       (unwind-protect (progn ,@body)
         (free ,name)))))

(defmacro with-free* (bindings &body body)
  `(let* ,bindings
     (unwind-protect (progn ,@body)
       ,@(loop :for (name) :in bindings :collect
            `(free ,name)))))

;;------------------------------------------------------------
;; Helpers for tangent-space calculations

(defstruct-g tb-data
  (tangent :vec3)
  (bitangent :vec3))

;; From play-with-verts, based on:
;; https://learnopengl.com/Advanced-Lighting/Normal-Mapping
(defun calc (verts i0 i1 i2)
  "returns a list pair of tangent and bitangent"
  (let* ((pos1 (pos (aref-c verts i0)))
         (pos2 (pos (aref-c verts i1)))
         (pos3 (pos (aref-c verts i2)))
         (uv1  (tex (aref-c verts i0)))
         (uv2  (tex (aref-c verts i1)))
         (uv3  (tex (aref-c verts i2)))
         ;;
         (edge1 (v3:- pos2 pos1))
         (edge2 (v3:- pos3 pos1))
         (delta-uv1 (v3:- uv2 uv1))
         (delta-uv2 (v3:- uv3 uv1))
         ;;
         (f (/ 1.0 (- (* (x delta-uv1) (y delta-uv2))
                      (* (x delta-uv2) (y delta-uv1)))))
         ;;
         (tangent1
          (v3:normalize
           (v! (* f (- (* (y delta-uv2) (x edge1))
                       (* (y delta-uv1) (x edge2))))
               (* f (- (* (y delta-uv2) (y edge1))
                       (* (y delta-uv1) (y edge2))))
               (* f (- (* (y delta-uv2) (z edge1))
                       (* (y delta-uv1) (z edge2)))))))
         (bitangent1
          (v3:normalize
           (v! (* f (+ (* (- (x delta-uv2)) (x edge1))
                       (* (x delta-uv1) (x edge2))))
               (* f (+ (* (- (x delta-uv2)) (y edge1))
                       (* (x delta-uv1) (y edge2))))
               (* f (+ (* (- (x delta-uv2)) (z edge1))
                       (* (x delta-uv1) (z edge2))))))))
    (list tangent1 bitangent1)))

(defun tbdata-from-vertex-and-indices (g-verts g-indices)
  (let* ((verts (pull1-g g-verts))
         (indices (pull-g g-indices))
         (result (make-gpu-array
                  nil :dimensions (first (dimensions verts))
                  :element-type 'tb-data)))
    (with-gpu-array-as-c-array (data result)
      (loop :for (i0 i1 i2)
         :on indices
         :by #'cdddr
         :do (let ((pair (calc verts i0 i1 i2)))
               (setf (aref-c data i0) pair)
               (setf (aref-c data i1) pair)
               (setf (aref-c data i2) pair))))
    result))

;;------------------------------------------------------------
;; Meshes
;;
;; We cache the data based on the the arguments so we don't
;; get lots of instances in memory

(defvar *meshes* (make-hash-table :test #'equal))

(defun sphere
    (&optional (radius 1f0)
       (lines-of-latitude 30)
       (lines-of-longitude 30) has-tangents)
  (declare (boolean has-tangents))
  (let ((key (list radius has-tangents)))
    (or (gethash key *meshes*)
        (destructuring-bind (vert index)
            (nineveh.mesh.data.primitives:sphere-gpu-arrays
             :radius radius
             :lines-of-latitude lines-of-latitude
             :lines-of-longitude lines-of-longitude)
          (setf (gethash key *meshes*)
                (if has-tangents
                    (make-buffer-stream
                     (list vert (tbdata-from-vertex-and-indices vert index))
                     :index-array index)
                    (make-buffer-stream vert :index-array index)))))))

(defun box (&optional (w 1f0) (h 1f0) (d 1f0) has-tangents)
  (declare (boolean has-tangents))
  (let ((key (list w h d has-tangents)))
    (or (gethash key *meshes*)
        (destructuring-bind (vert index)
            (nineveh.mesh.data.primitives:box-gpu-arrays :width w
                                                         :height h
                                                         :depth d)
          (setf (gethash key *meshes*)
                (if has-tangents
                    (make-buffer-stream
                     (list vert (tbdata-from-vertex-and-indices vert index))
                     :index-array index)
                    (make-buffer-stream vert :index-array index)))))))

(defun cylinder (&optional (radius 1f0) (height 1f0) has-tangents)
  (declare (boolean has-tangents))
  (let ((key (list radius height has-tangents)))
    (or (gethash key *meshes*)
        (destructuring-bind (vert index)
            (nineveh.mesh.data.primitives:cylinder-gpu-arrays :radius radius
                                                              :height height)
          (setf (gethash key *meshes*)
                (if has-tangents
                    (make-buffer-stream
                     (list vert (tbdata-from-vertex-and-indices vert index))
                     :index-array index)
                    (make-buffer-stream vert :index-array index)))))))

(defun cone (&optional (radius 1f0) (height 1f0) has-tangents)
  (declare (boolean has-tangents))
  (let ((key (list radius height has-tangents)))
    (or (gethash key *meshes*)
        (destructuring-bind (vert index)
            (nineveh.mesh.data.primitives:cone-gpu-arrays :radius radius
                                                          :height height)
          (setf (gethash key *meshes*)
                (if has-tangents
                    (make-buffer-stream
                     (list vert (tbdata-from-vertex-and-indices vert index))
                     :index-array index)
                    (make-buffer-stream vert :index-array index)))))))

(defun lattice
    (&optional (width 100f0) (height 100f0) (x 500) (y 500) has-tangents)
  (declare (boolean has-tangents))
  (let ((key (list :lat width height x y has-tangents)))
    (or (gethash key *meshes*)
        (destructuring-bind (vert index)
            (nineveh.mesh.data.primitives:lattice-gpu-arrays
             :width width
             :height height
             :x-segments x
             :y-segments y)
          (setf (gethash key *meshes*)
                (if has-tangents
                    (make-buffer-stream
                     (list vert (tbdata-from-vertex-and-indices vert index))
                     :index-array index)
                    (make-buffer-stream vert :index-array index)))))))


;;----------------------------------------
;; Dirt - image loader into CEPL sampler

(defvar *samplers* (make-hash-table :test #'equal))

(defun get-tex (path &optional (force nil) (mipmap t) (image-format :rgba8))
  (when force
    (let ((s (gethash path *samplers*)))
      (when s
        (free (sampler-texture s)))
      (remhash path *samplers*)))
  (let ((absolutep (uiop:absolute-pathname-p path)))
    (or (gethash path *samplers*)
        (setf (gethash path *samplers*)
              (cepl:sample
               (dirt:load-image-to-texture
                (if absolutep
                    path
                    (asdf:system-relative-pathname :shiny path))
                image-format
                mipmap
                t))))))

;;--------------------------------------------------
;; Assimp - 3d object loader

(defun assimp-mesh-to-stream (mesh)
  (declare (ai:mesh mesh))
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
        (make-buffer-stream v-arr :index-array i-arr)))))

;;--------------------------------------------------
;; Cubemap

(defun make-cubemap-tex (&rest paths)
  "Returns a gpu texture from the provided images"
  (assert (= 6 (length paths)))
  (with-c-arrays-freed
      (ca (mapcar (lambda (p)
                    (dirt:load-image-to-c-array
                     (asdf:system-relative-pathname :shiny p)))
                  paths))
    (make-texture ca :element-type :rgb8 :cubes t)))
