(in-package #:shiny)

;; CEPL HELPERS

(defun mynow ()
  ;; Just some number that increases over time that we use
  ;; in a bunch of places
  (/ (get-internal-real-time)
     1000f0))


;;------------------------------------------------------------
;; Meshes
;;
;; We cache the data based on the the arguments so we don't
;; get lots of instances in memory

(defvar *meshes* (make-hash-table :test #'equal))

(defun sphere
    (&optional (radius 1f0) (lines-of-latitude 30) (lines-of-longitude 30))
  (let ((key radius))
    (or (gethash key *meshes*)
        (destructuring-bind (vert index)
            (nineveh.mesh.data.primitives:sphere-gpu-arrays
             :radius radius
             :lines-of-latitude lines-of-latitude
             :lines-of-longitude lines-of-longitude)
          (setf (gethash key *meshes*)
                (make-buffer-stream vert :index-array index))))))

(defun box (&optional (w 1f0) (h 1f0) (d 1f0))
  (let ((key (list w h d)))
    (or (gethash key *meshes*)
        (destructuring-bind (vert index)
            (nineveh.mesh.data.primitives:box-gpu-arrays :width w
                                                         :height h
                                                         :depth d)
          (setf (gethash key *meshes*)
                (make-buffer-stream vert :index-array index))))))

(defun cylinder (&optional (radius 1f0) (height 1f0))
  (let ((key (list radius height)))
    (or (gethash key *meshes*)
        (destructuring-bind (vert index)
            (nineveh.mesh.data.primitives:cylinder-gpu-arrays :radius radius
                                                              :height height)
          (setf (gethash key *meshes*)
                (make-buffer-stream vert :index-array index))))))

(defun cone (&optional (radius 1f0) (height 1f0))
  (let ((key (list radius height)))
    (or (gethash key *meshes*)
        (destructuring-bind (vert index)
            (nineveh.mesh.data.primitives:cone-gpu-arrays :radius radius
                                                          :height height)
          (setf (gethash key *meshes*)
                (make-buffer-stream vert :index-array index))))))

(defun lattice
    (&optional (width 100f0) (height 100f0) (x 500) (y 500))
  (let ((key (list :lat width height x y)))
    (or (gethash key *meshes*)
        (destructuring-bind (vert index)
            (nineveh.mesh.data.primitives:lattice-gpu-arrays
             :width width
             :height height
             :x-segments x
             :y-segments y)
          (setf (gethash key *meshes*)
                (make-buffer-stream vert :index-array index))))))


;;----------------------------------------
;; Dirt - image loader into CEPL sampler

(defvar *samplers* (make-hash-table :test #'equal))

(defun get-tex (path &optional (force nil) (mipmap t))
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
                :rgba8
                mipmap
                t))))))

;;--------------------------------------------------
;; Assimp - 3d object loader

(defun assimp-mesh-to-stream (mesh)
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
