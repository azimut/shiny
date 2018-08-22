(in-package #:somecepl)

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

(defun sphere (&optional (radius 1f0))
  (let ((key radius))
    (or (gethash key *meshes*)
        (destructuring-bind (vert index)
            (nineveh.mesh.data.primitives:sphere-gpu-arrays :radius radius)
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
