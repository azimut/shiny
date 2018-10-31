(in-package :shiny)

(defvar *fbo* nil)
(defvar *sam* nil)
(defvar *lfbo* nil)
(defvar *sfbo* nil)

(defun render-all-the-things (actor camera)
  (update actor)
  (draw actor camera))

(defgeneric draw (actor camera))
(defmethod draw (actor camera))

(defmethod draw ((actor piso) camera)
  (with-slots (buf) actor
    (map-g #'generic-pipe buf
           :time (mynow)
           ;;           :color (v! 1f0 .6 .31)
           :scale 1f0
           :color (v! .1 .1 .8)
           :model-world (model->world actor)
           :world-view (world->view camera)
           :view-clip  (projection camera))))

(defmethod draw ((actor box) camera)
  (with-slots (buf) actor
    (map-g #'generic-pipe buf
           :time (mynow)
           :scale .2f0
           :color (v! 1 1 1)
           :model-world (model->world actor)
           :world-view (world->view camera)
           :view-clip  (projection camera))))


(defmethod draw ((actor assimp-thing) camera)
  (with-slots (stream) actor
    (map-g #'assimp-pipe stream
           :time (mynow)
           :scale .2f0
           :color (v! 1 1 1)
           :model-world (model->world actor)
           :world-view (world->view camera)
           :view-clip  (projection camera))
    (map-g #'assimp-norm-pipeline stream
           :model->world (model->world actor)
           :world->view (world->view camera)
           :view->clip (projection camera)
           :scale .002f0)))
