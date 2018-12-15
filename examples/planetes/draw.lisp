(in-package :shiny)

(defparameter *dirlight-pos* (v! 1000 1000 1000))
(defparameter *dirlight-mul* .5)
(defparameter *pointlight-pos* (v! 0 0 0))

(defun render-all-the-things (actor camera)
  (update actor)
  (draw actor camera))

(defgeneric draw (actor camera))

(defmethod draw ((actor piso) camera)
  (with-slots (buf scale tex) actor
    (map-g #'tex-pipe buf
           :scale 1f0
           :albedo tex
           :cam-pos (pos camera)
           :model-world (model->world actor)
           :world-view (world->view camera)
           :view-clip  (projection camera))))

(defmethod draw ((actor box) camera)
  (with-slots (buf scale) actor
    (map-g #'generic-pipe buf
           :scale scale
           :color (v! .01 .01 .01)
           :model-world (model->world actor)
           :world-view (world->view camera)
           :view-clip  (projection camera))))

(defmethod draw ((actor sphere) camera)
  (with-slots (buf scale) actor
    (map-g #'circle-pipe buf
           :scale scale
           :color (v! 1 1 1)
           :model-world (model->world actor)
           :world-view (world->view camera)
           :view-clip  (projection camera))))

(defmethod draw ((actor pbr) camera)
  (with-slots (buf albedo normal height roughness scale ao) actor
    (map-g #'pbr-pipe buf
           :scale scale
           :albedo albedo
           :normal-map normal
           :height-map height
           :ao-map ao
           :time (mynow)
           :rough-map roughness
           :cam-pos (pos camera)
           :model-world (model->world actor)
           :world-view (world->view camera)
           :view-clip  (projection camera))))

(defmethod draw ((actor celestial-sphere) camera)
  (with-slots (buf scale) actor
    (with-setf* ((cull-face) :front
                 (depth-test-function) #'always
                 (depth-mask) nil)
      (map-g #'light-pipe buf
             :color (v! .5 .2 .5)
             :scale 1f0
             :time (mynow)
             :cam-pos (pos camera)
             :model-world (model->world actor)
             :world-view (world->view camera)
             :view-clip  (projection camera)))))

(defmethod draw ((actor clouds) camera)
  (with-slots (buf scale) actor
    (map-g #'clouds-pipe buf
           :scale scale
           :time (mynow)
           :model-world (model->world actor)
           :world-view (world->view camera)
           :view-clip  (projection camera))))
