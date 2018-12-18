(in-package :shiny)

(defparameter *pointlight-pos* (v! 0 0 0))

(defun render-all-the-things (actor camera)
  (update actor)
  (draw actor camera))

(defgeneric draw (actor camera))
(defmethod draw (actor camera))

(defmethod draw ((actor box) camera)
  (with-slots (buf scale) actor
    (map-g #'generic-pipe buf
           :scale scale
           :color (v! .001 .001 .001)
           :model-world (model->world actor)
           :world-view (world->view camera)
           :view-clip  (projection camera))))

(defmethod draw ((actor pbr) camera)
  (with-slots (buf albedo normal height roughness scale ao) actor
    (map-g #'pbr-pipe buf
           :uv-repeat 8f0
           :uv-speed .1f0
           :scale scale
           :albedo albedo
           :ao-map ao
           :normal-map normal
           :height-map height
           :rough-map roughness
           :time (mynow)
           :color-mult 1f0
           :cam-pos (pos camera)
           :light-pos *light-pos*
           :model-world (model->world actor)
           :world-view (world->view camera)
           :view-clip  (projection camera))))


(defmethod draw ((actor pbr-simple) camera)
  (with-slots (buf scale color) actor
    (map-g #'pbr-simple-pipe buf
           :scale scale
           :color color
           :time (mynow)
           :color-mult 1f0
           :cam-pos (pos camera)
           :light-pos *light-pos*
           :model-world (model->world actor)
           :world-view (world->view camera)
           :view-clip  (projection camera))))
