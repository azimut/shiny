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
           :color (v! .001 .001 .001)
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

(defmethod draw ((actor planet) camera)
  (with-slots (buf albedo normal height roughness scale ao) actor
    (map-g #'pbr-pipe buf
           :scale scale
           :uv-repeat 1f0
           :uv-speed .01
           :albedo albedo
           :normal-map normal
           :height-map height
           :ao-map ao
           :time (mynow)
           :rough-map roughness
           :oclussion-factor (v! 1 0 0 1)
           :cam-pos (pos camera)
           :light-pos (v! -60 90 -50)
           :model-world (model->world actor)
           :world-view (world->view camera)
           :color-mult 100f0
           :view-clip  (projection camera))))

(defmethod draw ((actor pbr) camera)
  (with-slots (buf albedo normal height roughness scale ao) actor
    (map-g #'pbr-pipe buf
           :uv-repeat 8f0
           :uv-speed 1f0
           :scale scale
           :albedo albedo
           :normal-map normal
           :height-map height
           :ao-map ao
           :oclussion-factor (v! 0 1 0 1)
           :time (mynow)
           :rough-map roughness
           :color-mult 1f0
           :cam-pos (pos camera)
           :light-pos *light-pos*
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
             :scale 200f0
             :light-pos (screen-coord (resolution (current-viewport)) *light-pos*)
             :cam-pos (pos camera)
             :model-world (model->world actor)
             :world-view (world->view camera)
             :view-clip  (projection camera)))))
