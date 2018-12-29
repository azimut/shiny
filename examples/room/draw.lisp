(in-package :shiny)

(defparameter *pointlight-pos* (v! 0 0 0))

(defun render-all-the-things (actor camera time)
  (declare (single-float time))
  (update actor)
  (draw actor camera time))

(defgeneric draw (actor camera time))
(defmethod draw (actor camera time))

(defmethod draw ((actor box) camera (time single-float))
  (with-slots (buf scale) actor
    (map-g #'generic-pipe buf
           :scale scale
           :color (v! .001 .001 .001)
           :model-world (model->world actor)
           :world-view (world->view camera)
           :view-clip  (projection camera))))

(defmethod draw ((actor pbr) camera (time single-float))
  (with-slots (buf
               color
               albedo normal height roughness
               uv-speed
               scale ao uv-repeat metallic)
      actor
    (map-g #'pbr-pipe buf
           :uv-repeat uv-repeat
           :uv-speed uv-speed
           :scale scale
           :time time
           :color color
           :samd *samd*
           ;; Lighting
           :cam-pos (pos camera)
           :light-pos *light-pos*
           ;;
           :model-world (model->world actor)
           :world-view (world->view camera)
           :view-clip  (projection camera)
           ;; PBR
           :albedo albedo
           :ao-map ao
           :metallic metallic
           :normal-map normal
           :height-map height
           :rough-map roughness           
           ;; IBL
           :brdf-lut *s-brdf*           
           :prefilter-map *s-cubemap-prefilter*
           :irradiance-map *s-cubemap-live*)))

(defmethod draw ((actor pbr-simple) camera (time single-float))
  (with-slots (buf scale color roughness metallic) actor
    (map-g #'pbr-simple-pipe buf
           :scale scale
           :color color
           :time time
           ;; Lighting
           :cam-pos (pos camera)
           :light-pos *light-pos*
           ;;
           :model-world (model->world actor)
           :world-view (world->view camera)
           :view-clip  (projection camera)
           ;; PBR
           :roughness roughness
           :metallic metallic
           ;; IBL
           :brdf-lut *s-brdf*
           :prefilter-map *s-cubemap-prefilter*
           :irradiance-map *s-cubemap-live*)))

(defmethod draw ((actor cubemap) camera (time single-float))
  (with-slots (buf) actor
    (with-setf* ((cull-face) :front
                 (depth-test-function) #'<=)
      (map-g #'cubemap-pipe buf
             :tex *s-cubemap*
             :mod-clip
             (m4:* (projection camera)
                   (world->view camera))))))
