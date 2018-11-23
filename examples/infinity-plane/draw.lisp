(in-package :shiny)

(defparameter *dirlight-pos* (v! 1000 1000 1000))
(defparameter *dirlight-mul* .5)
(defparameter *pointlight-pos* (v! 0 2 0))

(defun render-all-the-things (actor camera)
  (update actor)
  (draw actor camera))

(defgeneric draw (actor camera))
(defmethod draw (actor camera))

;; (defmethod draw ((actor camera) camera)
;;   (with-slots (buf tex) actor
;;     (map-g #'generic-tex-pipe buf
;;            :time (mynow)
;;            :scale 1f0
;;            :tex tex
;; ;;           :color (v! .1 .1 .8)
;;            :model-world (model->world actor)
;;            :world-view (world->view camera)
;;            :view-clip  (projection camera))))

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
           :color (v! .9 .9 .9)
           :model-world (model->world actor)
           :world-view (world->view camera)
           :view-clip  (projection camera))))

;; (defmethod draw ((actor sphere) camera)
;;   (with-slots (buf scale) actor
;;     (map-g #'generic-pipe buf
;;            :time (mynow)
;;            :scale scale
;;            :color (v! 1 1 1)
;;            :model-world (model->world actor)
;;            :world-view (world->view camera)
;;            :view-clip  (projection camera))))

(defmethod draw ((actor celestial-sphere) camera)
  (with-slots (buf scale) actor
    (with-setf* ((cull-face) :front
                 ;;(depth-test-function) #'always
                 ;;(depth-mask) nil
                 )
      (map-g #'light-pipe buf
             :color (v! .5 .2 .5)
             :scale 20f0
             :time (mynow)
             :model-world (model->world actor)
             :world-view (world->view camera)
             :view-clip  (projection camera)))))
