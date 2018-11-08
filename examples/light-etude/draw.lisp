(in-package :shiny)

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

;; (defmethod draw ((actor piso) camera)
;;   (with-slots (buf scale) actor
;;     (map-g #'generic-pipe buf
;;            :scale 1f0
;;            :color (v! .9 .9 .9)
;;            :model-world (model->world actor)
;;            :world-view (world->view camera)
;;            :view-clip  (projection camera))))

(defmethod draw ((actor cement) camera)
  (with-slots (buf scale) actor
    (map-g #'generic-pipe buf
           :scale scale
           :color (v! .9 .9 .9)
           :model-world (model->world actor)
           :world-view (world->view camera)
           :view-clip  (projection camera))))

;; (defmethod draw ((actor box) camera)
;;   (with-slots (buf scale) actor
;;     (map-g #'generic-pipe buf
;;            :time 1f0 ;;(mynow)
;;            :scale scale
;;            :color (v! .9 .9 .9)
;;            :model-world (model->world actor)
;;            :world-view (world->view camera)
;;            :view-clip  (projection camera))))

;; (defmethod draw ((actor sphere) camera)
;;   (with-slots (buf scale) actor
;;     (map-g #'generic-pipe buf
;;            :time (mynow)
;;            :scale scale
;;            :color (v! 1 1 1)
;;            :model-world (model->world actor)
;;            :world-view (world->view camera)
;;            :view-clip  (projection camera))))


;;--------------------------------------------------
(defmethod draw ((actor assimp-thing) camera)
  (with-slots (buf scale) actor
    (map-g #'generic-pipe buf
           :scale 1f0
           :color (v! .2 .2 .2)
           :model-world (model->world actor)
           :world-view (world->view camera)
           :view-clip  (projection camera))))

(defmethod draw ((actor assimp-bloom) camera)
  (with-slots (buf scale) actor
    (map-g #'light-pipe buf
           :scale 1f0
           :color (v3:*s (v! 1f0 .7 .3) 230f0)
           :model-world (model->world actor)
           :world-view (world->view camera)
           :view-clip  (projection camera))))
