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

;; (defmethod draw ((actor piso) camera)
;;   (with-slots (buf scale) actor
;;     (map-g #'generic-pipe buf
;;            :scale 1f0
;;            :color (v! .9 .9 .9)
;;            :model-world (model->world actor)
;;            :world-view (world->view camera)
;;            :view-clip  (projection camera))))

(defmethod draw ((actor cement) camera)
  (with-slots
        (buf scale albedo normal height)
      actor
    ;; (map-g #'assimp-norm-pipeline buf
    ;;        :normal-map normal
    ;;        :scale scale
    ;;        :model-world (model->world actor)
    ;;        :world-view (world->view camera)
    ;;        :view-clip  (projection camera))
    (map-g #'generic-pipe ;;#'generic-tex-pipe
           buf
           :scale scale
           :color (v! 0 0 0)
;;           :albedo albedo
;;           :normap normal
;;           :light-pos *pointlight-pos*
           :cam-pos (pos camera)
;;           :height-map height
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
;; (defmethod draw ((actor assimp-thing) camera)
;;   (with-slots (buf scale) actor
;;     (map-g #'generic-pipe buf
;;            :scale 1f0
;;            :color (v! .2 .2 .2)
;;            :model-world (model->world actor)
;;            :world-view (world->view camera)
;;            :view-clip  (projection camera))))

(defmethod draw ((actor assimp-thing-with-maps) camera)
  (with-slots (buf scale albedo normal height) actor
    (map-g #'assimp-pipe buf
           :scale 1f0
           :albedo albedo
           :normap normal
           :height-map height
           :model-world (model->world actor)
           :world-view (world->view camera)
           :view-clip  (projection camera))))

(defmethod draw ((actor assimp-bloom) camera)
  (with-slots (buf scale) actor
    (map-g #'light-pipe buf
           :color (v3:*s (v! 1f0 .7 .3) 230f0)
           :model-world (model->world actor)
           :world-view (world->view camera)
           :view-clip  (projection camera))))

(defmethod draw ((actor celestial-sphere) camera)
  (with-slots (buf scale) actor
    (with-setf (cull-face) :front
      (map-g #'light-pipe buf
             :color (v! .5 .2 .5)
             :model-world (model->world actor)
             :world-view (world->view camera)
             :view-clip  (projection camera)))))
