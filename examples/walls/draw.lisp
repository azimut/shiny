(in-package :somecepl)

(defgeneric draw (actor res))
(defvar *light-factor* 1f0)

(defmethod draw ((actor lead) res)
  ;; (with-slots (buf) actor
  ;;   (map-g #'lead buf
  ;;          :time (mynow)
  ;;          :light-factor *light-factor*
  ;;          :model-world (model->world actor)
  ;;          :world-view (world->view *currentcamera*)
  ;;          :view-clip  (projection *currentcamera*
  ;;                                  (x res)
  ;;                                  (y res))))
  )

(defmethod draw ((actor voz) res)
  (with-slots (buf) actor
    (with-instances 10
      (map-g #'pipe buf
             :time (mynow)
             :lead-pos (pos *lead*)
             :light-factor *light-factor*
             :model-world (model->world actor)
             :world-view (world->view *currentcamera*)
             :view-clip  (projection *currentcamera*
                                     (x res)
                                     (y res))))))

(defmethod draw ((actor sphere) res)
  (with-setf (cull-face) :front
    (with-slots (buf) actor
      (map-g #'white buf
             :time (mynow)
             :light-factor *light-factor*
             :model-world (model->world actor)
             :world-view (world->view *currentcamera*)
             :view-clip  (projection *currentcamera*
                                     (x res)
                                     (y res))))))

(defmethod draw ((actor wall) res)
  (with-slots (buf) actor
    (map-g #'white buf
           :time (mynow)
           :model-world (model->world actor)
           :world-view (world->view *currentcamera*)
           :view-clip  (projection *currentcamera*
                                   (x res)
                                   (y res)))))
