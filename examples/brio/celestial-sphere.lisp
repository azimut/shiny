(in-package #:shiny)

(defclass celestial-sphere (actor)
  ((buf :initform (sphere))))

(defun make-celestial-sphere ()
  (let ((obj (make-instance 'celestial-sphere)))
    (push obj *actors*)
    obj))

(defmethod draw ((actor celestial-sphere) camera time)
  (with-slots (buf color scale) actor
    (with-setf* ((cull-face) :front
                 (depth-test-function) #'<=
                 (depth-mask) nil)
      (map-g #'celestial-pipe buf
             :color color
             :cam-pos (pos camera)
             :mod-clip
             (m4:* (projection camera)
                   (world->view camera))))))

(defun-g celestial-frag ((uv :vec2)
                         (frag-pos :vec3)
                         &uniform
                         (cam-pos :vec3)
                         (light-pos :vec2)
                         (color :vec3))
  (atmosphere (normalize frag-pos)
              (v! 0 6372000 0)
              (v! 0 0 -100)
              20f0
              6373000f0
              6471000f0
              (v! .0000055 .000013 .0000224)
              .000021
              8000f0 ;; rayleigh scale height
              1200   ;; mie scale height
              .758
              3 ;; 16 AND 8
              2))

(defpipeline-g celestial-pipe ()
  :vertex   (cubemap-vert g-pnt)
  :fragment (celestial-frag :vec2 :vec3 :vec3))
