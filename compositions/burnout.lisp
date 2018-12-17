(in-package #:shiny)
;;--------------------------------------------------
;; Code for "burnout"
;; https://www.youtube.com/watch?v=TcabByrOzow

;;--------------------------------------------------
;; Visual interaction code
(setf *exposure* 0f0)
(setf *sun-intensity* .1f0)
(box 10f0 10f0 10f0 t) ;; EVALUATE! to cache the geometry

(defparameter *g-exposure* (make-line (iota 10 :start .1 :step .05)))
(defparameter *g-sun* (make-line (iota 5 :start 0f0 :step 1f0)))

(defmethod play-midi :after (time note velocity duration channel))
(defmethod play-midi :after (time (note fixnum) velocity duration channel)
  (at time
      (lambda () (appendf *actors*
                     (list
                      (make-instance
                       'planet
                       :name :asda
                       :buf (box 10f0 10f0 10f0 t)
                       :pos (v! (between -45f0 20f0)
                                70f0
                                (between -100f0 0f0))
                       :scale (random 1f0)
                       :rot (q:from-axis-angle (v! (random 1f0)
                                                   (random 1f0)
                                                   (random 1f0))
                                               (radians 30))))))))

(defun dall (class-name)
  (setf *actors*
        (delete-if
         (lambda (x) (string= class-name (class-name (class-of x))))
         *actors*)))
(defun ds (name)
  (declare (keyword name))
  (setf *actors*
        (delete-if (lambda (x) (eq name (slot-value x 'name)))
                   *actors*)))
(defun cs (name &optional
                  (buf (box))
                  (pos (v! 0 2 0))
                  (scale 1f0)
                  (instance 'box)
                  (rot (q:identity)) )
  (or (and (position-if (lambda (x) (equal name (slot-value x 'name))) *actors*)
           (ds name))
      (appendf
       *actors*
       (list
        (make-instance
         instance
         :scale scale
         :buf buf
         :name name
         :pos pos
         :rot rot)))))

;;--------------------------------------------------
;; Dexer - "Cirrus" sound
;; Nudruz - code shamefully copied from official repo "scratch.lisp"
(defparameter *c5-branch*
  (cm::generic-branch
   #'cm::tonnetz-func  
   (cm::mod12 (cm::expand (list (cm::m7 (cm::indices 12)) '(0 4 7))))))

(defparameter *smoothpits*
  (cm::shuffle-all
   (cm::smoothlist
    (cm::transp (mapcar #'cm::stack-up (cm::shuffle-all (cm::flatter *c5-branch*))) 60))))

(defparameter *notes* (make-cycle (cm::flatten *smoothpits*)))
(defparameter *durs*  (make-cycle (cm::ornadurs *smoothpits* .5)))

(f (now))
(defun f ())
(defun f (time)
  (let* ((ns (next *notes* 3))
         (ds (next *durs* 3))
         (d  (reduce #'+ ds)))
    
    (dall "BOX")
    (dall "PLANET")
    (setf *sun-intensity* (next *g-sun*))
    (setf *exposure* (* 999200f0 (next *g-exposure*)))
    (play-midi-arp time ns 40 ds 0 (d2o ds))

    (if (member 0.25 ds)
        (progn
          (dall "BOX")
          (dall "PLANET")
          (play-midi time (+ 12 (car ns)) 80 d 0)
          (cs (gensym)
              (box 10f0 10f0 10f0 t)
              (v! (between -4f0 4f0)
                  0f0
                  (between -100f0 0f0))
              (random 10f0)
              'box
              (q:from-axis-angle (v! (random 1f0)
                                     (random 1f0)
                                     (random 1f0))
                                 (radians 30)))
          )
        )
    

    (aat (+ time #[d b])
         #'f it)))
