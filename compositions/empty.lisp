(in-package :shiny)

;; needs quickload of lib/csound.lisp
;; uses examples/walls/ for visuals

(start-csound (gethash :xanadu *orcs*))

(bt:make-thread
 (lambda ()
   (loop (let ((frame (csound:csoundperformksmps *c*)))
           (when (not (= frame 0))
             (return))))))

;; XANADU
(make-play plucke "i1" 0)
(make-play pluck  "i2" 0)
(make-play newfm  "i3" 0 .2 2.0)

(defun f ())

(defun make-random-v3 ()
  (v! (+ 0 (random 40)) (+ 50 (random 30)) (+ 20 (random 10))))

(defparameter *wave* 1f0)

(let ((stars (make-heap '(.991 .99 .99)))
      (intent (make-heap '(5f0 1f0 2f0 3f0)))
      (crot  (make-heap '(nil t)))
      (chord (make-heap (make-chord-fixed 60 3 (scale 0 'ryukyu))))
      (here  (make-cycle '(t nil)))
      (lead  (make-heap (make-chord-fixed 80 5 (scale 0 'ryukyu)))))
  (defun f (time)
    (let ((n (next chord)))
      (setf *wave* 500f0)
      (play-newfm n 2 0 .2 2)
      ;; (if (odds .5)
      ;;     (progn
      ;;       (setf *light-factor* (next intent))
      ;;       (p time 60 60 1 0))
      ;;     ;; (progn
      ;;     ;;   (setf *head* (v! 0 50 0))
      ;;     ;;   (p time 60 60 .5 2 :pan 0)
      ;;     ;;   (p (+ time #[.5 b]) 60 60 .5 3 :pan 127))
      ;;     )
      ;; (progn
      ;;   (setf *crotate* (next crot))
      ;;   (pa time (list (pickl '(79 83 84 88 89)) 0)
      ;;       4 (rcosr 40 5 5) 4 4))
      ;; (when (next here)
      ;;   (setf *stars* (next stars))
      ;;   (play-pluck (+ 12 (next lead)) 4 0))
      )
    (aat (+ time #[2 b])
         #'f it)))

(fg .5f0)
(fp 4 23)
(defun f ())
(f (tempo-sync #[1 b]))


