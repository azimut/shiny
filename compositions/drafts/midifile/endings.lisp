(in-package #:shiny)

(defparameter *mf*
  "/home/sendai/Downloads/Octopath_Traveler_-_Haanits_Theme.mscz.mid")

(defparameter *notes*
  (subseq (get-measures-pair *mf* 999 1.5 0) 0))
(defparameter *sing*
  (subseq (get-measures-pair *mf* 999 1.5 1) 0))

(defparameter *pc-follow*
  (delete-duplicates
   (sort (mapcar (lambda (x) (mod x 12))
                 (subseq (get-notes *mf* 1) 0 16))
         #'<)))

(defun f ())

(let ((measures (make-cycle *notes*))
      (sing     (make-cycle *sing*)))
  (defun f (time)
    (let* ((measure   (next measures))
           (notes     (first measure))
           (durations (second measure)))
      (play-midi-arp time notes 50 durations 0 (d2o durations)))
    (let* ((n (pc-random 70 90 *pc-follow*))
           (i (max 0.1 (+ -3 (cm:interp n 20f0 .1f0 100f0 5f0)))))
      (at (+ time #[.5 b]) (lambda () (setf *scale* i)))
      ;; (play-midi (+ #[.5 b] time)
      ;;            n 30 .2 0)
      )
    
    (let* ((measure (next sing))
           (notes   (first measure))
           (durations (second measure)))
      (setf *uvs* (+ -.5 (random 1f0)))

      (progn
        (setf *rotcube* (drunk 5 5 :low 0 :high 30))
        ;; (play-midi time
        ;;            (pc-relative (+ 12 (first notes))
        ;;                         (1+ (random 11))
        ;;                         *pc-follow*)
        ;;            30 .3 0)
        )
      ;;(setf *rotcube* 0)
      (play-midi-arp time notes 1 durations 0 (d2o durations))
      )
    (aat (+ time #[1.5 b]) #'f it))
  )
(setf *actors* nil)
(make-cubemap)
(make-thing)
(make-piso (v! 0 -2 0) (q:identity) -.2)
;;(make-piso (v! 0 -2 0) (q:from-axis-angle (v! 0 1 0) (radians -180)) -.2)
;;(make-piso (v! 0 2 0) (q:from-axis-angle (v! 1 0 0) (radians -180)) -.2)
(make-piso (v! 0 2 0) (q:*
                       (q:from-axis-angle (v! 0 1 0) (radians -180))
                       (q:from-axis-angle (v! 1 0 0) (radians -180)))
           -.2)

(make-piso (v! 0 -2 0))
(f (now))
