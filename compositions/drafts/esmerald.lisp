(in-package :shiny)

;; https://musescore.com/user/17725126/scores/4265161

(setf (bpm *tempo*) 120)

(progn
  (defparameter *mf*      "/home/sendai/Downloads/Layers_of_Fear.mid")
  (defparameter *notes*   (subseq (get-measures-pair *mf* 0 8) 4))
  (defparameter *bass*    (subseq (get-measures-pair *mf* 1 8) 4))
  (defparameter *quarter* (first (get-midi-tempo *mf*))))

(alsaseq-start)
(alsaseq-pgmchange 1 44)
(alsaseq-pgmchange 2 28)

(let ((nd (make-cycle (subseq (get-measures-pair *mf* 0 12) 0)))
      (outer (make-cycle '(1f0 4f0))))
  (defun p3 (time)
    (destructuring-bind (ns ds) (next nd)
      (mapcar (op (play-midi-note time (+ -12 _) 120 _ 1)
                  (when (not (= 0 _1))
                    (setf (incandescent::pos incandescent::*camera*)
                          (incandescent::v! (serapeum:random-in-range -10f0 10f0)
                                            (serapeum:random-in-range -10f0 10f0)
                                            (serapeum:random-in-range -10f0 10f0)))
                    (setf (incandescent::rot incandescent::*camera*)
                          (q:point-at (rtg-math:v! 0 1 0)
                                      (incandescent::pos incandescent::*camera*)
                                      (rtg-math:v! 0 0 0)))
                    (setf incandescent::*outer* (next outer))))
              (reverse ns)
              (reverse ds))
      (aat (+ time #[(reduce #'max ds) b]) #'p3 it))))

(aat (tempo-sync #[1 b]) #'p3 it)
(defun p3 ())

(let ((inner (make-cycle '(1f0 2f0 3f0 4f0))))
  (defmethod play-midi-note :around (time pitch velocity duration channel)
    (when (= channel 0)
      (setf incandescent::*inner* (next inner)))
    (call-next-method)))

(let ((bass-notes (make-cycle (car (car *bass*))))
      (bass-durs (make-cycle (cdr (car *bass*)))))
  (defun p2 (time)
    ;;#+nil
    (let* ((bn (ensure-list (next bass-notes)))
           (d  (/ 1 (length bn))))
      (play-midi-arp time bn 90 d 0 d))
    ;;#+nil
    (mapcar (lambda (slot)
              (destructuring-bind (n d) slot
                (mapcar (op (play-midi-note time _ (rcosr 45 10 4) _ 2))
                        n
                        d)))
            (next *notes*))
    (aat (+ time #[1 b]) #'p2 it)))

(aat (tempo-sync #[1 b]) #'p2 it)
(defun p2 ())
