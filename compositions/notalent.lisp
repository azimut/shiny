(in-package :shiny)

;; some inspiration from jazz.lisp

(fp 0 10)
(fp 1 20)
(fp 4 11)
(fp 3 20)

(fg 2.0)

(fp 0 0)
(fp 1 11)

(defvar *notes* nil)

(defun spawn-circle (pitch life
                     &key growth (pos (v2! 0)) (color (v3! 0))
                       (shape 0)
                       (selfcolor (v3! 1))
                       (pencil 1)
                       (rotation 0f0)
                       (fade 0))
  (let ((mypitch pitch))
    (when (eq 'cons (type-of mypitch))
      (setf mypitch (first mypitch)))
    (when (> mypitch 0)
      (push (make-instance 'circle-note
                           :pos pos
                           :pencil pencil
                           :color color
                           :selfcolor selfcolor
                           :shape shape
                           :growth growth
                           :life life
                           :die-at (+ (get-universal-time)
                                      (round life))
                           :pitch mypitch
                           :rotation rotation
                           :fade (if (= 1 fade) (float life) -1f0))
            *notes*)))
  pitch)

(defun spawn-circle (pitch &rest rest)
  pitch)

(fg 1.0)

;;--------------------------------------------------

(let ((notes (make-cycle '(0 41))))
  (defun ff (time)
    (let ((note (next notes)))
      (p time (spawn-circle
               note 1
               :shape 0
               :pencil 0
               :fade 1
               :growth '-
;;;;;;;               :selfcolor (v! 1 0 0)
               ;;:color (v! 0 0 0)
               )
         50 1 1))
    (aat (+ time 1) #'ff it)))

(defun ff ())
(ff (quant 4))
(fg .1)

(let ((rhythms (make-cycle '(2/3 4/3)))
      (notes   (make-weighting '(60 (0 .1) (53 .1)))))
  (defun f (time)
    (let ((rhythm (next rhythms))
          (note   (next notes)))
      (p time (spawn-circle note rhythm
                            ;; :shape 1
                            ;; :pos (v! (cm:interp note 30 -.5 90 .5) -.5)
                            :growth (if (= note 60) nil '+)
                            )
         30 rhythm 0)
      (aat (+ time rhythm) #'f it))))

(defun f ())
(f (tempo-sync #[1 b]))
(aat (quant 4) #'f it)

(defun fff ())
(let ((notes   (make-weighting '(60 62 (59 .1))))
      (rhythms (make-weighting '(4 3 (2 .5))))
      (amps    (make-weighting '(45 (50 .1)))))
  (defun fff (time)
    (let ((rhythm (next rhythms)))
      (p time (spawn-circle (+ (pick 0 12 12) (next notes))
                            rhythm
                            :selfcolor (v! 1 0 0)
                            :shape 3
                            :growth '-
                            :rotation (random 45f0))
         (next amps) .5 3)
      (aat (+ time rhythm) #'fff it))))

(fff (tempo-sync #[1 b]))
(fff (quant 4))

(freverb-toggle 1)
(freverb-preset 6)

(let* ((i (make-cycle '(i iv v iv) (make-weighting '(3 (2 .1)))))
 ;;      (b (make-cycle '(65 68 70) 10))
       (c (make-cycle (pval (make-chord-fixed 75 4 (pc-diatonic 0 '- (next i))))
                      (make-weighting '(1 (2 .4) 3))))
       (r (make-weighting '(.5 3/2 (2 .1)))))
  (defun bb (time)
    (let ((rhythm (next r))
          (notes (next c 3)))
      (p time (spawn-circle (nths notes
                                  (pick '(0 3) '(0) '(0) '(0 2)))
                            rhythm
;;                            :growth '-
;;                            :pos (v! (random 1f0) (random 1f0))
                            :shape 1
                            :rotation (if (> (first notes) 70)
                                          0f0
                                          (random 90f0))
;;                            :selfcolor (v3! 0 (random .8))
                            )
         65 rhythm 5)
      (aat (+ time rhythm) #'bb it))))

(defun bb ())

(fp 5 0)

(bb (tempo-sync #[1 b]))
(bb (quant 4))
(aat (quant 4) #'bb it)

(fg .4)
