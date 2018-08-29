(in-package :shiny)

;; p - amplitude
;; pick - generic
;; weight - without :weight

(defparameter *c* (make-chord 40 50 3 (ov-pc-scale :scriabin)))
(defparameter *c* '(40 45 49))
(callback (quant 3) #'(lambda () (setf *c* (subseq (chord-degree :iii :c3 :scriabin) 0 3))))

(fg .5)

(let ((h (make-heap *c*)))
  (defun bass (time)
    (p time (+ -12 (next h)) (rcosr 40 5 1/2) 3 3)
    (aat (+ time 3) #'bass it)))

;; invert-chord
;; arpegio subseq
(defun ff (time)
  (let* ((n-notes (pick 2 3 1))
         (beat-dur 1)
         (l-notes (/ beat-dur n-notes))
         (*c* (mapcar (lambda (x) (+ 12 x)) *c*)))
    ;; (and (odds .2) (p time (mapcar (lambda (x) (+ 7 x))
    ;;                                (loop repeat (+ 1 (random n-notes))
    ;;                                   :collect (pickl *c*)))
    ;;                   (rcosr 60 4 1/2)
    ;;                   1 2))
    (pa time (pick (subseq *c* 0 n-notes)
                   (reverse *c*)
                   ;;(invert-chord *c* (pick 2 1 0))
                   )
        l-notes
        '(55 55 60) '(0 1 2)
        (list (pick l-notes)
              (pick l-notes (/ l-notes 2))
              (pick l-notes (/ l-notes 2))))
    (aat (+ time beat-dur) #'ff it)))


(ff (quant 3))
(bass (quant 3))

(fp 0 20)
(fp 1 20)
(fp 2 20)

(fp 3 51)
(fp 2 41)

(defun ff ())
(defun bass ())

;; --------------
;; EWW
(defvar *c* '())
(let ((s (ov-scale :c4 :scriabin)))
  (defun fff (time)
    (let ((beat-length (* .4 16))
          (*c* (list (first *c*)
                     (third *c*)
                     (second *c*))))
      (pa time (transpose *c* -3) .3 50 1 .2)
      (if (odds .5)
          (pa (+ time .9) *c* .3 50 1 '(.2 .2 .3))
          (p (+ time .9) *c* 40 .9 1))
      (if (odds .5)
          (pa (+ time 1.8) (transpose *c* +3) .3 50 1 .2)
          (pa (+ time 1.8) (invert-chord *c* 3) .3 50 1 .2))
      (p (+ time 2.7) (first *c*) 50 3 1)
      (aat (+ time beat-length) #'fff it))))

(fff (quant (* .4 16)))
(defun ff ())
(defun ff (time)
  (let* ((beat-length (* .4 16))
         (pc (ov-pc-scale :scriabin))
         (c (make-chord-alberti
             55 75 pc)))
    (setf *c* c)
;;    (setf c (transpose c -12))
    (p time (pc-relative (first c)
                         (pick -8 -5 -3)
                         pc)
       25 beat-length 8)
    ;; (pa time c
    ;;     (cumsum (repeat 4 (list (/ beat-length 10))))
    ;;     30 10 (/ beat-length 4))
    (aat (+ time beat-length) #'ff it)))
(ff (quant (* .4 16)))
