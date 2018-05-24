(in-package :somecepl)
;; https://www.youtube.com/watch?v=4lyI7O7kLP0

(all-piano 0)

(fp 1 20 0)
(fp 2 20 0)
(fp 3 20 0)
(fp 4 20 0)

(fp 1 30 0)
(fp 2 30 0)
(fp 3 20 0)
(fp 4 20 0)

(fp 22 0)

(defparameter *triggered* (trigger-once))
(defparameter *triggered2* (trigger-once))

(defun ff ())
(defun ff (time)
  (let* ((c (make-chord 60 80 3  (scale 0 'phrygian)))
         (bass (+ -12 (first c)))
         (c (reverse c))
         )
    (when (zmodt 1)
     ;; (p time bass 50 1 22)
      (pa time c .333 '(60 50 50 50) '(0 10 20 10) (pick .4 .4 .1 .3 .5))
      )
    )
  (let ((c (+ 1 (random 4))))
    (when (zmodt 4)
      (if (or (= c 1)
              (= c 2))
          (with-trigger (*triggered2* time)
            (p it (pick 70 72 73) 50 7 c))
          (with-trigger-expire (*triggered* time 1)
            (p time (pick 70 72 73) 50 7 c)))
      )
    (aat (+ time 1) #'ff it)))

(ff (quant 4))
(defun ff ())
