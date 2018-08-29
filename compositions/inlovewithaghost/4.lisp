(in-package :somecepl)

(let ((i (make-cycle '(iv v iii vi))))
  (defun ff (time)
    (let* ((pc (pc-diatonic 0 'major (next i)))
           (lchord 1) ;; !
           (larp (/ 1 lchord))
           (mychord (make-chord 55 75 lchord pc))
           ;;(mychord (mapcar (lambda (x) (pc-relative x -3 pc)) mychord))
           (r       (reverse mychord))
                      (r (mapcar (lambda (x) (pc-relative x 5 pc)) r))
           ;;(r (mapcar (lambda (x) (pc-relative x -100 pc)) r))
           (rr      (append (list (pc-relative (first r) -1 pc))
                            (subseq r 1 lchord))))
      (pa time (nths mychord '(0 1 2)) (* 2 larp) 60 13 (* 2 larp))
      (pa (+ time 2) (nths mychord '(1 0 1)) (* 2 larp) 60 13 (* 2 larp))
      
      (if (odds 1)
          (pa   time
                ;;r
                (pick r mychord)
                ;;(pick r (reverse (mapcar (lambda (x) (pc-relative x +1 pc)) mychord)))
                larp '(65 55 50 50) 5 (+ -.1 larp))
          (pa time r '(0 .5 .5 .5) 50 5 .5))
      (if (odds 1)
          (pa  (+ 1 time) rr larp 50 6 (+ -.1 larp))
          (p  (+ 1 time) rr 50 (+ -.1 larp) 6))
      (if (odds 1)
          (pa  (+ 2 time)
               ;;r
               (pick r rr)
               ;;(pick r (mapcar (lambda (x) (pc-relative x +1 pc)) mychord))
               larp '(65 50 50 50) 7 (+ -.1 larp))
          (pa (+ 2 time) r '(0 .5 .5 .5) 50 5 .5))

      ;; bass
      (if (odds .5)
          (p   (+ time 1) (pick-random-list mychord 2) (rcosr 55 5 1/2) 3 10)
          (progn
            (p   (+ time 1) (first mychord) (rcosr 55 5 1/2) 2 2)
            (p   (+ time 3) (pick (list (second mychord) (+ -12 (third mychord)))
                                  (first mychord))
                 (rcosr 55 5 1/2) 1 2)))
      (pa  (+ 3 time) rr larp 50 8 (+ -.1 larp))
      (aat (+ time 4) #'ff it))))

(defun ff ())
(fp 2 20)
(ff (quant 4))
(fp 10 20)
(fg 1.0)

(fp 10 20 0)
