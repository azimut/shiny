(in-package :shiny)

;; extempore - a study in part
;; this basically covers a scale descendent
;; cambiar los random
;; +
(fp 8 52)
(defun g ())
(defun g (time start pitch &optional duration)
  (print (format nil  "~a ~a" start pitch))
  (p time pitch 60 duration 8)
  (aat (+ time duration) #'g it
       (if (> pitch (- start 12))
           start
           (pc-relative start -1 *phrygian*))
       (if (> pitch (- start 12))
           (pc-relative pitch -1 *phrygian*)
           start)
       (random-elt #(1 2 3))))

(g (quant 4) 72 72 1)

;;; extempore - an evening livecoding
(fp 11 50)
(defun g (time)
  (let ((c (make-chord 65 75 2 (list (pick 10 0 0 1)
                                     (pick 1 3 3 5)
                                     (pick 7 8 8 10)))))
    (print c)
    (p time c 60 4 11))
  (aat (+ time 4) #'g it))
(g (quant 4))


;;; same , but synth
(defun g (time)
  (let ((c (make-chord 65 75 2 (list (pick 10 0 0 1)
                                     (pick 1 3 3 5)
                                     (pick 7 8 8 10)))))
    (print c)
    (mapcar (lambda (x) (synth 'soprano
                          :fmod .1
                          :freq (midicps x)
                          :amp .2 :sus 4)) c)
;;    (aat (+ time 4) #'g it)
    ))
  
(g (quant 4))
