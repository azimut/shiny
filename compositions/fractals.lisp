(in-package :shiny)

;; Fractal music
;; https://github.com/holgafreak/maxann-grace
;; https://web.archive.org/web/20000118053335/http://www.sci.fi/~mjkoskin/fractal.cm

(defun kamtorus (max &key (angle 1.3) (F 0.9)
                       (step 0.2) (stop 1) (orb -1) (comp 10))
  (let ((xold)
        (yold)
        (x)
        (y)
        (maxl)
        (skip)
        (compr comp)
        (o orb)
        (output))
    (loop :while (< o stop) :do
       (setf o (+ o step))
       (setf xold (/ o 3))
       (setf yold (/ o 3))
       (setf maxl (+ max 100))
       (setf skip 100)
       (loop :for i :from 0 :below maxl :do
          (setf x (+ (* xold (cos angle))
                     (* (- (* xold xold) yold)  (sin angle))))
          (setf y (- (* xold (sin angle))
                     (* (- (* xold xold)(* F yold)) (cos angle))))
          (when (< skip 0)
            (decf compr)
            (when (= compr 0)
              (push (list x y) output)
              (setf compr comp)))
          (decf skip)
          (setf xold x)
         (setf yold y)))
    (reverse output)))

(defun sign(x)
  (if (< x 0)
      (- 1)
    1))

(defun threeply (max &key (a -55) (b -1) (c -42) (comp 1))
  (let ((xold 0)
    (yold 0)
    (x)
    (y)
    (output)
    (compr comp))
    (loop for i from 0 below max do
      (setf x (- (+ (- yold (* (sign xold) (abs (sin xold)) (cos b))) c)
             (* xold (sin (+ a b c)))))
      (setf y (- a xold))
      (decf compr)
      (when (= 0 compr)
        (push (list x y) output)
        (setf compr comp))
      (setf xold x)
      (setf yold y))
    (reverse output)))

(defun lorenz (max &key  (dt 0.02) (a 5) (b 15) (c 1) (comp 1))
  (let ((xold 1)
    (yold 1)
    (zold 1)
    (x)
    (y)
    (z)
    (output)
    (tt dt)
    (compr comp))
    (loop for i from 0 below max do
      (setf x (+ xold (* (- a) xold tt) (* a yold tt)))
      (setf y (- (+ yold (* b xold tt)) (* yold tt) (* zold xold tt)))
      (setf z (+ zold (* (- c) zold tt) (* xold yold tt)))
; (format t "~D ~S ~S ~S ~%" i x y z)
      (decf compr)
      (when (= 0 compr)
        (push (list x y) output)
        (setf compr comp))
      (setf xold  x)
      (setf yold y)
      (setf zold z))
    (reverse output)))

;;; -------------------------------------------
;; Play
;;; -------------------------------------------


(defvar *n1* nil)
(defvar *t1* nil)
(defvar *n2* nil)
(defvar *n3* nil)


(setf *n1* (loop :for x :in (nthcdr 0 (kamtorus 12 :angle 1.2 :comp 9))
              :collect (-> x
                         (first)
                         (cm:interp -0.3 25 0.3 95)
                         (round))))

(setf *n2* (loop :for x :in (nthcdr 0 (kamtorus 24))
              :collect (-> x
                         (first)
                         (cm:interp -0.3 25 0.3 95)
                         (round))))

(setf *n2* (loop :for x :in (nthcdr 0 (lorenz 50 :dt 0.01 :a 4 :b 10))
              :collect (-> x
                         (first)
                         (cm:interp -15 25 15 95)
                         (round))))

(setf *t1* (loop :for x :in (nthcdr 0 (kamtorus 12))
              :collect (-> x
                         (first)
                         (cm:interp -0.3 0 0.3 2.0)
                         (round))))

(defun fractaltest (time &optional notes rhythms)
  
  (when (null notes)
    
    (if (cm:odds .9)
        (setf notes *n1*)
        (setf notes (reverse *n1*))))
  
  (when (null rhythms) (setf rhythms (repeat 24 '(1))))
;;    (setf rhythms *t1*))
  
  (let ((rhythm (first rhythms)))
    (play-midi-note
     time (first notes) (round (cosr 40 5 3/4)) rhythm 1)
    (aat (+ time #[rhythm b])
         #'fractaltest
         it (rest notes) (rest rhythms))))

(defun fractaltest2 (time &optional notes rhythms)

  ;; normal
  ;; transpose
  ;; reverse
  (when (null notes)    
    (let ((r (random 1.0)))
      (cond ((> r .5)(setf notes *n2*))
            ((> r .1)(setf notes (mapcar (lambda (x) (- x 12)) *n2*)))
            (t (setf notes (reverse *n2*))))))

  (when (null rhythms)
    (setf rhythms (repeat 24 '(.5))))
  
  (let ((rhythm (first rhythms)))
    (play-midi-note
     time (first notes) 30 (cm:pick .3 .2 .5) 2)
    (aat (+ time #[rhythm b])
         #'fractaltest2
         it (rest notes) (rest rhythms))))

(defun fractaltest3 (time &optional notes rhythms)
  
  (when (null notes)
    
    (if (cm:odds .9)
        (setf notes *n2*)
        (setf notes (reverse *n2*))))
  
  (when (null rhythms)
    (setf rhythms (cm:next (cm:new cm:weighting :of '(.5 .25 .5 .5 1 .75 .75 .5 .5 .5)) 't) ))
  
  (let ((rhythm (first rhythms)))
    (play-midi-note
     time (first notes) 70 rhythm 3)
    (aat (+ time #[rhythm b])
         #'fractaltest3
         it (rest notes) (rest rhythms))))

(fractaltest  (funcall *metro* (funcall *metro* 'get-beat 4)))
(fractaltest2 (funcall *metro* (funcall *metro* 'get-beat 4.5)))
(fractaltest3 (funcall *metro* (funcall *metro* 'get-beat 4.5)))

(flush-pending)

(fluidsynth:program-change *synth* 1 49)
(fluidsynth:program-change *synth* 2 1)


#|

(defvar *notes* nil)
(defvar *rhythms* nil)
(setf *notes* nil)
(setf *rhythms* nil)

(defun generate-notes()
  (let ((rhythm '(0))
        (note-data nil))
    (loop :for i :from 0 :below 96
       :do (push (list
                  i
                  (cm:pick 1.0 1.33 0.666))
                 note-data))
    (loop :for i :from 1 :below 5
       :do (push (/ 2.0 (expt 2 i)) rhythm))
    (push '-0.5 rhythm)
    (push '-1.0 rhythm)
    (values (reverse rhythm)
            (reverse note-data))))

(multiple-value-bind (x y) (generate-notes)
  (setf *rhythms* x) (setf *notes* y))

;;; func to calc note and rhythm     
(defun trig(x y maxx minx maxy miny start-n start-r mode)
  (let ((note-val)
        (picker)
        (rhyt)
        (diff)
        (rpick))
    (if (< minx 0)
        (setf diff (- x minx))
        (setf diff (+ x minx)))    
    (setf picker (round (+ (* diff maxx) start-n)))
    (when (> picker (length *notes*))
        (setf picker (1- (length *notes*))))
    (when (< picker 0) (setf picker 0))
    (setf note-val (nth picker *notes*))
    (unless (null mode)
      (if (< miny 0)
          (setf diff (- y miny))
          (setf diff (+ y miny)))
      (setf picker (round (+ (* diff maxy) start-r)))
      (when (< picker 0) (setf picker 0)) 
      (when (> picker (length *rhythms*))
          (setf picker (1- (length *rhythms*))))
      (setf rhyt (nth picker *rhythms*))
      (setf note-val (list (car note-val) rhyt)))    
    (format t "~S ~S ~S~%"x y note-val)
    note-val))


(defun trig(x y maxx minx  maxy miny start-n start-r mode)
  (let ((note-val)
        (picker)
        (rhyt)
        (diff)
        (rpick))
    (if (< minx 0)
        (setf diff (- x minx))
        (setf diff (+ x minx)))    
    (setf picker (round (+ (* diff maxx) start-n)))
    (when (> picker (length *notes*))
        (setf picker (1- (length *notes*))))
    (when (< picker 0) (setf picker 0))
    (setf note-val (nth picker *notes*))
    (unless (null mode)
      (if (< miny 0)
          (setf diff (- y miny))
          (setf diff (+ y miny)))
      (setf picker (round (+ (* diff maxy) start-r)))
      (when (< picker 0) (setf picker 0)) 
      (when (> picker (length *rhythms*))
          (setf picker (1- (length *rhythms*))))
      (setf rhyt (nth picker *rhythms*))
      (setf note-val (list (car note-val) rhyt)))    
    (format t "~S ~S ~S~%"x y note-val)
    note-val))



(defun fract-music (tim amp fract-pairs  &key (f-rhythm nil) (note-ex '(24 95)) (rhythm-ex '(0 5)) (func 'copy-list) (compr 1)) 
  (let ((x2 0)
        (y2 0)
        (note-val 0)
        (y 0)
        (yold 0)
        (maxx 0)
        (minx 0)
        (maxy 0)
        (miny 0)
        (compression compr)
        (f-p)
        (angle 0)) 
    (setf f-p (funcall func fract-pairs))
    (loop for n in f-p do
         (setf x2 (car n)) 
         (setf y2 (cadr n)) 
         (if (> x2 maxx)
             (setf maxx x2))
         (if (< x2 minx)
             (setf minx x2))
         (if (> y2 maxy)
          (setf maxy y2))
         (if (< y2 miny)
             (setf miny y2)))
    (format t "max x ~S min x ~S max y ~S min y ~S~%" maxx minx maxy miny)
    (setf maxy (/ (- (cadr rhythm-ex) (car rhythm-ex)) (- maxy miny))) ;; rhythms now scaled
    (setf maxx (/ (- (cadr note-ex) (car note-ex)) (- maxx minx)))
    (loop for i from 0 below (length f-p) do
         (setf x2 (car (nth i f-p)))
         (setf y2 (cadr (nth i f-p)))
         (when (= compression 0)
           (setf note-val (trig x2 y2 maxx 
                                minx maxy miny (car note-ex) 
                                (car rhythm-ex) f-rhythm))
           (print (car note-val))
           ;; (if (< (cadr note-val) 0)
           ;;     (object rest rhythm (abs (cadr note-val)))
           ;;     (object midi-note note (note (car note-val)) rhythm (cadr note-val) amplitude amp))
           (setf compression compr))
         (setf compression (1- compression)))))

|#
