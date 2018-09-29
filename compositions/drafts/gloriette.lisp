(in-package :shiny)

;; --------------------------------------------------------------------
;; Gloriette for John Cage - Translated from notes from metalevel
;; TODO: stop using a global and try to use a local variable instead
;; --------------------------------------------------------------------

(defparameter *c0* 0)


(let* ((d (pval *c0*))
       (p (make-weighting `((g3 :weight ,d)
                            (a3 :weight ,d)
                            bf3
                            (c4 :weight ,d)
                            d4
                            (e4 :weight ,d)
                            f4
                            (g4 :weight ,d)
                            (0 :max 1 :weight .25))
                          1))
       (q (make-weighting (list 1/16
                                1/8
                                (make-cycle 1/32 2)))))
  (defun f (time)
    (let ((r (rhythm (next q))))
      (p time
       (cm:transpose (cm:keynum (next p)) 12)
       40 r 2)
      (p time
       (cm:transpose (cm:keynum (next p)) 0)
       40 r 1)
      (p time
       (cm:transpose (cm:keynum (next p)) -12)
       40 r 0)
      (aat (+ time #[r b]) #'f it))))

(all-piano 0)
(flush-pending)
(f (tempo-sync #[1 b]))
;;--------------------------------------------------

(let* ((d (pval *c0*))
       (p (make-weighting `((g3 :weight ,d)
                            (a3 :weight ,d)
                            bf3
                            (c4 :weight ,d)
                            d4
                            (e4 :weight ,d)
                            f4
                            (g4 :weight ,d)
                            (0 :max 1 :weight .25))
                          1))
       (q (make-weighting (list 1/16
                                1/8
                                (make-cycle 1/32 2)))))
  (defun f (time chan offset)
    (let ((r (rhythm (next q) 20)))
      (if (and (= 1 chan) (odds .1))
          (setf *c0* 0)
          (incf *c0* .1))
      (p
       time
       (cm:transpose (cm:keynum (next p)) offset)
       40
       r
       chan)
      (aat (+ time #[r b]) #'f it chan offset))))

(defun f ())

(f (tempo-sync #[4 b]) 0 -12)
(f (tempo-sync #[4 b]) 1 0)
(f (tempo-sync #[4 b]) 2 12)
;;--------------------------------------------------

(defun bird ())
(defun bird (time offset vel chan p q i &optional (w 0))
;;  (if (= chan 3) (setf vel 40))
;;  (if (not (= i 200))
  (let* ((i    (1+ i))
         (note (next p))
         (dur  (next q))
         (mul  12))
    (setf *c0* (cm:interp i 0 .5 90 4))
;; (setf *c0* 0)
;; (setf i 0)
    (p
     time
     (cm:keynum (cm:transpose note offset))
     vel
     (* dur (- mul 3) )
     chan)
    (aat (+ time (* mul dur)) #'bird it offset vel chan p q i w)))

(defun cage (offset vel chan)
  (let* ((d (pval *c0*))
         (p (make-weighting `((g3 :weight ,d)
                              (a3 :weight ,d)
                              bf3
                              (c4 :weight ,d)
                              d4
                              (e4 :weight ,d)
                              f4
                              (g4 :weight ,d)
                              (0 :max 1 :weight .25))
                            1))
         (q (make-weighting (list 1/16
                                  1/8
                                  (make-cycle 1/32 2)))))
    (aat (quant 4) #'bird it offset vel chan p q 0)))

;; 10 violin
;; 11 pizzicato (?)
;; 15 wind

(cage -12 40 0)
(cage   0 50 1)
(cage  12 30 2)

(fg .9)
(fp 2 30)

(fluidsynth:program-change *synth* 3 10)
(fluidsynth:program-change *synth* 1 33)
(fluidsynth:program-change *synth* 2 10)

(fluidsynth:program-change *synth* 3 26)
(fluidsynth:program-change *synth* 1 77)
(fluidsynth:program-change *synth* 2 33)

(fg .1)

(at (funcall *metro* (funcall *metro* 'get-beat 4))    #'cage -12 40 3)
(at (funcall *metro* (funcall *metro* 'get-beat 4.5))  #'cage 0 45 1)
(at (funcall *metro* (funcall *metro* 'get-beat 1.75)) #'cage 12 45 2)

(at (tempo-sync #[4 b]) #'cage -12 40 3)
(at (tempo-sync #[4.5 b]) #'cage 0 45 1)
(at (tempo-sync #[1.75 b]) #'cage 12 45 2)


(progn
  (at (tempo-sync #[1 b])  #'cage -12 30 3)
  (at (tempo-sync #[32 b]) #'cage 0 50 1)
  (at (tempo-sync #[64 b]) #'cage 12 40 2))

(flush-pending)
(off-with-the-notes *synth*)

#||
(process repeat 100
             for n = (next p)
             for r = (rhythm (next q) 65)
             for i from 0
             set w = (interp i 0 .5 90 4)
             output (new midi :time (now)
                         :duration r 
                         :keynum (transpose n offset))
             wait r)
||#

