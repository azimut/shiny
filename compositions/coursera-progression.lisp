(in-package :somecepl)

;; lesson 3 - basic progresssions

;; I IV V I - familiar
(let ((i (new cycle :of '(i vi ii v))))
  (defun f (time)
    (p (now)
       (make-chord 50 70 3 (pc-diatonic 0 'major (next i)))
       50 2 3)
    (aat (+ time #[2 b]) #'f it)))
(f (now))

(fp 3 44) ;; tremolo!
;; I IV V I
(let ((i (new cycle :of '(i iv v))))
  (defun f (time)
    (p (now)
       (make-chord 50 70 3 (pc-diatonic 0 'major (next i)))
       50 2 3)
    (aat (+ time #[2 b]) #'f it)))
(f (now))

;; I V I
(all-piano 0)

(fp 2 41)
(fp 3 40)

;; using function constants (might be I can use this for a macro later)
;; another way is with a closure (i think) like make-tempo
(let ((c (new cycle :of '(2 3)))
      (i (new cycle :of '(i v))))
  (defun f (time)
    (p (now)
       (make-chord 50 70 3 (pc-diatonic 0 'major (next i)))
       30 2 (next c))
    (aat (+ time #[2 b]) #'f it)))

(f (now))

#|
            /----------        
IV - vii - iii - vi - IV - V   - I - * (ANY)
                      ii   vii
|#

(defparameter *mymarkov*
  (new markov :of '((i   :-> ii iii iv v vi vii)
                    (ii  :-> v vii)
                    (iii :-> vi iv ii)
                    (iv  :-> v vii)
                    (v   :-> i)
                    (vi  :-> iv ii)
                    (vii :-> i ))))


(p (now) (make-chord 50 70 3 (pc-diatonic 0 'major (next *mymarkov*))) 60 1.5 3)

(defun f (time)
  (p (now) (make-chord 50 70 3 (pc-diatonic 0 'major (next *mymarkov*))) 60 1.5 3)
  (aat (+ time #[1.5 b]) #'f it))

(f (now))

(defparameter *assocmarkov*
  '((i   ii iii iv v vi vii)
    (ii  v vii)
    (iii vi iv ii)
    (iv  v vii)
    (v   i)
    (vi  iv ii)
    (vii iii i)))

(defun f (time degree)
  (p (now) (make-chord 50 70 3 (pc-diatonic 0 'major degree)) 60 1.5 3)
  (aat (+ time #[1.5 b]) #'f it (pickl (cdr (assoc degree *assocmarkov*)))
       ))
(f (now) 'i)

;;; -------------
;; I <-> V/vii
;;; -------------

(fp 3 52)
;; ew!
(defparameter *mychord*
  (new cycle :of
       `((maj 1)
         ,(new cycle :of '((maj 5) (min 7)) :for 1))))
(p (now) (make-chord 50 70 3 (apply #'pc-diatonic 0 (next *mychord*))) 60 1.5 3)

;;; all major
(defparameter *mychord*
  (new cycle :of `(i ,(new weighting :of '(v vii) :for 1))))
(p (now) (make-chord 50 70 3 (pc-diatonic 0 'maj (next *mychord*))) 60 1.5 3)

;; some glitch with midi
(defparameter *r* (new cycle :of '(1.5 2)))

(defun f (time)
  (p (now) (make-chord 50 70 3 (pc-diatonic 0 'maj (next *mychord*))) 60 (next *r*) 3)
  (aat (+ time #[1.5 b]) #'f it))
(f (now))

;; without randomness in place i can just write it down, without nesting
;; but still need the cycle
(defparameter *mychord* (new cycle :of '((maj 1) (maj 5) (maj 1) (min 7))))
(p (now) (make-chord 50 70 3 (apply #'pc-diatonic 0 (next *mychord*))) 60 1.5 3)

;; unless I had a destructive pick and rotate function
(defparameter *mychord* '((maj 1) (maj 5) (maj 1) (min 7)))
;;?
;;(p (now) (make-chord 50 70 3 (apply #'pc-diatonic 0 (pick *mychord*))) 60 1.5 3)

(progn
  (fp 3 (random 60))
  (p (now) (make-chord 50 70 3 (pc-diatonic 0 'maj (pick 1 (pick 5 7)))) 60 1.5 3))


;;; ---------------------
;; Lesson 4 - voice leading
;;;

;; In classical era music, one often "tries" to keep the
;;   common tones between two chords when voice leading.

(let ((i (new cycle :of '(i iv))))
  (defun f (time)
    (p (now)
       (make-chord 50 70 3 (pc-diatonic 0 'major (next i)))
       50 2 3)
    (aat (+ time #[2 b]) #'f it)))
(f (now))


;;;
;; overtone / extemp_piano.clj
;;;
;; (def chord-prog
;;   [#{[2 :minor7] [7 :minor7] [10 :major7]}
;;    #{[0 :minor7] [8 :major7]}])


