(in-package :shiny)

;; -----------------------------------------------
;; Extempore - An Overview
;; https://vimeo.com/21956071
;; at 11:30
;; -----------------------------------------------
#|
(define loop
    (lambda (beat dur root)
      (for-each (lambda (p offset)
                  (play (+ offset) sampler p 100 (* 2.0 dur)))
                (pc:make-chord 40 (cosr 75 10 1/32) 5
                               (pc:chord root (if (member root '(10 8))
                                                  '^7
                                                  '-7)))
                '(1/3 1 3/2 1 2 3))
      (callback (*metro* (+ beat (* .5 dur))) 'loop (+ dur beat)
                dur
                (if (member root '(0 8))
                    (random '(2 7 10))
                    (random '(0 8))))))
(loop (*metro* get-beat 4) 4 0)
|#

(fluidsynth:set-reverb *synth* 0.7d0 0.3d0 0.5d0 0.9d0)

(defparameter *metro* (make-metro 60))
 
(defvar *beat-offset* nil)
(setf *beat-offset* '(1/3 1 3/2 1 2 3))
(setf *beat-offset* '(1/3 4 1 1.5))
(setf (bpm *tempo*) 60)
  
(flush-pending)

(defvar *scale* (scale 0 'aeolian))

(defun sometune (beat time dur root)
  (let ((n-beat (+ beat 4)))
    (if (= 0.0 (mod beat 12))
        (progn (p time (+ root 72) 90 4 3)
               (p (+ time 6)
                               (pc-relative (+ root 72) (random-elt #(-1 -2 -3)) *scale*)
                               90 4 4))
        (p time (pc-quantize (+ root 67) *scale*) 90 2 2))
    (p (+ time 3) 36 90 (* 3.0 dur) (+ 10 (random 10)))
    (mapcar (lambda (x y) (p (+ time y) x 100 (* 2.0 dur) 0))
            (make-chord 40 (rcosr 75 10 1/32) 5
                        (pc-chord root (if (member root '(10 8))
                                        '^7
                                        '-7)))
            *beat-offset*)
    (aat (funcall *metro* n-beat) #'sometune n-beat it dur
         (if (member root '(0 8))
             (random-list '(2 7 10))
             (random-list '(0 8))))))

(sometune (funcall *metro* 'get-beat 4)
          (funcall *metro* (funcall *metro* 'get-beat 4))
          4 0)

(defun sometune ())

(fluidsynth:program-change *synth* 2 53)
(fluidsynth:program-change *synth* 3 52)
(fluidsynth:program-change *synth* 1 1)
(fluidsynth:program-change *synth* 0 1)


(fluidsynth:program-change *synth* 2 110)
(fluidsynth:program-change *synth* 2 78)
(fluidsynth:program-change *synth* 3 78)


(setf *beat-offset* (reverse *beat-offset*))

(setf *beat-offset* '(0 0.1 1/3 0.7 0.9 0.9))
(setf *beat-offset* '(0 0.2 1/3 0.5 0.8))
(setf *beat-offset* '(0 0.2 0.4 0.6 0.8))
(setf *beat-offset* '(0 0.1 0.2 0.3 0.4))
(setf *beat-offset* '(0 0.1 0.11 0.13 0.15 0.17 0.2 0.4 0.5 0.55 0.6 0.8))


