(in-package :somecepl)

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

(defvar *beat-offset* nil)
(setf *beat-offset* '(1/3 1 3/2 1 2 3))
(setf *beat-offset* '(1/3 4 1 1.5))
(setf (bpm *tempo*) 30)

(progn
  (setf (fluidsynth:setting *fluid-settings* "synth.gain") .9)
  (setf (bpm *tempo*) 90)
)
  
(sometune 4 0)
(flush-pending)

(defun sometune (dur root)
  (let ((time (now)))
;;    (aat (+ time #[3 b]) #'play-midi-note it 36 90 (* 3.0 dur) 1)
    (mapcar (lambda (x y) (aat (+ time #[y b]) #'play-midi-note it x 100 (* 2.0 dur) 0))
            (make-chord 40
                        (rcosr 75 10 1/32)
                        5
                        (chord root (if (member root '(10 8))
                                        '^7
                                        '-7)))
            *beat-offset*)
    (at (+ time #[4 b]) #'sometune
        dur
        (if (member root '(0 8))
            (random-list '(2 7 10))
            (random-list '(0 8))))))

(setf *beat-offset* (reverse *beat-offset*))

(setf *beat-offset* '(0 0.1 1/3 0.7 0.9 0.9))
(setf *beat-offset* '(0 0.2 1/3 0.5 0.8))
(setf *beat-offset* '(0 0.2 0.4 0.6 0.8))
(setf *beat-offset* '(0 0.1 0.2 0.3 0.4))
(setf *beat-offset* '(0 0.1 0.11 0.13 0.15 0.17 0.2 0.4 0.5 0.55 0.6 0.8))
