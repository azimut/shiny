(in-package :somecepl)

;;; -----------
;; ryukyu scale (aka ionic pentantonic scale)
;;
;; "Algorithmic Composition with Impromptu"
;; https://vimeo.com/6587048
;; https://ianring.com/musictheory/scales/2225
;;
;; Just a random walk over the scale has an interesting sound
;;; -----------

(defvar *metro* nil)
(defvar *metre* nil)
(setf *metro* (make-metro 90))
(setf *metre* (make-metre '(20) .5))


(defvar *metre2* nil)
(setf *metre2* (make-metre '(10) .5))


(defun newscale (time pitch)
  (play-midi-note time pitch (if (cm:odds .3) 0 30) .3 1)
  (aat (+ time #[(random-elt #(.5 .25 .5 .5)) b])
       #'newscale it
       (if (and (cm:odds .4)
                (ispitch pitch '(0))
                (not (= pitch 60)))
           60
           (relative
            pitch
            (random-elt #(1 -1))
            (scale 0 'ryukyu)))))

(newscale (tempo-sync #[1 b]) 60)

(setf *metre2* (make-metre '(8) .5))

;; Nice overall, BUT the chord progression feels
;; awkward at times..
;; Also I would like strings eventually...how sync them?
(defun newscale (beat time &optional (pitch 60))
  (let ((n-beat (+ beat 1)))

    (print beat)
    (when (funcall *metre2* beat 1.0)
      (play-midi-note time 48 35 1 2))
    
    ;; (when (funcall *metre* beat 1.0)
    ;;   (dolist (x (make-chord 48
    ;;                          72
    ;;                          (random-elt #(2 3)) '(0 4 5 7 11)))
    ;;     (play-midi-note time x 30 4 8)))
    
    (when (funcall *metre* beat 1.0)
      (if (cm:odds .3)
          (let ((c (reverse (make-chord
                             60
                             84
                             3 '(0 4 5 7 11)))))
            (play-midi-arpeggio time c 35 1 8))
          (dolist (x (make-chord 48
                                 72
                                 (random-elt #(2 3)) '(0 4 5 7 11)))
            (play-midi-note time x 35 4 7))))
    
    (play-midi-note time pitch (cm:odds .1 20 30) .5 1)
    (aat (funcall *metro* n-beat) #'newscale
         n-beat it
         (if (and (cm:odds .4)
                  (ispitch pitch '(0))
                  (not (= pitch 60)))
             60
             (relative pitch (random-elt #(1 -1)) '(0 4 5 7 11))))))

(newscale (funcall *metro* 'get-beat 4)
          (funcall *metro* (funcall *metro* 'get-beat 4)))

(setf *metre* (make-metre '(2 3 2) 0.5))
(setf *metre* (make-metre '(3) .5)) ;; 3/8

(all-piano *synth*)

(fluidsynth:program-change *synth* 2 43)
(fluidsynth:program-change *synth* 8 *nk-piccolo*)
(fluidsynth:program-change *synth* 7 77)
(fluidsynth:program-change *synth* 1 80)
