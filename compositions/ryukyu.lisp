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


(defun newscale2 (time pitch)
  (play-midi-note time pitch (if (cm:odds .3) 0 30) .3 1)
  (aat (+ time #[(random-elt #(.5 .25 .5 .5)) b])
       #'newscale2 it
       (if (and (cm:odds .4)
                (ispitch pitch '(0))
                (not (= pitch 60)))
           60
           (relative
            pitch
            (random-elt #(1 -1))
            (scale 0 'ryukyu)))))

(newscale2 (tempo-sync #[1 b]) 60)

(setf *metre2* (make-metre '(8) .5))

(setf *metre3* (make-metre '(8) .5))

;; Nice overall, BUT the chord progression feels
;; awkward at times..
;; Also I would like strings eventually...how sync them?
(defun newscale (beat time &optional (pitch 60))
  (let ((n-beat (+ beat .5)))

    ;; (when (funcall *metre2* beat 1.0)
    ;;   (play-midi-note time 48
    ;;                   45
    ;;                   (random-elt #(1 1.5)) 1))

    ;; piano chord (?
    ;; (when (funcall *metre3* beat 1.0)
    ;;   (let ((pitch (+ pitch 12)))
    ;;     (play-midi-arpeggio
    ;;      time
    ;;      (list pitch (+ pitch 3 (random 2)) (+ pitch 7))
    ;;      35 .3 1)))

    ;; (when (funcall *metre2* beat 1.0)
    ;;   (play-midi-note time (+ 24 pitch) 30 65 (+ 23 (random 2))))
    
    ;; violin
    ;; (when (funcall *metre* beat 1.0)
    ;;   (play-midi-note time pitch 18 2 20)
    ;;   (play-midi-note time (+ 12 pitch) 20 40 10))

    ;; chord
    ;; (when (funcall *metre* beat 1.0)
    ;;   (dolist (x (make-chord 48
    ;;                          72
    ;;                          (random-elt #(2 3)) '(0 4 5 7 11)))
    ;;     (play-midi-note time x 25 4 8)))
    
    ;; (when (funcall *metre* beat 1.0)
;;       (if (cm:odds .9)
;;           (let ((c (reverse (make-chord
;;                              60
;;                              84
;;                              3 '(0 4 5 7 11)))))
;;             ;; ghost            
;; ;;            (play-midi-note time (+ 24 pitch) 30 65 (+ 23 (random 2)))
;;             (play-midi-arpeggio time c 25 1 8))
;;           (dolist (x (make-chord 48
;;                                  72
;;                                  (random-elt #(2 3)) '(0 4 5 7 11)))
;;             (play-midi-note time x 25 4 7))))

    (play-midi-note time pitch (cm:odds .1 25 35) .3 1)
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

(fluidsynth:program-change *synth* 10 34)
(fluidsynth:program-change *synth* 20 34)
(fluidsynth:program-change *synth* 20 34)

(fluidsynth:program-change *synth* 2 43)
(fluidsynth:program-change *synth* 8 77)
(fluidsynth:program-change *synth* 7 77)

(fluidsynth:program-change *synth* 23 43)
(fluidsynth:program-change *synth* 24 43)
(fluidsynth:program-change *synth* 25 43)

(setf (fluidsynth:setting *fluid-settings* "synth.gain") 1.)

(flush-pending)
(off-with-the-notes *synth*)
