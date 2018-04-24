;;;
;; Moonlight
;; https://github.com/devinroth/GenerativeMusic
;; trying to translate it...but is ending up in a different thing
;; which is fine because I am getting used to "traditional"
;; things like bass notes and arpeggios
;;;

(defun play-midi-arpeggio (time notes vel dur chan)
  (let ((l-notes (length notes)))
    (mapcar (lambda (x y) (play-midi-note (+ time #[x b])
                                     y vel dur chan))
            (do ((i dur (+ i dur))
                 (l '() (push i l)))
                ((> i (* dur l-notes)) (reverse l)))
            notes)))

(defun play-note-mord (time pitch vol dur pc chan)
  (play-midi-note time
                  pitch
                  (- vol 10)
                  (/ dur 2)
                  chan)
  (play-midi-note (+ time #[1/4 b])
                  (pc-relative pitch 1 pc)
                  (- vol 10)
                  (/ dur 2)
                  chan)
  (play-midi-note (+ time #[1/4 b])
                  pitch
                  vol dur chan))

(defun make-chord-random (&optional (upper 100))
  (let* ((n (random 12))
         (tri (+ n 3))
         (fif (+ n 5)))
    (mapcar (lambda (x) (let ((nn (+ 48 x)))
                     (if (> nn upper)
                         (- nn 12)
                         nn)))
            (list n tri fif))))

(defun make-chord-random (&optional (upper 100))
  (let* ((n (random 12))
         (tri (+ n 3 (random 2)))
         (fif (+ n 7)))
    (sort (mapcar (lambda (x) (let ((nn (+ 48 x)))
                     (if (> nn upper)
                         (- nn 12)
                         nn)))
                  (list n tri fif))
          '<)))
 
(defun make-chord-random-pc ()
  (let* ((n (random 12))
         (tri (+ n 3 (random 2)))
         (fif (+ n 7)))
    (list n tri fif)))

(defun make-chord-from-pc (pc &optional (upper 100))
  (sort (mapcar (lambda (x) (let ((nn (+ 48 x)))
                         (if (> nn upper)
                             (- nn 12)
                             nn)))
                pc)
        '<)))

(defvar *metro* (make-metro 90))
(defvar *m1* nil)
(defvar *m2* nil)
(setf *m1* (make-metre '(2) 4))
(setf *m2* (make-metre '(2) 4))

#|
//melody generator
func generateMelody(_ chord: [Int]) -> Int {
    var melody = chord[random(3)] + 72
    if melody > 84 {
        melody -= 12
    }
    return melody
}
|#

(defun moonlight (beat time)
  (let* ((n-beat (+ beat 1)))
    (play-midi-note time 45 27 .5 1)
    (aat (funcall *metro* n-beat) #'moonlight n-beat it)))

(defun moonlight (beat time)
  (let* ((n-beat (+ beat 1))
         (chord  (make-chord-random 60)))
    (play-midi-note time 45 27 .5 1)
    (when (funcall *m1* beat 1.0)
      (if (cm:odds .6)
          (play-midi-arpeggio time chord 40 2 2)
          (play-midi-arpeggio time (ivl-transpose 6 chord) 40 2 2)))
    (aat (funcall *metro* n-beat) #'moonlight n-beat it)))

(defun moonlight (beat time)
  (let* ((n-beat (+ beat 1))
         (chord  (make-chord-random 60))
         (r-chord (nth (random 3) chord))
         (r-n-chord (+ 72 r-chord))
         (r-nn-chord (if (> r-n-chord 84)
                         (- r-n-chord 48)
                         r-n-chord)))
    (play-midi-note time 45 27 .5 1)
    (when (funcall *m1* beat 1.0)
      (play-midi-arpeggio time chord 40 2 2))
    (when (funcall *m2* beat 1.0)
      (if (cm:odds .3)
          (play-midi-note time r-chord (cm:odds .3 45 35) 3 4)
          (play-note-mord time r-nn-chord 35 3 '(0 4 7) 4)))
    (aat (funcall *metro* n-beat) #'moonlight n-beat it)))

(defun moonlight  (beat time)
  (let* ((n-beat  (+ beat 1))
         (pc      (make-chord-random-pc))
         (chord   (make-chord-from-pc pc 60))
         (r-pc    (nth (random 3) pc))
         (r-chord (nth (random 3) chord))
         (r-n-chord (+ 72 r-pc))
         (r-nn-chord (if (> r-n-chord 84)
                         (- r-n-chord 12)
                         r-n-chord)))
    
    ;; beat
;;    (play-midi-note time 45 27 .5 3)
;;    (play-midi-note time (random-elt #(60 45)) 27 .5 3)

    ;; bass + melody
    ;; bass slightly less loud
    (when (funcall *m1* beat 1.0)
      (play-midi-note time
                      (+ 36 (nth 0 pc))
                      (+ 15 (random 10))
                      20 (random 2))
      (play-midi-note time
                      r-nn-chord
                      (+ 20 (random 10))
                      20 (random 2)))
    
    ;; ;; arpeggio
    ;; - violin
    (when (funcall *m1* beat 1.0)
      (play-midi-arpeggio time chord 30 1.8 11))
    ;; - trompet
    ;; (when (funcall *m1* beat 1.0)
    ;;   ;; (play-midi-note time
    ;;   ;;                 (+ 24 (nth 0 chord))
    ;;   ;;                 40 20 (+ 20 (random 2)))
    ;;   (play-midi-arpeggio time chord 40 2 2))

    ;; trompet solo - kill beat
    ;; (when (funcall *m1* beat 1.0)
    ;;   (play-midi-arpeggio time chord 40 2 24))
    
    ;; ghost
;;    (play-midi-note time 72 30 20 (+ 20 (random 2)))
    
    ;; mord or note
    ;; (when (funcall *m2* beat 1.0)
    ;;   (if (cm:odds .6)
    ;;       (play-midi-note time r-chord (cm:odds .3 35 30) 3 4)
    ;;       (play-note-mord time r-nn-chord 30 3.5 '(0 4 7) 7)))
    ;; ;;    (play-midi-note time r-nn-chord 55 5 4)))
    
    (aat (funcall *metro* n-beat) #'moonlight n-beat it)))

(moonlight (funcall *metro* 'get-beat 4)
           (funcall *metro* (funcall *metro* 'get-beat 4)))

(fluidsynth:program-change *synth* 1 1)
(fluidsynth:program-change *synth* 4 46)
(fluidsynth:program-change *synth* 7 38)
(fluidsynth:program-change *synth* 11 21) ;; 33

(fluidsynth:program-change *synth* 21 43)
(fluidsynth:program-change *synth* 20 43)


(flush-pending)
(off-with-the-notes)
