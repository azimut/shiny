(in-package :somecepl)

;; Run it on the repl
;; (rt-start)

(ql:quickload :incudine-fluidsynth)

(defvar *fluid-settings* (fluidsynth:new-settings
                          `(("synth.polyphony" 128)
                            ("synth.sample-rate" ,*sample-rate*)
                            ("audio.sample-format" "float"))))


(defvar *synth* (fluidsynth:new *fluid-settings*))

;; (defparameter *env1* (make-envelope '(0 1 1 0) '(0 .9 .1)))

(dsp! fluid-test ((synth fluidsynth:synth))
  (with ((len (block-size))
         (left (make-f32-array len))
         (right (make-f32-array len)))
    (fluidsynth:write-float synth len left 0 1 right 0 1)
    (foreach-frame
      (out (f32-ref left current-frame)
           (f32-ref right current-frame)))))

;(fluidsynth:sfload *synth* "/home/sendai/Downloads/fluid-soundfont-3.1/FluidR3_GM.sf2" 1)
;(fluidsynth:sfload *synth* "/usr/share/sounds/sf2/FluidR3_GM.sf2" 1)
;(fluidsynth:sfload *synth* "/home/sendai/Downloads/samples/GeneralUser GS 1.471/GeneralUser GS v1.471.sf2" 1)
(fluidsynth:sfload *synth* "/home/sendai/Downloads/Sonatina_Symphonic_Orchestra.sf2" 1)
;(fluidsynth:sfload *synth* "/home/sendai/Downloads/Nice-Keys-Ultimate-V2.3.sf2" 1)

#|
(fluidsynth:get-active-voice-count *synth*)
(fluidsynth:stop *synth* 1)
(incudine:free 0)
(fluidsynth:delete *synth*)
|#

(fluidsynth:set-reverb *synth* 0.7d0 0.9d0 0.5d0 0.9d0)
(set-rt-block-size 64)

(setf (fluidsynth:setting *fluid-settings* "synth.gain") .8)
;(setf (fluidsynth:setting *fluid-settings* "synth.polyphony") 128)
;(setf (fluidsynth:setting *fluid-settings* "synth.midi-channels") 24)

(rt-stop)
(rt-start)
(fluid-test *synth*)
;;(fluidsynth:program-change *synth* 11 2)
(fluidsynth:noteon *synth* 0 60 100)

#|
(incudine:free 1)
(flush-pending)
(define play-midi-note
  (lambda (time device pitch velocity duration channel)
    (callback time 'midi_send device *midi-note-on* channel pitch velocity)
    (callback (+ time duration) 'midi_send device *midi-note-off* channel pitch velocity)))
|#

(defun play-midi-note (time pitch velocity dur c)
     (at time #'fluidsynth:noteon *synth* c pitch velocity)
     (at (+ time #[dur s]) #'fluidsynth:noteoff *synth* c pitch)
  )

(play-midi-note (now) 60 90 2 11)
(play-midi-note (now) 36 60 5 0)
(play-midi-note (now) 36 60 10 (random 6))

;; ----------------------
;; DANCING PHALANGES
(setf (bpm *tempo*) 60)
(defvar *root* nil)
(setf *root* 0)
(defvar *degree* nil)
(setf *degree* 'i)
(defvar *scale* nil)
(setf *scale* (scale 0 'phrygian))

(defun melody (time ms rs)
  (if (not ms)
      'done
      (let ((p (car ms))
            (d (car rs)))
        ;; (play-midi-note time
        ;;                 (quant (+ 48 *root* p) *scale*)
        ;;                 (round (cosr 60 40 1/2)) 
        ;;                 (* d (cosr 4. .2 1/2))
        ;;                 1)
        (play-midi-note time
                        (quant (+ 55 *root* p) *scale*)
                        (round (cosr 60 40 1/2))
                        (* d (cosr 4. .2 1/2))
                        2)
        (aat (+ time #[d b]) #'melody it (cdr ms) (cdr rs))
        )))

(defun right (degree)
  (setf *degree* degree)
  (setf *root* (car (diatonic 0 '- degree)))
  (at (+ (now) #[4 b]) #'right (random-list (cdr (assoc degree '((i n6 iii vii)
                                                                 (n6 v7)
                                                                 (iii v7)
                                                                 (vii vi)
                                                                 (vi vii v7 v7)
                                                                 (v7 i))))))
  )

(right 'i)

(defun left (time dur)
  (if (> (random 1.0) .85) (melody time '(0 1 0 -1) '(1/3 2/3 2/3 1)))
;  (play-midi-note time
;                  (+ 48 *root*)
;                  (round (cosr 80 30 1))
;                  (* dur (cosr 2.2 .3 1/7))
;                  2)
  (play-midi-note time
                  36
                  (round (cosr 40 30 1))
                  (* dur (cosr .6 .3 1/7))
                  0)
  (aat (tempo-sync #[dur b]) #'left it (random-list '(1 2/3))))

(left (now) 2/3)
(flush-pending)
(incudine:free 0)

;; -----------------------------------
;; Overtone example
;; "Piano phase" - https://en.wikipedia.org/wiki/Piano_Phase
;; ----------------------------------

(defvar *piece* nil)
(setf *piece* '(:E4 :F#4 :B4 :C#5 :D5 :F#4 :E4 :C#5 :B4 :F#4 :D5 :C#5))

(repeat 1000 *piece*)

(setf (bpm *tempo*) 35)

(defun player (time speed notes c)
  (let ((n      (first notes))
        (notes  (cdr notes))) 
    (when n
      (play-midi-note time
                      (note-name-to-midi-number (symbol-name n))
                      (round (cosr 60 10 1/2))
                      1
                      c)
      (aat (+ time speed) #'player it speed notes c))))

#|
(player (now) #[.335 b] *piece* 0)
(flush-pending)
|#

(let ((time (now)))
  (player time #[.338 b] *piece* 0)
  (player time #[.335 b] *piece* 1))

;; --------------------------------------------------------------------
;; Another late christmas
;; --------------------------------------------------------------------
(setf (bpm *tempo*) 30)
(defvar *root* nil)
(setf *root* 72)

(defun loop1 (time dur)
  (play-midi-note time
                  (if (= (mod (get-internal-real-time) 12) 0)
                      67
                      (random-list '(60 60 60 58)))
                  50
                  dur
                  0)
  (aat (tempo-sync #[(* .5 dur) b]) #'loop1 it dur))

(defun s ()
;;  '(0)
  '(0 2 3 5)
)

(defun loop2 (time dlist slist)
  (if (car slist)
      (play-midi-note time
                      (relative *root* (car slist) (s))
                      (rrandom 60 80)
                      (car dlist)
                      1))
  (aat (tempo-sync #[(car dlist) b]) #'loop2 it
       (alexandria:rotate dlist -1)
       (alexandria:rotate slist -1)))

#|
(loop1 (now) 1)
(loop2 (now) '(1 1/2 1/2 1/2 1/2) '(2 1 2 0 nil))
(setf *root* 72)
(setf *root* 63)
(flush-pending)
|#

;; Ah!...tempo-sync...I mean beat...without it I cannot sync two things...dah!

;; --------------------------------------------------------------------
;; Extempore - An Overview
;; https://vimeo.com/21956071
;; at 11:30
;; --------------------------------------------------------------------
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
;    (aat (+ time #[3 b]) #'play-midi-note it 36 90 (* 3.0 dur) 1)
    (mapcar (lambda (x y)
            (aat (+ time #[y b]) #'play-midi-note it x 100 (* 2.0 dur) 0))
            (make-chord 40
                        (cosr 75 10 1/32)
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

;; ----------------------------
;; Euclidean Rythms
;; https://github.com/overtone/overtone/blob/master/src/overtone/examples/compositions/euclidean_rhythms.clj
;; ----------------------------

(setf (bpm *tempo*) 60)

(defun eu (time vel chan beat rythm notes)
  ;; If rythm is 1 we play the note
  (if (= 1 (first rythm))
      (let ((note (first notes)))
        (play-midi-note time (cm:keynum note) vel 1 chan)
        (setf notes (alexandria:rotate notes 1))))
  (aat (tempo-sync #[beat b]) #'eu
       it
       vel
       chan
       beat
       (alexandria:rotate rythm 1)
       notes)
)

(flush-pending)

(eu (now) 60 0 1/2 (bjorklund 5 13) '(:c3 :g3 :d3))
(eu (now) 50 1 1   (bjorklund 4 4)  '(:c3 :g3 :d3))
(eu (now) 50 2 1/2 (bjorklund 3 8)  '(:c3 :g3 :d3))

;; -----------
;; Playground
;; https://digego.github.io/extempore/note-level-music.html
;; http://impromptu.moso.com.au/tutorials/making_music/
;; -----------
(setf (fluidsynth:setting *fluid-settings* "synth.gain") .4)

;; arpeggio
(mapcar (lambda (note delay vel c)
            (play-midi-note (+ (now) #[delay b]) note vel 2 c)
            )
          '(60 64 67 70)
          '(1 2 3 4)
          '(90 50 50 60)
          '(0 1 2 3)
          )
;; chord
(let ((time (now)))
  (mapcar (lambda (note c)
            (play-midi-note time note 80 5 c)
            )
          '(60 64 67)
          '(0 1 2)
          ))

;; 1/f
(defun 1f (repeats)
    (mapcar (lambda (note delay vel c)
            (play-midi-note (+ (now) #[delay b]) note vel 2 c)
            )
          (mapcar (lambda (x) (+ 60 x)) (1-over-f repeats))
          (range (round (/ repeats 2)) :min 1 :step .5)
          (repeat repeats '(60))
          (repeat repeats '(0))
          ))

(1f 30)

;; --------------------------------------------------------------------
;; GOTO 2014 • Programming In Time - Live Coding for Creative Performances
;; https://www.youtube.com/watch?v=Sg2BjFQnr9s
;; --------------------------------------------------------------------
#|
(defun seq-test (&optional (root 60))
  (let* ((newroot (random-list (cdr (assoc root '((60 58 55)
                                                  (58 60 56)
                                                  (56 55 58)
                                                  (55 60 56)))))))
    (dsp-seq
     (play-lsample (+ root -12)  .35 .3)
     (play-lsample (+ -5  root)  .35 .3)
           ;(play-lsample  (- root 12) 1 .2 :id 2)
           ;(play-lsample  (+ 12 root) 1  .2 :id 3)
           (seq-test newroot))))
|#

(setf (bpm *tempo*) 60)
(defvar *root* nil)
(setf *root* 60)
(defvar *myscale* nil)
(setf *myscale* (scale 0 'aeolian))

(defun left (time)
  (setf *root* (random-list (cdr (assoc *root* '((60 58 55)
                                                 (58 60 56)
                                                 (56 55 58)
                                                 (55 60 56))))))
     (play-midi-note time (- *root* 12) 60 3 0)
     (aat (tempo-sync #[3/2 b]) #'play-midi-note it (+ -5  *root*) 60 1 1)
     (aat (tempo-sync #[4 b])   #'left it))

(defun right ()
  (play-lsa (round (qcosr *myscale* *root* 7 3/2)) 1.4 .1)
  (at (tempo-sync #[.5 b]) #'right)
)
(left (now))
(right)
(flush-pending)

;; --------------------------------------------------------------------
;; The rules for the algorithm are as follows :
;;
;;     two different note lengths need to be defined, e.g. "4" and "3"
;;     a scale needs to be defined, e.g. C major (the white keys on a piano), let's say we start on the E note, the list of notes will then contain : E, F, G, A, B, C
;;     a pattern length needs to be defined, e.g. 4 bars
;;
;; The algorithm will then function like so (keeping the above definitions in mind) :
;;
;;     the first note of the scale (E) is played at the length of the first defined note length (4)
;;     each time the duration of the played note has ended, the NEXT note in the scale (F) is played
;;     once the first pattern length has been reached (4 bars), a new pattern will start
;;     the previously "recorded" pattern will loop its contents indefinitely while the new patterns are created / played
;;     if a newly played note sounds simultaneously with another note from a PREVIOUS pattern, the note length will change (in above example from 4 to 3).
;;     this will be the new note length to use for ALL SUBSEQUENT added notes, until another simultaneously played note is found, leading it to switch back to the previous note length (in above example, back to 4).
;;     as the pattern is now played over an existing one, it is likely that notes will be played in unison, leading to the switching of note length
;;     as more patterns are accumulated, a perfectly mathematical pattern of notes are weaving in and out of the notes of the other patterns
;;
;; https://github.com/igorski/molecular-music-generator
;; --------------------------------------------------------------------



;; (defun mpattern (time lpattern notes length1 length2 &key accumnotes accumtempos (counter 0) (swap "N"))
;;   (if (not (null notes))
;;       (let* ((counter (1+ counter))
;;              (next    (tempo-sync #[length1 b]))
;;              (neext   (tempo-sync #[length2 b]))
;;              (note    (first notes))
;;              (realnext nil)
;;              (accumnotes  (append  accumnotes (list note))))
;;         (play-midi-note time note 50 1 (random 9))

;; ;;        (print swap)
;;         (if (or (equal swap "Y") (= counter lpattern)) (setf realnext neext) (setf realnext next))
;;         (if (numberp (position realnext *mtempos*)) (setf swap "Y"))
;; ;;        (print swap)

;;         (cond ((= counter lpattern)
;;                (progn
;;                  (aat next  #'spattern it accumnotes (append accumtempos (list length1)) (random 11))
;;                  (aat neext #'mpattern it length2 (cdr notes) length2 length1 :counter 0)))
;;               ((equal swap "Y")
;;                (progn
;;                  (print "swapping it up")
;;                  (aat neext #'mpattern
;;                       it lpattern (cdr notes) length2 length1
;;                       :accumnotes  accumnotes
;;                       :accumtempos (append accumtempos (list length1))
;;                       :counter counter)))
;;               (t 
;;                (aat next #'mpattern
;;                     it lpattern (cdr notes) length1 length2
;;                     :accumnotes  accumnotes
;;                     :accumtempos (append accumtempos (list length1))
;;                     :counter counter
;;                     :swap swap))))))

;; (defun mpattern (time lpattern notes length1 length2 &key accumnotes accumtempos (counter 0) (swap "N"))
;;   (if (not (null notes))
;;       (let* (
;;              (counter (+ counter length1))
;;              (next    (tempo-sync #[length1 b]))
;;              (neext   (tempo-sync #[length2 b]))
;;              (note    (first notes))
;;              (realnext nil)
;;              (accumnotes  (append  accumnotes (list note))))
;;         (play-midi-note time note 50 1 11)

;;         (print swap)
;;         ;;(if (or (equal swap "Y") (>= counter lpattern)) (setf realnext neext) (setf realnext next))
;;         (if (numberp (position next *mtempos*)) (setf swap "Y"))
;;         (print swap)

;;         (cond ((>= counter lpattern)
;;                (progn
;;                  (aat next #'spattern it accumnotes (append accumtempos (list length1)) (random 11))
;;                  (aat next #'mpattern it lpattern (cdr notes) length1 length2)))
;;               ((equal swap "Y")
;;                (progn
;;                  (print "swapping it up")
;;                  (aat neext #'mpattern
;;                       it lpattern (cdr notes) length2 length1
;;                       :accumnotes  accumnotes
;;                       :accumtempos (append accumtempos (list length1))
;;                       :counter counter )))
;;               (t 
;;                (aat next #'mpattern
;;                     it lpattern (cdr notes) length1 length2
;;                     :accumnotes  accumnotes
;;                     :accumtempos (append accumtempos (list length1))
;;                     :counter counter
;;                     :swap swap))))))


;; (defun spattern (time notes tempos pattern r) 
;;   (let* ((note1 (cm:next notes))
;;          (note2 (cm:next notes))
;;          (pat1 (cm:next pattern))
;;          (pat2 (cm:next pattern))
;;          (next (tempo-sync #[1 b]))
;;          (neext (tempo-sync #[2 b])))
;;     ;;(setf *mtempos* (append (list next) *mtempos*))
;;     (if (= pat1 1)
;;         (play-midi-note time note1 60 1 (random 11)))
;;     (if (= pat2 1)
;;         (play-midi-note next note2 60 1 (random 11)))
;;     (aat neext #'spattern
;;          it
;;          notes
;;          (alexandria:rotate tempos -1)
;;          pattern
;;          r)))

;; (defun spattern (time notes tempos pattern r)
;;   (let* ((note1 (first notes))
;;          (pat1  (first pattern))
;;          (pat2  (second pattern))
;;          (next  (tempo-sync #[1 b])))
;;     (if (= pat1 1)
;;         (progn
;;           (print "now1")
;;           (play-midi-note (now) note1 60 1 (random 11))))
;;     (if (= pat2 1)
;;         (progn
;;           (print "now2")
;;           (setf *mtempos* (append (list (round next)) *mtempos*))))
;;     (aat next #'spattern
;;          it
;;          (rotate-list notes -1)
;;          (rotate-list tempos -1)
;;          (rotate-list pattern -1)
;;          r)))
  
;; (defun mpattern (time lpattern notes length1 length2 &key accumnotes accumtempos accumbeats (counter 0) (beat 0) (swap "N"))
;;   (if (not (null notes))
;;       (let* (
;;              (counter (+ counter length1))
;;              (next    (tempo-sync #[length1 b]))
;;              (neext   (tempo-sync #[length2 b]))
;;              (realnext nil)
;;              (note    (first notes))
;;              (accumnotes (append accumnotes (list note)))
;;              (accumbeats (append accumbeats '(1) (repeat (- length1 1) '(0)))))
        
;;         (play-midi-note time note 60 .5 (random 11))
;;         (print swap)
;;         (print length1)

       
;;         ;; ;; Predict if new pattern to be created is going to play next beat
;;         (if  accumbeats)) (equal swap "Y") (>= (+ length2 counter) lpattern))
;;               ((and (= 1 (first accumbeats)) (equal swap "N") (>= (+ length1 counter) lpattern))
;;                  (setf *mtempos* (append *mtempos* (list next))))))
        
;;         (if (numberp (position (round next) *mtempos*))
;;             (setf swap "Y"))
        
;;         (cond ((>= counter lpattern)
;;                (progn
;;                  (print "end patern")
;;                  (print accumtempos)
;;                  (print accumbeats)
;;                  (aat time #'spattern
;;                       it
;;                       accumnotes
;;                       (append accumtempos (list length1))
;;                       accumbeats
;;                       (random 11))
;;                  ;; (setf accumbeats nil)
;;                  ;; Star a new pattern
;;                  (if (equal swap "Y")
;;                      (aat next #'mpattern it lpattern (cdr notes) length2 length1)
;;                      (aat next #'mpattern it lpattern (cdr notes) length1 length2))))
;;              ((equal swap "Y")
;;                (progn
;;                  (print "swapping it up")
;;                  (aat neext #'mpattern
;;                       it lpattern (cdr notes) length2 length1
;;                       :accumnotes  accumnotes
;;                       :accumtempos (append accumtempos (list length1))
;;                       :accumbeats accumbeats
;;                       :counter counter
;;                       :beat (+ beat length1))))
;;               (t
;;                (progn
;;                  (print "what")
;;                  (aat next #'mpattern
;;                       it lpattern (cdr notes) length1 length2
;;                       :accumnotes  accumnotes
;;                       :accumtempos (append accumtempos (list length1))
;;                       :accumbeats accumbeats
;;                       :counter counter
;;                       :swap swap
;;                       :beat (+ beat length1))))))))

(defvar *mtempos* nil)
(setf *mtempos* nil)
(setf (bpm *tempo*) 80)

;; (defun spattern (time notes pattern r)  
;;   (let (
;;          (pat1  (nth 1 pattern))
;;          (pat2  (nth 2 pattern))
;;          (pat3  (nth 3 pattern))
;;          (pat4  (nth 4 pattern))
;;          (pat5  (nth 5 pattern))
;;          (next1  (tempo-sync #[1 b]))
;;          (next2  (tempo-sync #[2 b]))
;;          (next3  (tempo-sync #[3 b]))
;;          (next4  (tempo-sync #[4 b])))
;;     (if (= pat1 1)
;;           (play-midi-note time (cm:next notes) 60 1 (random 11)))
;;     (if (= pat2 1)
;;         (progn
;;           (play-midi-note next1 (cm:next notes) 60 1 (random 11))
;;           (setf *mtempos* (append (list next1) *mtempos*))))
;;     (if (= pat3 1)
;;         (progn
;;           (play-midi-note next2 (cm:next notes) 60 1 (random 11))
;;           (setf *mtempos* (append (list next2) *mtempos*))))
;;     (if (= pat4 1)
;;         (progn
;;           (play-midi-note next3 (cm:next notes) 60 1 (random 11))
;;           (setf *mtempos* (append (list next3) *mtempos*))))
;;     (if (= pat5 1)
;;         (setf *mtempos* (append (list next4) *mtempos*)))
;;     (aat next4 #'spattern
;;          it
;;          notes
;;          (rotate-list pattern -4)
;;          r)))

(defun spattern (time notes pattern r)
  (let* ((lpattern   (length pattern))
         (t-lpattern (tempo-sync #[(/ lpattern 2) b]))
         (pbeat      (remove nil
                            (loop :for beat :in pattern
                               :for nbeat :from 0 :upto 64
                               :collect (if (= 1 beat) nbeat)))))
    (loop :for cbeat :in pbeat :do
        (if (= cbeat 0)
            (play-midi-note time (cm:next notes) 60 1 (random 11))
            (let ((t-beat (tempo-sync #[(/ cbeat 2) b])))
              (play-midi-note t-beat (cm:next notes) 60 1 (random 11))
              (setf *mtempos* (append *mtempos* (list t-beat))))))
    (if (= 1 (first pattern))
        (setf *mtempos* (append *mtempos* (list t-lpattern))))
    (aat t-lpattern #'spattern it notes pattern r)))


(defun spattern (time notes pattern r)
  (let* ((lpattern   (length pattern))
         (t-lpattern (tempo-sync #[(/ lpattern 2) b]))
         (pbeat      (remove nil
                            (loop :for beat :in pattern
                               :for nbeat :from 0 :upto 64
                               :collect (if (= 1 beat) nbeat)))))
    (print pattern)
    (loop :for cbeat :in pbeat :do
        (if (not (= cbeat 0))
;;            (play-midi-note time (cm:next notes) 60 1 (random 11))
            (let ((t-beat (tempo-sync #[(/ cbeat 2) b])))
;;              (play-midi-note t-beat (cm:next notes) 60 1 (random 11))
              (setf *mtempos* (append *mtempos* (list t-beat))))))
    (if (= 1 (first pattern))
        (setf *mtempos* (append *mtempos* (list t-lpattern))))
    (aat t-lpattern #'spattern it notes pattern r)))

(defun spattern (time notes pattern r)
  (let* ((lpattern   (length pattern))
         (t-lpattern (tempo-sync #[(/ lpattern 2) b]))
         (pbeat      (remove nil
                            (loop :for beat :in pattern
                               :for nbeat :from 0 :upto 64
                               :collect (if (= 1 beat) nbeat)))))
    (loop :for cbeat :in pbeat :do
        (if (= cbeat 0)
            (play-midi-note time (cm:next notes) 30 .7 (random 11))
            (let ((t-beat (tempo-sync #[(/ cbeat 2) b])))
              (play-midi-note t-beat (cm:next notes) 30 .7 (random 11))
              (setf *mtempos* (append *mtempos* (list cbeat))))))
    (if (= 1 (first pattern))
        (setf *mtempos* (append *mtempos* (list 0))))
    (aat t-lpattern #'spattern it notes pattern r)))



;; ;; I need to use (mod) to get the next beat where there is a note
;; (defun ppattern (time lpattern notes length1 length2 &key
;;                                                        (cbeat 0)
;;                                                        (ibeat 0)
;;                                                        accumbeats accumnotes play pbeat)
;;   (if (not (null notes))
;;       (let ((nbeat   (+ .5 cbeat))
;;             (nibeat  (+  1 ibeat))
;;             (t-nbeat (tempo-sync #[.5 b]))
;;             (note    (first notes))
;;             (swap    nil))
;;         ;; play now
;;         (cond ((or (equal play "yes")
;;                    (= pbeat ibeat))
;;                (progn
;;                  (play-midi-note time note 60 1 (random 11))
;;                  (setf notes      (cdr notes))
;;                  (setf accumnotes (append accumnotes (list note)))
;;                  (setf accumbeats (append accumbeats '(1)))
;;                  (setf pbeat      (mod (+ ibeat (* length1 2)) lpattern))
;;                  (if (numberp (position (tempo-sync #[length1 b]) *mtempos*))
;;                      (setf swap "y"))))
;;               (t
;;                (setf accumbeats (append accumbeats '(0)))))
;;         ;; reset when the next .5 beat is the last beat of the pattern
;;         ;; if not is business as usual and we stick with this pattern
;;         (if (= lpattern nibeat)
;;             (progn
;;               (print "endpattern")
;;               (print length1)
;;               (print accumbeats)
;;               (aat t-nbeat #'spattern it
;;                    (cm:new cm:cycle :of accumnotes) accumbeats 1)
;;               ;; This works to match agains the prev NOT the global
;;               (if (= 1 (first accumbeats))
;;                   (progn
;;                     (print "swap")
;; ;;                    (setf *mtempos* (append *mtempos* (list t-nbeat)))
;;                     (aat t-nbeat #'ppattern it lpattern notes length2 length1 :pbeat pbeat))
;;                   (aat t-nbeat #'ppattern it lpattern notes length1 length2 :pbeat pbeat)))
;;             (if (equal swap "y")
;;                 (progn
;;                   (aat t-nbeat #'ppattern it lpattern notes length2 length1
;;                        :accumnotes accumnotes
;;                        :accumbeats accumbeats
;;                        :cbeat nbeat
;;                        :ibeat nibeat
;;                        :pbeat pbeat)
;;                   (print "other swap"))
;;                 (aat t-nbeat #'ppattern it lpattern notes length1 length2
;;                      :accumnotes accumnotes
;;                      :accumbeats accumbeats
;;                      :cbeat nbeat
;;                      :ibeat nibeat
;;                      :pbeat pbeat))))))


;; I need to use (mod) to get the next beat where there is a note
(defun ppattern (time lpattern notes length1 length2 &key
                                                       (cbeat 0)
                                                       (ibeat 0)
                                                       accumbeats accumnotes play pbeat)
  (if (not (null notes))
      (let ((nbeat   (+ .5 cbeat))
            (nibeat  (+  1 ibeat))
            (t-nbeat (tempo-sync #[.5 b]))
            (note    (first notes))
            (swap    nil)) 
        ;; play now
        (cond ((or (equal play "yes")
                   (= pbeat ibeat))
               (progn
                 (play-midi-note time note 50 1 (random 11))
                 (setf notes      (cdr notes))
                 (setf accumnotes (append accumnotes (list note)))
                 (setf accumbeats (append accumbeats '(1)))
                 (setf pbeat      (mod (+ ibeat (* length1 2)) lpattern))))
              (t
               (setf accumbeats (append accumbeats '(0)))))
        ;; reset when the next .5 beat is the last beat of the pattern
        ;; if not is business as usual and we stick with this pattern
        (if (= lpattern nibeat)
            (progn
              (print "endpattern")
              (print length1)
              (print accumbeats)
              (aat t-nbeat #'spattern it
                   (cm:new cm:cycle :of accumnotes) accumbeats 1)
              ;; This works to match agains the prev NOT the global
              (if (and (= 1 (first accumbeats)) (= pbeat 0))
                  (progn
                    (print "endswap")
                    (print length1)
;;                    (setf *mtempos* (append *mtempos* (list t-nbeat)))
                    (aat t-nbeat #'ppattern it lpattern notes length2 length1 :pbeat pbeat))
                  (aat t-nbeat #'ppattern it lpattern notes length1 length2 :pbeat pbeat)))
            (if (and (= pbeat (mod nibeat lpattern))
                     (numberp (position pbeat *mtempos*)))
                (progn
                  (print "middle swap")
                  (print pbeat)
                  (print ibeat)
                  (print length1)
                  (aat t-nbeat #'ppattern it lpattern notes length2 length1
                       :accumnotes accumnotes
                       :accumbeats accumbeats
                       :cbeat nbeat
                       :ibeat nibeat
                       :pbeat pbeat))
                (progn
                  ;; (print "normal")
                  ;; (print pbeat)
                  ;; (print ibeat)
                  (aat t-nbeat #'ppattern it lpattern notes length1 length2
                       :accumnotes accumnotes
                       :accumbeats accumbeats
                       :cbeat nbeat
                       :ibeat nibeat
                       :pbeat pbeat)))))))

(defun mbox (time lpattern note length1 length2 pc)  
  (setf *mtempos* nil)
  (let* ((midinote (cm:keynum note))
         (notes    (loop for x from -14 to 21 collect (relative midinote x pc))))
    (ppattern time lpattern notes length1 length2 :play "yes")))

(mbox (tempo-sync #[1 b]) 32 :E 4 3 (scale 0 'dorian))
(mbox (now) 16 :E 4 3 (scale 0 'dorian))

(mbox (tempo-sync #[1 b]) 32 :C 9 14.5 (scale 0 'dorian))
(mbox (tempo-sync #[1 b]) 32 :G 10 3.5 (scale 0 'dorian))


(mbox (now) 32 :C 9 14.5 (scale 0 'dorian))
(mbox (now) 32 :G 10 3.5 (scale 0 'dorian))

(flush-pending)
(mapcar (lambda (x) (fluidsynth:noteoff *synth* 11 x)) (range 100))
