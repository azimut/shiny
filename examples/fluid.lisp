(in-package :somecepl)

;; Run it on the repl
;; (rt-start)

(ql:quickload :incudine-fluidsynth)

(defun off-with-the-notes (s)
  (loop :for c :from 0 :upto 25 :do
     (mapcar (lambda (x) (fluidsynth:noteoff s c x)) (range 120))))

(defun all-piano (s)
  (dotimes (i 32) (fluidsynth:program-change s i 1) ))
  
(setf (fluidsynth:setting *fluid-settings* "synth.gain") .5)

;; (define-constant CHORUS-DEFAULT-N      3)  
;; (define-constant CHORUS-DEFAULT-LEVEL  2.0d0)
;; (define-constant CHORUS-DEFAULT-SPEED  0.3d0)
;; (define-constant CHORUS-DEFAULT-DEPTH  8.0d0)
;; (define-constant CHORUS-DEFAULT-TYPE   CHORUS-MOD-SINE)


(defvar *fluid-settings* (fluidsynth:new-settings
                          `(("synth.polyphony" 128)
                            ("synth.midi-channels" 32)
                            ("synth.sample-rate" ,*sample-rate*)
                            ("audio.sample-format" "float"))))


(defvar *synth* (fluidsynth:new *fluid-settings*))

(defun play-midi-note (time pitch velocity dur c)
     (at time #'fluidsynth:noteon *synth* c pitch velocity)
     (at (+ time #[dur b]) #'fluidsynth:noteoff *synth* c pitch))

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
;(fluidsynth:sfload *synth* "/home/sendai/Downloads/Sonatina_Symphonic_Orchestra.sf2" 1)
(fluidsynth:sfload *synth* "/home/sendai/Nice-Keys-Ultimate-V2.3.sf2" 1)
;(fluidsynth:sfload *synth* "/home/sendai/Downloads/samples/grand-piano-YDP-20160804/grand-piano-YDP-20160804.sf2" 1)
;(fluidsynth:sfload *synth* "/home/sendai/Downloads/samples/KawaiUprightPiano-20180102/KawaiUprightPiano-20180102.sf2" 1)

#|
(fluidsynth:get-active-voice-count *synth*)
(fluidsynth:stop *synth* 1)
(incudine:free 0)
(fluidsynth:delete *synth*)
|#

(fluidsynth:set-reverb *synth* 0.7d0 0.9d0 0.5d0 0.9d0)
(set-rt-block-size 64)

(setf (fluidsynth:setting *fluid-settings* "synth.gain") .7)
;(setf (fluidsynth:setting *fluid-settings* "synth.polyphony") 128)
;(setf (fluidsynth:setting *fluid-settings* "synth.midi-channels") 24)

(rt-stop)
(rt-start)
(fluid-test *synth*)
;;(fluidsynth:program-change *synth* 11 2)
(fluidsynth:program-change *synth* 23 0)
(fluidsynth:program-change *synth* 22 0)
(fluidsynth:program-change *synth* 21 1)
(fluidsynth:program-change *synth* 20 0)
(fluidsynth:program-change *synth* 19 0)
(fluidsynth:program-change *synth* 18 0)
(fluidsynth:program-change *synth* 17 0)
(fluidsynth:program-change *synth* 15 0)
(fluidsynth:program-change *synth* 14 0)
(fluidsynth:program-change *synth* 13 0)
(fluidsynth:program-change *synth* 12 0)
(fluidsynth:program-change *synth* 10 0)
(fluidsynth:program-change *synth* 9 3)
(fluidsynth:program-change *synth* 9 3)
(fluidsynth:program-change *synth* 9 3)
(fluidsynth:noteon *synth* 0 60 100)

#|
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

(play-midi-note (now) 60 90 2 15)
(play-midi-note (now) 36 60 1 1)
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
  (let ((note   (first notes))
        (notes  (cdr notes))) 
    (when note
      (play-midi-note time
                      (note-name-to-midi-number (symbol-name note))
                      (round (cosr 60 10 1/2))
                      1
                      c)
      (aat (+ time speed) #'player it speed notes c))))

#|
(player (now) #[.335 b] *piece* 0)
(flush-pending)
|#

(let ((time (now)))
  (player time #[.338 b] *piece* 3)
  (player time #[.335 b] *piece* 3))

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
  (setf (fluidsynth:setting *fluid-settings* "synth.gain") .2)
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

(eu (tempo-sync #[1 b]) 60 0 1/2 (bjorklund 5 13) '(:c3 :g3 :d3))
(eu (tempo-sync #[1 b]) 50 1 1   (bjorklund 4 4)  '(:c3 :g3 :d3))
(eu (tempo-sync #[1 b]) 50 2 1/2 (bjorklund 3 8)  '(:c3 :g3 :d3))

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
  (play-midi-note (round (qcosr *myscale* *root* 7 3/2)) 1.4 .1)
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

(defvar *mtempos* nil)
(setf *mtempos* nil)
(setf (bpm *tempo*) 50)

;; Specially needed the first "off" on ptterns
;; might be a reason to drop midi :S
(defun play-midi-note (time pitch velocity dur c)
     (fluidsynth:noteoff *synth* c pitch)
     (at time #'fluidsynth:noteon *synth* c pitch velocity)
     (at (+ time #[dur b]) #'fluidsynth:noteoff *synth* c pitch))

;; All piano
(dotimes (i 32) (fluidsynth:program-change *synth* i 1) )

(fluidsynth:program-change *synth* 4 34)
(fluidsynth:program-change *synth* 2 1)
(fluidsynth:program-change *synth* 3 42)

(fluidsynth:program-change *synth* 2 0)
(fluidsynth:program-change *synth* 2 0)
(fluidsynth:program-change *synth* 2 0)

(fluidsynth:program-change *synth* 3 0)
(fluidsynth:program-change *synth* 8 4)
(fluidsynth:program-change *synth* 7 1)
(fluidsynth:program-change *synth* 6 1)
(dotimes (i 32) (fluidsynth:program-change *synth* i ) )

(fluidsynth:set-reverb *synth* 0.2d0 0.0d0 0.5d0 0.9d0)
(fluidsynth:set-reverb *synth* 0.4d0 0.2d0 0.5d0 0.8d0)
(fluidsynth:set-reverb *synth* 0.6d0 0.4d0 0.5d0 0.7d0)
(fluidsynth:set-reverb *synth* 0.8d0 0.7d0 0.5d0 0.6d0)
(fluidsynth:set-reverb *synth* 0.8d0 1.0d0 0.5d0 0.5d0)

(fluidsynth:set-chorus *synth* 3 10.0d0 0.3d0 8.0d0 0)
(fluidsynth:set-chorus *synth* 3 4.1d0 0.3d0 1.0d0 1)

(setf (fluidsynth:setting *fluid-settings* "synth.gain") 1.4)

(defun spattern (time notes pattern lengths r)
  (print r)
  (print pattern)
  (let* ((lpattern   (length pattern))
         (t-lpattern (tempo-sync #[(/ lpattern 2) b]))
         (pbeat      (remove nil
                            (loop :for beat :in pattern
                               :for nbeat :from 0 :upto 64
                               :collect (if (= 1 beat) nbeat)))))
    (print lpattern)
    (print pbeat)
    (loop :for cbeat :in pbeat :do
       (let ((note   (cm:next notes))
             (length (cm:next lengths)))
;;         (print note)
;;         (print length)
         (if (= cbeat 0)
             (play-midi-note time note 25 length r)
             (let ((t-beat (+ time #[(/ cbeat 2) b])))
               (play-midi-note t-beat note 25 length r)
               (setf *mtempos* (append *mtempos* (list cbeat)))))))
    (if (= 1 (first pattern))
        (setf *mtempos* (append *mtempos* (list 0))))
    (aat t-lpattern #'spattern it notes pattern lengths r)))

;; I need to use (mod) to get the next beat where there is a note
(defun ppattern (time lpattern notes length1 length2 &key
                                                       (cbeat 0)
                                                       (ibeat 0)
                                                       (chan 2)
                                                       (pchan 0)
                                                       accumbeats accumnotes accumlengths play pbeat)
  (if (not (null notes))
      (let ((nbeat   (+ .5 cbeat))
            (nibeat  (+  1 ibeat))
            (nchan   (if (= 9 (+ 1 chan)) (+ 2 chan) (+ 1 chan)))
            (npchan  (mod (+ pchan 1) 2))
            ;;(t-nbeat (tempo-sync #[.5 b]))
            (t-nbeat (+ time #[.5 b]))
            (note    (first notes))) 
        ;; play now
        (cond ((or (equal play "yes")
                   (= pbeat ibeat))
               (progn
                 (play-midi-note time note 35 length1 pchan)
                 (setf notes        (cdr notes))
                 (setf accumnotes   (append accumnotes (list note)))
                 (setf accumbeats   (append accumbeats '(1)))
                 (setf accumlengths (append accumlengths (list length1)))
                 (setf pbeat        (mod (+ ibeat (* length1 2)) lpattern))))
              (t
               (setf accumbeats (append accumbeats '(0)))))
        ;; reset when the next .5 beat is the last beat of the pattern
        ;; if not is business as usual and we stick with this pattern
        (if (= lpattern nibeat)
            (progn
              (print "endpattern")
;;              (print accumbeats)
;;              (print length1)
              (aat t-nbeat #'spattern it
                   (cm:new cm:cycle :of accumnotes)
                   accumbeats
                   (cm:new cm:cycle :of accumlengths)
                   chan)
              ;; This works to match agains the prev NOT the global
              (if (and (= 1 (first accumbeats)) (= pbeat 0))
                  (progn
                    (print "endswap")
;;                    (setf *mtempos* (append *mtempos* (list '(0))))
                    (aat t-nbeat #'ppattern it lpattern notes length2 length1 :pbeat pbeat :chan nchan :pchan npchan ))
                  (aat t-nbeat #'ppattern it lpattern notes length1 length2 :pbeat pbeat :chan nchan :pchan npchan )))
            (if (and (= pbeat (mod nibeat lpattern))
                     (numberp (position pbeat *mtempos*)))
                (progn
                  (print "middle swap")
                  (aat t-nbeat #'ppattern it lpattern notes length2 length1
                       :accumnotes accumnotes
                       :accumbeats accumbeats
                       :accumlengths accumlengths
                       :cbeat nbeat
                       :chan chan
                       :ibeat nibeat
                       :pbeat pbeat))
                (progn
                  (aat t-nbeat #'ppattern it lpattern notes length1 length2
                       :accumnotes accumnotes
                       :accumbeats accumbeats
                       :accumlengths accumlengths
                       :cbeat nbeat
                       :chan chan
                       :ibeat nibeat
                       :pbeat pbeat)))))))

(defun mbox (time lpattern note length1 length2 pc)  
  (setf *mtempos* nil)
  (let* ((midinote (cm:keynum note))
         (notes    (loop for x :from -14 :to 24 :collect (relative midinote x pc))))
    (print notes)
    (ppattern time lpattern notes length1 length2 :play "yes")))

#|
(mbox (tempo-sync #[1 b]) 32 :E 4 3 (scale 1 'dorian))
(mbox (tempo-sync #[1 b]) 32 :C 9 14.5 (scale 0 'dorian))
(mbox (tempo-sync #[1 b]) 32 :G 10 3.5 (scale 0 'dorian))


(mbox (tempo-sync #[1 b]) 32 :C 8 14.5 (scale 0 'dorian))
(mbox (tempo-sync #[1 b]) 32 :D 9 1.5 (scale 0 'dorian))

(flush-pending)
(off-with-the-notes *synth*)
|#


;; --------------------------------------------------------------------
;; Gloriette for John Cage - Translated from notes from metalevel
;; TODO: stop using a global and try to use a local variable instead
;; --------------------------------------------------------------------
(defvar *c0* 0)
(setf *c0* 0)


(defun bird (time offset vel chan p q i &optional (w 0))
  (if (= chan 0) (setf vel 20))
;  (if (not (= i 200))
      (let* ((i    (1+ i))
             (note (cm:next p))
             (dur  (cm:next q))
             (mul  12))
        (setf *c0* (cm:interp i 0 .5 90 4))
        ;;(setf *c0* 0)
        ;;(setf i 0)
;;        (print note)
;;        (print i)
        (if (not (equal note 'r))
            (play-midi-note time (cm:keynum (cm:transpose note offset) ) vel (* dur (- mul 3) ) chan))
        (aat (tempo-sync #[(* mul dur) b]) #'bird it offset vel chan p q i w)))

(defun cage (offset vel chan)
  (let* ((d (cm:pval *c0*))
         (p (cm:new cm:weighting :of `((g3 :weight ,d)
                                       (a3 :weight ,d)
                                       bf3
                                       (c4 :weight ,d)
                                       d4
                                       (e4 :weight ,d)
                                       f4
                                       (g4 :weight ,d)
                                       (r :max 1 :weight .25))
                    :for 1))
         (q (cm:new cm:weighting :of (list 1/16
                                           1/8
                                           (cm:new cm:cycle :of 1/32 :for 2)))))
    (aat (tempo-sync #[1 b]) #'bird it offset vel chan p q 0)))

;; 10 violin
;; 11 pizzicato (?)
;; 15 wind

(fluidsynth:program-change *synth* 0 32)
(fluidsynth:program-change *synth* 1 10)
(fluidsynth:program-change *synth* 2 10)

(fluidsynth:program-change *synth* 0 22)
(fluidsynth:program-change *synth* 1 77)
(fluidsynth:program-change *synth* 2 33)

(cage -12 30 3)
(cage 0 50 1)
(cage 12 40 2)

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

;; --------------------------------------------------------------------
;; Genetic music
;; https://www.reddit.com/r/algorithmicmusic/comments/3xlrvk/some_music_composed_by_a_program_i_wrote_genetic/
;; https://soundcloud.com/music-organism
;;
;; Basically, the program is based on a genetic algorithm. There are different "organisms/species" that are in charge of creating pieces of a song. For instance, one "species" generates a synthesizer definition. This species has 5 chromosomes: 1 for overall synth configuration (vibrato, envelope, etc.), and 4 that control signal generators (square wave, sine, tri, etc.) along with definitions for optional filters for each signal generator.
;;
;; There are similar "species" for overall song structure (bpm, meter, number of instruments/synths), pattern structure (allowed note range, on/off beat notes, etc.), and one for the actual melody (produces a list of notes, with durations and rests).
;;
;; The basic idea is that when a song is generated, it will choose 2 "organisms" from from a set from each type of "species". It mates these "organisms" together using common genetic algorithm techniques (crossover, mutation, etc). It will then use the offspring organism to create the different parts of the song. The whole thing gets exported to a format that supercollider can play, and I listen to it to see if it's any good (and the best ones get posted on the soundcloud account). The "organisms" that were used to make a "good" song get saved back to the set of possible organisms to choose from for the next song.
;;
;; Or that's how I want it to eventually work. Right now, it's already working like this for the synthesizer species, but the other species are basically just generating random organisms as a starting generation and then running a simple generation-based learning loop to try to produce "interesting" organisms. But soon, I'll make those species work the same as the synths.
;;
;; I don't have a website yet. Eventually the plan is to basically have it streaming music 24/7 where people would be able to listen and say "yeah this
;; is cool" or "no this is garbage", which would then feed back into the algorithm and influence the next song.
;; --------------------------------------------------------------------

;; --------------------------------------------------------------------
;; https://www.youtube.com/watch?v=J_4zr0Qk6o0
;; Major and minor eleventh chords in 5 limit just intonation modulate in a variety of ways. It doesn't stick to a fixed set of pitches, so it can modulate arbitrarily without intonation problems.
;;
;; As usual it's all PD with no samples or VSTs or anything.
;;
;; Basically I'm constructing major and minor eleventh chords from two chains of fifths (1/1, 3/2, 9/4) separated by a third (major or minor). The size of the major third can be set arbitrarily, and the minor third is derived from that. I can modulate to other chords in a number of ways:
;;
;; -Transpose the base pitch by 3/2 or 4/3 (dominant/subdominant)
;; -Transpose the base pitch up or down by the third and toggle between major/minor thirds (diatonic mediant/submediant)
;; -Transpose the base pitch up or down by the third (chromatic mediant/submediant)
;; -Toggle between major/minor
;; -Transpose by the interval between the major/minor thirds and toggle between major/minor (I'm not proficient enough to know what this is supposed to be called, but it's like alternating between Cm and BM).
;;
;; Sometimes several of these are combined (up to 4). This has to be done carefully to keep it from sounding too weird. I tried a couple other things besides that (hexatonic poles, etc.), but I thought they didn't work very well.
;;
;; Originally the idea was to use some exotic interval for the major third, but I found that most of them sounded too dissonant. For major thirds larger than about 32/25 (427 cents), some of the resulting intervals get too small. And for major thirds smaller than 5/4 (386 cents), the major and minor intervals are too close together. So the useful range is pretty small, and a lot of the ones I wanted to try (9/7, 11/9) don't really work. I thought the only major third sizes that worked well were 14/11, 81/64 (Pythagorean) and 5/4. I went with 5/4. But I want to revisit this idea with more exotic intervals. I'll just have to construct the chord differently so that it works better.
;; --------------------------------------------------------------------

#|
m maxima q quarter
l longa  e eighth
b brevis s sixteenth
w whole  t thirty-second
h half   x sixty-fourth
|#

;; https://github.com/benmca/csound-pieces/
;; http://listenfaster.tumblr.com/

(defun play-midi-note (time pitch velocity dur c)
     (fluidsynth:noteoff *synth* c pitch)
     (at time #'fluidsynth:noteon *synth* c pitch velocity)
     (at (+ time #[dur b]) #'fluidsynth:noteoff *synth* c pitch))

;; trompet
(fluidsynth:program-change *synth* 3 42)
(fluidsynth:program-change *synth* 2 43)
(fluidsynth:program-change *synth* 1 46)


(fluidsynth:program-change *synth* 4 1)

(fluidsynth:program-change *synth* 3 33)



(fluidsynth:program-change *synth* 1 1)
(fluidsynth:program-change *synth* 2 1)

(defun pp (chan time keys rhythms)
  (let ((note   (cm:keynum (cm:next keys)))
        (rhythm (cm:rhythm (cm:next rhythms) 30)))
    (play-midi-note time
                    (cm:keynum note)
                    30 rhythm chan)
    (aat (tempo-sync #[rhythm b]) #'pp chan it keys rhythms)))

(pp 1
    (tempo-sync #[1 b])
    (cm:new cm:cycle :of '(e3 gs4 b4 ds4))
    (cm:new cm:cycle :of '(e s q e. s.)))

(pp 2
    (tempo-sync #[1 b])
    (cm:new cm:cycle :of '(e4 gs4 b4 ds4))
    (cm:new cm:cycle :of '(s e s. e. q)))

#|
(flush-pending)
(off-with-the-notes *synth*)
|#

(defun p (chan vel time keys rhythms amps &key (life nil))
  ;;  (if (not (= chan 4))
  (if (or (null life)
          (> life 0))
      (let* ((amp    (cm:next amps))
             (note   (if (= amp 1) (cm:keynum (cm:next keys)) 0))
             (rhythm (cm:rhythm (cm:next rhythms) 30))
             (life   (if (integerp life) (1- life) nil)))
        (if (not (= note 0))
            (play-midi-note time
                            (cm:keynum note)
                            vel rhythm chan ))
        (aat (+ time #[rhythm b]) #'p chan vel it keys rhythms amps
             :life life))))

(defun q1 (chan vel time keys &key (life nil))
  (let ((rhythms (cm:new cm:cycle :of '(s s s s s s s s+q q)))
        (amps    (cm:new cm:cycle :of '(0 1 0 1 0 1 0 1   0))))
    (p chan vel time keys rhythms amps :life life)))

(defun q2 (chan vel time keys &key (life nil))
  (let ((rhythms (cm:new cm:cycle :of '(e e e e+q q)))
        (amps    (cm:new cm:cycle :of '(1 1 1 1 0))))
    (p chan vel time keys rhythms amps :life life)))

(defun q3 (chan vel time keys &key (life nil))
  (let ((rhythms (cm:new cm:cycle :of '(s q e.+q q)))
        (amps    (cm:new cm:cycle :of '(1 1 1 0))))
    (p chan vel time keys rhythms amps :life life)))

(defun q4 (chan vel time keys &key (life nil) )
  (let ((rhythms (cm:new cm:cycle :of '(e s s s s s s+q q)))
        (amps    (cm:new cm:cycle :of '(1 1 1 1 1 1 1 0))))
    (p chan vel time keys rhythms amps :life life)))

(progn
  (q1 1 10 (tempo-sync #[1 b]) (cm:new cm:heap :of '(gs4 b4 a4 fs4)))
  (q2 2 30 (tempo-sync #[2 b]) (cm:new cm:heap :of '(gs4 b4 a4 fs4)))
  (q3 3 60 (tempo-sync #[3 b]) (cm:new cm:heap :of '(e4 fs5)))
  (q4 4 45 (tempo-sync #[4 b]) (cm:new cm:heap :of '(fs3))) )
;; 33 sting
;; 40 trumpet
;; 37- winds wood
;; 40- winds brass
;; 46- winds synth
(fluidsynth:program-change *synth* 1 40)
(fluidsynth:program-change *synth* 2 1)
(fluidsynth:program-change *synth* 3 1)
(fluidsynth:program-change *synth* 1 1)
(fluidsynth:program-change *synth* 3 33)
(fluidsynth:program-change *synth* 4 40)

(q4 3 40
    (tempo-sync #[1 b])
    (cm:new cm:cycle :of '(e3 gs4 b4 ds4)))

(p 4 30 (tempo-sync #[1 b])
   (cm:new cm:cycle :of '(e3 gs4 b4 ds4))
   (cm:new cm:cycle :of '(s e s. e. q))
   (cm:new cm:cycle :of '(1)))

;; emergen.scm - launchOB

(progn
  (q1 1 30 (tempo-sync #[1 b])
      (cm:new cm:heap :of '(gs4 b4 a4 fs4)) :life 10)
  (q2 2 30 (tempo-sync #[1 b])
      (cm:new cm:heap :of '(gs4 b4 a4 fs4)) :life 10)
  (q3 4 35 (tempo-sync #[1 b])
      (cm:new cm:heap :of '(e4 fs5)) :life 10)
  (q4 4 45 (tempo-sync #[1 b])
      (cm:new cm:heap :of '(fs3)) :life 10) )


(defun q5 (rhythms)
  (let* ((rhythm (cm:rhythm (cm:next rhythms) 30)))
    (print "hey")
    (q1 1 30 (tempo-sync #[1 b])
        (cm:new cm:heap :of '(gs4 b4 a4 fs4)) :life 10)
    (q2 2 30 (tempo-sync #[1 b])
        (cm:new cm:heap :of '(gs4 b4 a4 fs4)) :life 10)
    (q3 4 35 (tempo-sync #[1 b])
        (cm:new cm:heap :of '(e4 fs5)) :life 10)
    (q4 4 45 (tempo-sync #[1 b])
        (cm:new cm:heap :of '(fs3)) :life 10)
    (aat (+ (now) #[rhythm s]) #'q5 rhythms)))
 
(q5 (cm:new cm:cycle :of '(e s s s s s s+q q)))
(q5 (cm:new cm:cycle :of '(w w+h w+w w+w+w)))
    
(q1 1 30 (tempo-sync #[1 b]) (cm:new cm:heap :of '(gs4 b4 as4 fs4)))

#|
(flush-pending)
(off-with-the-notes *synth*)
|#

;; --------------------
;; CM archive mailing list
;; ftp://ccrma-ftp.stanford.edu/pub/Lisp/old-cmdist.html
;; --------------------
(defvar chorale1 nil)
(setf chorale1 '((G2 B2 D3) (G3 B3 D4) (C4 E3 G3) (C4 E3 G3) (D4 FS3 A3)
                 (G3 B3 D4) (B3 D3 FS3) (B3 D3 FS3) (E3 G3 B3) (C3 E3 G3)
                 (C3 E3 G3) (C4 E4 G4) (FS3 A2 C3) (G2 B2 D3) (D3 FS3 A3)
                 (G2 B2 D3) (D3 FS2 A2) (E3 G2 B2 D3) (FS3 A2 C3) (G3 B2 D3)
                 (A3 C3 E3 G3) (D3 FS3 A3) (D3 FS3 A3 C4) (G2 B2 D3) (G2 B2 D3)
                 (G2 B2 D3) (G2 B2 D3) (A2 C3 E3) (FS3 A2 C3) (G3 B2 D3)
                 (G3 B2 D3) (G3 B2 D3) (G3 B2 D3) (G3 B2 D3) (D3 FS3 A2 C3)
                 (G2 B2 D3) (D3 FS3 A3) (E3 G3 B3) (E3 G3 B3) (B3 D3 FS3)
                 (B3 D3 FS3) (A3 C3 E3) (G3 B2 D3) (G3 B2 D3) (G3 B2 D3)
                 (D3 FS3 A3 C4) (D3 FS3 A3 C4) (G2 B2 D3) (G2 B2 D3) (G3 B2 D3)
                 (G2 B2 D3 F3) (C3 E3 G3) (G2 B2 D3) (D3 FS2 A2) (D3 FS2 A2 C3)
                 (G2 B2 D3) (G2 B2 D3) (FS3 A2 C3) (G3 B2 D3) (G2 B2 D3)
                 (D3 FS3 A3) (D3 FS3 A3 C4) (E3 G3 B3) (E3 G3 B3) (C3 E3 G3)
                 (C3 E3 G3) (G2 B2 D3) (G2 B2 D3) (D3 FS3 A3) (G3 B3 D4)
                 (D4 FS3 A3) (D4 FS3 A3) (C4 E3 G3) (C4 E3 G3) (E3 G3 B3)
                 (E3 G3 B3) (A3 C3 E3 G3) (D3 FS3 A3) (D3 FS3 A3 C4) 
                 (G2 B2 D3)))

#|
(defprocess play-chords (num chords rhy dur amp)
  (process repeat num
           for c = (next chords)
           do
           (dolist (k c)
             (output (new midi time (now) duration dur
                          keynum k amplitude amp)))
           (wait rhy)))

(events (play-chords 50 (markov-analyze chorale1 :order 1 :print nil)
                     .5 .4 .2)
        "midi.port")
|#

(defun m (time chords)
  (let ((chord (cm:next chords))
        (vel   (round (cosr 35 5 .5))))
    (dolist (k chord)
      (play-midi-note (now) (cm:keynum k) vel 1 1))
    (aat (+ time #[1 b]) #'m it chords)))

(m (tempo-sync #[1 b])
   (cm:markov-analyze chorale1 :order 1 :print? nil))

(defun m (time chords)
  (let ((chord (cm:next chords)))
    (play-midi-note time (cm:keynum (nth 0 chord)) 30 1 1)
    (play-midi-note (+ time #[1 b]) (cm:keynum (nth 1 chord)) 30 1 1)
    (play-midi-note (+ time #[2 b]) (cm:keynum (nth 2 chord)) 30 1 1)
    (aat (+ time #[3 b]) #'m it chords)))

;; --------------------------------
(defvar idxdur '((0.018 q) (.697 q)  (1.376 s)
                 (1.538 e) (1.869 s) (2.032 s)
                 (2.2 e)   (2.543 s) (2.705 q)
                 (3.373 e.)(3.895 e) (4.232 q)
                 (4.894 e) (5.236 s)))

(defun quarterDur->tempo (quarterdur)
  (* 60 (/ 1.0 quarterdur)))



;; --------------------------------
#|
(flush-pending)
(off-with-the-notes *synth*)
|#

;; --------------------------------
;; https://www.quicklisp.org/beta/UNOFFICIAL/docs/fomus/doc/ch09s03.html
;; 50 80 / 40 70
;; 60 80 / 40 60
(defun q (time rhythms)
  (let ((rhythm (cm:rhythm (cm:next rhythms) 30)))
    (play-midi-note time (cm:between 50 70) 30 1 1)
    (play-midi-note time (cm:between 50 70) 30 1 2)
    (play-midi-note time (cm:between 50 70) 30 1 2)
    (aat (tempo-sync #[rhythm b]) #'q it rhythms)))

(q (tempo-sync #[1 b]) (cm:new cm:cycle :of '(e q s s q)))


;; Data set of Bach
;; https://archive.ics.uci.edu/ml/datasets/Bach+Chorales


;; Fractal music
;; https://github.com/holgafreak/maxann-grace
;; https://web.archive.org/web/20000118053335/http://www.sci.fi/~mjkoskin/fractal.cm

;; ----------------------------------
;; Morse-Thue
;; From:
;; - "Music composition with lisp"
;; - nudruz
 
(defvar mtrules nil)

(setf mtrules '((0 :-> (0 1)) 
                (1 :-> (1 0))))

;; RW-NEXT -- returns next complete generation of rewrite
;; rwthing = rules; alist = input string
;; example: (rw-next mtrules '(1 0)) = (1 0 0 1)
(defun rw-next (rwthing alist)
  (let* ((this-rw (cm:new cm:rewrite
                    :of (append rwthing '((rw-end :-> rw-end)))
                    :initially (append alist (list 'rw-end))))
         (sink (cm:next this-rw (+ (length alist) 1))))
    (loop for x = (cm:next this-rw) until (eql x 'rw-end) collect x)))

;; RWGEN -- returns arbitrary generation of rewrite
;; (rwgen mtrules '(1 0) 2) =  (1 0 0 1 0 1 1 0)
(defun rwgen (rwrules initgen gennbr)
  (case gennbr 
    (0 initgen)
    (1 (rw-next rwrules initgen))
    (t (rw-next rwrules (rwgen rwrules initgen (- gennbr 1))))))

(defun pw (chan time notes amps)
  (let* ((amp  (cm:next amps)))
    (if (= amp 1)
        (play-midi-note time (cm:next notes) 40 1 chan))
    (aat (tempo-sync #[1 b]) #'pw chan it notes amps)))

; clav  - 19
; brass - 40
(fluidsynth:program-change *synth* 1 16)
(fluidsynth:program-change *synth* 2 41)

(defvar *chord* nil)
(setf *chord* (make-chord 50 60 3 (scale 0 'aeolian)))
(defvar *cycle* nil)
(setf *cycle* (cm:new cm:cycle :of *chord*))

(pw 1
    (tempo-sync #[1 b])
    (cm:new cm:cycle :of *chord*)
    (cm:new cm:cycle :of (rwgen mtrules '(1 0) 2)))
 
(pw 2
    (tempo-sync #[1 b])
    (cm:new cm:cycle :of *chord*)
    (cm:new cm:cycle :of (rwgen mtrules '(1 0) 4))))

#|
(flush-pending)
(off-with-the-notes *synth*)
|#

;; This one has "dynamic" chord progression

(setf (bpm *tempo*) 60)

(defvar *chord* nil)
(defvar *notes* nil)
(setf *chord* (make-chord 50 65 5 (scale 0 'phrygian)))
(setf *notes*  (cm:new cm:cycle :of *chord*))

(defun pw (chan time amps)
  (let* ((amp  (cm:next amps)))
    (if (= amp 1)
        (play-midi-note time (cm:next *notes*) 40 1 chan))
    (aat (tempo-sync #[1 b]) #'pw chan it amps)))

(pw 1
    (tempo-sync #[1 b])
    (cm:new cm:cycle :of (rwgen mtrules '(1 0) 2)))

(pw 2
    (tempo-sync #[1 b])
    (cm:new cm:cycle :of (rwgen mtrules '(1 0) 4))))


;; EXPWARP -- 'warps' pits by expt factor
;; (above optional bass-note, or lowest note in chd)
(defun expwarp (pits factor &optional (bassnote nil))
      (let* ((orig-hz (remove-duplicates (cm:hertz pits)))
	     (bn (if bassnote bassnote (apply #'min orig-hz)))
	     (hzdiffs (mapcar (lambda (x) (- x bn)) orig-hz)))
	(loop for n to (- (length orig-hz) 1) collect
	      (cm:keynum
	       (+ bn (* (nth n hzdiffs) factor))
	       :hz 't))))

(fluidsynth:program-change *synth* 1 33)

(defvar *chords* nil)
(setf *chords*  (cm:new cm:cycle :of (loop :for n :from 1.0 :to 2.0 :by .1 :collect (expwarp '(36 55 64) n))))

(defun ew (time)
  (let ((chord (cm:next *chords*)))
    (dolist (k chord)
      (play-midi-note time (round k) 30 1 1))
    (aat (+ (now) #[1 b]) #'ew it)))

(ew (now))

(defun ewinc (time)
  (setf *chords* (cm:new cm:cycle :of (loop :for n :from 1.0 :to 2.0 :by (random-list '(.2 .3 .4)) :collect (expwarp '(36 55 64) n))))
  (aat (+ time #[4 b]) #'ewinc it))

(ewinc .1)

;; --------------------------------
;; extepore tutorial
;; cycle with two callbacks

(defun ext (time notes)
  (play-midi-note time (car notes) 40 1 1)
  (if (null (cdr notes))
      (aat (+ time #[1 b]) #'ext it '(60 62 65))
      (aat (+ time #[1 b]) #'ext it (cdr notes))))

(ext (now) '(60 62 65) )

#|
(flush-pending)
(off-with-the-notes *synth*)
|#

;; --------------------------------
;; https://www.quicklisp.org/beta/UNOFFICIAL/docs/fomus/doc/ch09s03.html
;; 50 80 / 40 70
;; 60 80 / 40 60
(defun q (time rhythms)
  (let ((rhythm (cm:rhythm (cm:next rhythms) 30)))
    (play-midi-note time (cm:between 50 70) 30 1 1)
    (play-midi-note time (cm:between 50 70) 30 1 2)
    (play-midi-note time (cm:between 50 70) 30 1 2)
    (aat (tempo-sync #[rhythm b]) #'q it rhythms)))

(q (tempo-sync #[1 b]) (cm:new cm:cycle :of '(e q s s q)))


;; Data set of Bach
;; https://archive.ics.uci.edu/ml/datasets/Bach+Chorales


;; Fractal music
;; https://github.com/holgafreak/maxann-grace
;; https://web.archive.org/web/20000118053335/http://www.sci.fi/~mjkoskin/fractal.cm

;; ----------------------------------
;; Morse-Thue
;; From:
;; - "Music composition with lisp"
;; - nudruz
 
(defvar mtrules nil)

(setf mtrules '((0 :-> (0 1)) 
                (1 :-> (1 0))))

;; RW-NEXT -- returns next complete generation of rewrite
;; rwthing = rules; alist = input string
;; example: (rw-next mtrules '(1 0)) = (1 0 0 1)
(defun rw-next (rwthing alist)
  (let* ((this-rw (cm:new cm:rewrite
                    :of (append rwthing '((rw-end :-> rw-end)))
                    :initially (append alist (list 'rw-end))))
         (sink (cm:next this-rw (+ (length alist) 1))))
    (loop for x = (cm:next this-rw) until (eql x 'rw-end) collect x)))

;; RWGEN -- returns arbitrary generation of rewrite
;; (rwgen mtrules '(1 0) 2) =  (1 0 0 1 0 1 1 0)
(defun rwgen (rwrules initgen gennbr)
  (case gennbr 
    (0 initgen)
    (1 (rw-next rwrules initgen))
    (t (rw-next rwrules (rwgen rwrules initgen (- gennbr 1))))))

(defun pw (chan time notes amps)
  (let* ((amp  (cm:next amps)))
    (if (= amp 1)
        (play-midi-note time (cm:next notes) 40 1 chan))
    (aat (tempo-sync #[1 b]) #'pw chan it notes amps)))

; clav  - 19
; brass - 40
(fluidsynth:program-change *synth* 1 33)
(fluidsynth:program-change *synth* 2 41)

(defvar *chord* nil)
(setf *chord* (make-chord 50 60 3 (scale 0 'aeolian)))
(defvar *cycle* nil)
(setf *cycle* (cm:new cm:cycle :of *chord*))

(pw 1
    (tempo-sync #[1 b])
    (cm:new cm:cycle :of *chord*)
    (cm:new cm:cycle :of (rwgen mtrules '(1 0) 2)))
 
(pw 2
    (tempo-sync #[1 b])
    (cm:new cm:cycle :of *chord*)
    (cm:new cm:cycle :of (rwgen mtrules '(1 0) 4))))

#|
(flush-pending)
(off-with-the-notes *synth*)
|#

;; This one has "dynamic" chord progression

(setf (bpm *tempo*) 60)

(defvar *chord* nil)
(defvar *notes* nil)
(setf *chord* (make-chord 50 65 5 (scale 0 'phrygian)))
(setf *notes*  (cm:new cm:cycle :of *chord*))

(defun pw (chan time amps)
  (let* ((amp  (cm:next amps)))
    (if (= amp 1)
        (play-midi-note time (cm:next *notes*) 40 1 chan))
    (aat (tempo-sync #[1 b]) #'pw chan it amps)))

(pw 1
    (tempo-sync #[1 b])
    (cm:new cm:cycle :of (rwgen mtrules '(1 0) 2)))

(pw 2
    (tempo-sync #[1 b])
    (cm:new cm:cycle :of (rwgen mtrules '(1 0) 4))))


;; EXPWARP -- 'warps' pits by expt factor
;; (above optional bass-note, or lowest note in chd)
(defun expwarp (pits factor &optional (bassnote nil))
      (let* ((orig-hz (remove-duplicates (cm:hertz pits)))
	     (bn (if bassnote bassnote (apply #'min orig-hz)))
	     (hzdiffs (mapcar (lambda (x) (- x bn)) orig-hz)))
	(loop for n to (- (length orig-hz) 1) collect
	      (cm:keynum
	       (+ bn (* (nth n hzdiffs) factor))
	       :hz 't))))

(fluidsynth:program-change *synth* 1 33)

(defvar *chords* nil)
(setf *chords*  (cm:new cm:cycle :of (loop :for n :from 1.0 :to 2.0 :by .1 :collect (expwarp '(36 55 64) n))))

(defun ew (time)
  (let ((chord (cm:next *chords*)))
    (dolist (k chord)
      (play-midi-note time (round k) 30 1 1))
    (aat (+ (now) #[1 b]) #'ew it)))

(ew (now))

(defun ewinc (time)
  (setf *chords* (cm:new cm:cycle :of (loop :for n :from 1.0 :to 2.0 :by (random-list '(.2 .3 .4)) :collect (expwarp '(36 55 64) n))))
  (aat (+ time #[4 b]) #'ewinc it))

(ewinc .1)

;; --------------------------------
;; extepore tutorial
;; cycle with two callbacks
;; spicing it up with pick, but the gist is that if defined like this
;;   we can change the things "dynamicly" without recurring to globs

(defun ext (time notes)
  (play-midi-note time (car notes) 40 1 1)
  (if (null (cdr notes))
;;      (aat (+ time #[1 b]) #'ext it `(60 62 ,(cm:pick 65 67)))
      (aat (+ time #[1 b]) #'ext it (cm:pick '(60 62 65)
                                             '(60 62 67)
                                             '(60 62 67 69) ))
      (aat (+ time #[1 b]) #'ext it (cdr notes))))

(ext (now) '(60 62 65) )

(setf (bpm *tempo*) 60)

(defun exte (chan time notes rhythms)
  (print (car notes))
  (print (car rhythms))
  (print "A")
  (play-midi-note time (car notes) 40 (car rhythms) 1)
  (aat (+ time #[(car rhythms) b]) #'exte chan it
       (if (null (cdr notes))
           '(60 62 65 69 67)
           (cdr notes))
       (if (null (cdr rhythms))
           '(1/4 1/4 1/2 1/4)
           (cdr rhythms))))

(exte 1 (now) '(60 62 65 69 67) '(1/4 1/4 1/2 1/4))
(exte 2 (now) '(60 62 65 69 67) '(1/4 1/4 1/2 1/4))

; Pick 2 random number from the list-lispy
;(loop :for x :below 2 :collect (cm:pickl '(60 45 63)))

(defun extem (chan time notes rhythms)
  (play-midi-note time (car notes) 40 (car rhythms) chan)
  (aat (+ time #[(car rhythms) b]) #'extem chan it
       (if (null (cdr notes))
           (loop :for x :below 4 :collect (cm:pickl '(60 62 64 67 69)))
           (cdr notes))
       (if (null (cdr rhythms))
           '(1/4 1/4 1/4 1/4)
           (cdr rhythms))))

(extem 2 (now) '(60 62 64 67) '(1/4))

(defun extemp (chan time notes rhythms)
;  (print (+ 60 (* 50 (cos (* 3.141592 0.0315 time)))))
  (play-midi-note time
                  (car notes)
                  (abs (round (+ 1 (* 40 (cos (* 3.141592 0.0315 (get-internal-real-time)))))))
;;                  (abs (round (+ 1 (* 40 (cos (* 3.141592 0.0315 (get-universal-time)))))))
;;                  (round (cosr 30 10 .6))
                  (car rhythms) chan)
  (aat (+ time #[(car rhythms) b]) #'extemp chan it
       (if (null (cdr notes))
           (loop :for x :below 4 :collect (cm:pickl '(60 62 64 67 69)))
           (cdr notes))
       (if (null (cdr rhythms))
           '(1/4 1/4 1/4 1/4)
           (cdr rhythms))))

(extemp 2 (now) '(60 62 64 67) '(1/4))
(extemp 3 (now) '(60 62 64 67) '(1/2))

#|
(flush-pending)
(off-with-the-notes *synth*)
|#
