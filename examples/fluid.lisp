(in-package :somecepl)

;; Run it on the repl
;; (rt-start)

(ql:quickload :incudine-fluidsynth)

(defun off-with-the-notes (s)
  (loop :for c :from 0 :upto 25 :do
     (mapcar (lambda (x) (fluidsynth:noteoff s c x)) (range 120))))

(defun all-piano (s &optional (instrument 0))
  (dotimes (i 32) (fluidsynth:program-change s i instrument) ))

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
  
(setf (fluidsynth:setting *fluid-settings* "synth.gain") 1.2)
(setf (fluidsynth:setting *fluid-settings* "synth.gain") .9)
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
                      (round (cosr 60 5 1/2))
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
        (play-midi-note time
                        (cm:keynum note)
                        (round (cosr vel 5 .4))
                        beat
                        chan)
        (setf notes (alexandria:rotate notes 1))))
  (aat (+ time #[beat b]) #'eu
       it
       vel
       chan
       beat
       (alexandria:rotate rythm 1)
       notes))

(flush-pending)

(eu (tempo-sync #[1 b])   60 0 1/2 (bjorklund 5 13) '(:c3 :g3 :d3))
(eu (tempo-sync #[2 b])   50 1 1   (bjorklund 4 4)  '(:c3 :g3 :d3))
(eu (tempo-sync #[2.5 b]) 50 2 1/2 (bjorklund 3 8)  '(:c3 :g3 :d3))

(eu (tempo-sync #[2.5 b]) 50 2 1/2 (bjorklund 5 12)  '(:c3 :g3 :d3))
(eu (tempo-sync #[2 b])   50 1 1   (bjorklund 4 12)  '(:c3 :g3 :d3))

(eu (funcall *metro* (funcall *metro* 'get-beat 1.0)) 60 0 1/2 (bjorklund 5 13) '(:c3 :g3 :d3))
(eu (funcall *metro* (funcall *metro* 'get-beat 2.0)) 50 1 1   (bjorklund 4 4)  '(:c3 :g3 :d3))
(eu (funcall *metro* (funcall *metro* 'get-beat 2.5)) 50 2 1/2 (bjorklund 3 8)  '(:c3 :g3 :d3))

;; -----------
;; Playground
;; https://digego.github.io/extempore/note-level-music.html
;; http://impromptu.moso.com.au/tutorials/making_music/
;; -----------
(setf (fluidsynth:setting *fluid-settings* "synth.gain") .5)

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

(defun brown (time chan dur notes)
  (play-midi-note time (cm:next notes) 30 dur chan)
  (aat (+ time #[dur b]) #'brown it chan dur notes))

(brown (tempo-sync #[1 b]) 0 1
       (cm:new cm:cycle :of (brownian-motion 40 6)))

(brown (tempo-sync #[.5 b]) 1 1
       (cm:new cm:cycle :of (brownian-motion 60 6)))

;; --------------------------------------------------------------------
;; GOTO 2014 • Programming In Time - Live Coding for Creative Performances
;; https://www.youtube.com/watch?v=Sg2BjFQnr9s
;; -------------------------------------------------------
(defvar *root* nil)
(defvar *myscale* nil)
(setf *root* 60)
(setf *myscale* (scale 0 'aeolian))
(setf (bpm *tempo*) 60)

(defun left (time dur)
  (setf *root* (alexandria:random-elt
                (cdr (assoc *root* '((60 58 55)
                                     (58 60 56)
                                     (56 55 58)
                                     (55 60 56))))))
  (play-midi-note time (+ *root* -12) 30 dur 0)
  (aat (+ time #[3/2 b])
       #'play-midi-note it (+ -5  *root*) 30 (- dur 3/2) 1)
  (aat (+ time #[dur b]) #'left it dur))

(defun right (time dur)
  (play-midi-note time
                  (round (qcosr *myscale* *root* 7 1.1))
                  (round (cosr 30 7 1.1))
                  dur 2)
  (aat (+ time #[dur b]) #'right it dur))

(left (tempo-sync #[4 b]) 4)
(right (tempo-sync #[4 b]) 1/4)

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
(defun play-midi-note-loop (time pitch velocity dur c)
     (at (- time 10) #'fluidsynth:noteoff *synth* c pitch)
     (at time #'fluidsynth:noteon *synth* c pitch velocity)
     (at (+ time #[dur b]) #'fluidsynth:noteoff *synth* c pitch))


(defun play-midi-note (time pitch velocity dur c)
;;     (at (- time 10) #'fluidsynth:noteoff *synth* c pitch)
     (at time #'fluidsynth:noteon *synth* c pitch velocity)
     (at (+ time #[dur b]) #'fluidsynth:noteoff *synth* c pitch))

;; All piano
(dotimes (i 32) (fluidsynth:program-change *synth* i 1) )

(fluidsynth:program-change *synth* 4 34)
(fluidsynth:program-change *synth* 2 1)
(fluidsynth:program-change *synth* 3 42)

(fluidsynth:program-change *synth* 4 0)
(fluidsynth:program-change *synth* 5 0)
(fluidsynth:program-change *synth* 6 77)
(fluidsynth:program-change *synth* 7 77)
(fluidsynth:program-change *synth* 5 0)
(fluidsynth:program-change *synth* 2 0)

(fluidsynth:program-change *synth* 3 43)
(fluidsynth:program-change *synth* 3 4)
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

(setf (fluidsynth:setting *fluid-settings* "synth.gain") .3)

(defun spattern (time notes pattern lengths r))
  (when (not (= r (random 21)))
  (print r)
  (print pattern)
  (let* ((lpattern   (length pattern))
         (t-lpattern (tempo-sync #[(/ lpattern 2) b]))
         (pbeat      (remove nil
                             (loop
                                :for beat :in pattern
                                :for nbeat :from 0 :upto 64
                                :collect (when (= 1 beat)
                                           nbeat)))))
    ;; Take the list of beats where a note is played
    ;; pbeat '(0 4 8 32) and schedule it
    (loop :for cbeat :in pbeat :do
       (let ((note   (cm:next notes))
             (length (cm:next lengths)))
         (push cbeat *mtempos*)
         (if (= cbeat 0)
             (play-midi-note-loop time note 25 length r)
             (play-midi-note-loop (+ time #[(/ cbeat 2) b]) note 25 length r))))
    (aat t-lpattern #'spattern it notes pattern lengths r))))

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
              (if (and (= 1 (first accumbeats))
                       (= pbeat 0))
                  (progn
                    (print "endswap")
;;;                    (setf *mtempos* (append *mtempos* (list '(0))))
                    (aat t-nbeat #'ppattern it lpattern notes length2 length1
                         :pbeat pbeat
                         :chan nchan
                         :pchan npchan))
                  (aat t-nbeat #'ppattern it lpattern notes length1 length2
                       :pbeat pbeat
                       :chan nchan
                       :pchan npchan)))
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

(defun mbox (time lpattern note length1 length2 bottom up pc)  
  (setf *mtempos* nil)
  (let* ((midinote (cm:keynum note))
         (notes    (loop
                      :for x :from bottom :to up
                      :collect (relative midinote x pc))))
    (ppattern time lpattern notes length1 length2 :play "yes")))

#|
(mbox (tempo-sync #[1 b]) 32 :E 4 3 -14 24 (scale 1 'dorian))
(mbox (tempo-sync #[1 b]) 32 :C 9 14.5 -14 14 (scale 0 'dorian))
(mbox (tempo-sync #[1 b]) 32 :G 10 3.5 -14 14 (scale 0 'dorian))

(mbox (tempo-sync #[1 b]) 32 :C 8 14.5 -7 14 (scale 0 'dorian))
(mbox (tempo-sync #[1 b]) 32 :D 9 1.5 -14 14 (scale 0 'dorian))

(mbox (tempo-sync #[1 b]) 32 :C 7 3 -7 14 (scale 0 'aeolian)) 

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
;  (if (= chan 3) (setf vel 40))
;  (if (not (= i 200))
      (let* ((i    (1+ i))
             (note (cm:next p))
             (dur  (cm:next q))
             (mul  12))
        (setf *c0* (cm:interp i 0 .5 90 4))
       ;; (setf *c0* 0)
       ;; (setf i 0)
        (if (not (equal note 'r))
            (play-midi-note time (cm:keynum (cm:transpose note offset)) vel (* dur (- mul 3) ) chan))
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

(fluidsynth:program-change *synth* 3 10)
(fluidsynth:program-change *synth* 1 33)
(fluidsynth:program-change *synth* 2 10)

(fluidsynth:program-change *synth* 3 26)
(fluidsynth:program-change *synth* 1 77)
(fluidsynth:program-change *synth* 2 33)

(at (funcall *metro* (funcall *metro* 'get-beat 4)) #'cage -12 40 3)
(at (funcall *metro* (funcall *metro* 'get-beat 4.5)) #'cage 0 45 1)
(at (funcall *metro* (funcall *metro* 'get-beat 1.75)) #'cage 12 45 2)

(at (tempo-sync #[4 b]) #'cage -12 40 3)
(at (tempo-sync #[4.5 b]) #'cage 0 45 1)
(at (tempo-sync #[1.75 b]) #'cage 12 45 2)


(cage   0 50 1)
(cage  12 30 2)

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
;;     (fluidsynth:noteoff *synth* c pitch)
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

(defvar *metro* nil)
(setf *metro* (make-metro 90))

(defun pp (chan time keys rhythms)
  (let ((note   (cm:keynum (cm:next keys)))
        (rhythm (cm:rhythm (cm:next rhythms) 30)))
    (play-midi-note time
                    (cm:keynum note)
                    40 rhythm chan)
    (aat (+ time #[rhythm b]) #'pp chan it keys rhythms)))


(defun ppp (chan time keys rhythms)
  (let ((note   (cm:keynum (cm:next keys)))
        (rhythm (cm:rhythm (cm:next rhythms) 30)))
    (play-midi-note (funcall *metro* time)
                    (cm:keynum note)
                    40 rhythm chan)
    (at (funcall *metro* (+ time rhythm)) #'ppp chan (+ time rhythm) keys rhythms)))

(pp 1
;;    (funcall *metro* (funcall *metro* 'get-beat 4))
    (tempo-sync #[4.25 b])
;    (funcall *metro* 'get-beat .5)
    (cm:new cm:cycle :of '(e3 gs4 b4 ds4))
    (cm:new cm:cycle :of '(e s q e. s.)))

(ppp 1
    (funcall *metro* 'get-beat .25)
    (cm:new cm:cycle :of '(e3 gs4 b4 ds4))
    (cm:new cm:cycle :of '(e s q e. s.)))

(pp 2
;;    (funcall *metro* (funcall *metro* 'get-beat 4.75))
    (tempo-sync #[4.75 b])
;;    (funcall *metro* 'get-beat 1)
    (cm:new cm:cycle :of '(e4 gs4 b4 ds4))
    (cm:new cm:cycle :of '(s e s. e. q)))

(ppp 2
    (funcall *metro* 'get-beat 4.0)
    (cm:new cm:cycle :of '(e4 gs4 b4 ds4))
    (cm:new cm:cycle :of '(s e s. e. q)))

#|
(flush-pending)
(off-with-the-notes *synth*)
|#

(defun p (chan vel time keys rhythms amps &key (life nil))
  ;;  (if (not (= chan 4))
  ;; (cm:odds .1 
  ;;          (q3 4 35 time
  ;;              (cm:new cm:heap :of '(e4 fs5)) :life 10))
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

(q4 3 20
    (funcall *metro* (funcall *metro* 'get-beat 3.5))
;;    (tempo-sync #[1 b])
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
        (tempo-sync #[4 b])
;;    (funcall *metro* (funcall *metro* 'get-beat 4.0))
    (cm:new cm:cycle :of *chord*)
    (cm:new cm:cycle :of (rwgen mtrules '(1 0) 2)))
 
(pw 2
    (tempo-sync #[2.5 b])
;;    (funcall *metro* (funcall *metro* 'get-beat 2.5))
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

(defun pw (chan time amps)
  (let* ((amp  (cm:next amps)))
    (if (= amp 1)
        (play-midi-note (funcall *metro* time)
                        (cm:next *notes*)
                        40
                        (funcall *metro* 'dur 1) chan))
    (at (funcall *metro* (+ time 1)) #'pw chan (+ time 1) amps)))

(pw 1
    ;;    (tempo-sync #[1 b])
;;    (funcall *metro* (funcall *metro* 'get-beat 1.75))
    (funcall *metro* 'get-beat 1.0)
    (cm:new cm:cycle :of (rwgen mtrules '(1 0) 2)))

(pw 2
    ;;    (tempo-sync #[2.75 b])
;;    (funcall *metro* (funcall *metro* 'get-beat 2.))
    (funcall *metro* 'get-beat 2.75)
    (cm:new cm:cycle :of (rwgen mtrules '(1 0) 4)))


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

(ewinc (now))

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

(defun exte (chan time notes rhythms))
  (play-midi-note time (car notes) 40 (car rhythms) 1)
  (aat (+ time #[(car rhythms) b]) #'exte chan it
       (if (null (cdr notes))
           '(60 62 65 69 67)
           (cdr notes))
       (if (null (cdr rhythms))
           '(1/4 1/4 1/2 1/4)
           (cdr rhythms))))

(exte 1 (tempo-sync #[1 b])   '(60 62 65 69 67) '(1/4 1/4 1/2 1/4))
(exte 2 (tempo-sync #[.75 b]) '(60 62 65 69 67) '(1/4 1/4 1/2 1/4))

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


;;; ----------------------------------
;; Extempore tutorial - Pitch classes
;;; ----------------------------------

;; Proofs that we cannot always transpose
;; things by adding
(pc:? 60 '(0 4 7))                                                                                       
(pc:? 84 '(0 4 7))                                                                                       
(pc:? 35 '(0 4 7))                                                                                       
(pc:? 79 '(0 4 7))  

(print (ispitch 60 '(0 4 7))) ;T
(print (ispitch 84 '(0 4 7))) ;T
(print (ispitch 35 '(0 4 7))) ;NIL
(print (ispitch 79 '(0 4 7))) ;T

;; ---

;; this chooses a C in any octave
(print (pcrrandom 0 127 '(0)))
;; this chooses any note from a D minor chord in octave 4
(print (pcrrandom 60 73 '(2 5 7)))
;; this chooses any note from a C pentatonic octaves 3-6
(print (pcrrandom 48 97 '(0 2 4 7 9)))

;; ---

(print (mapcar (lambda (p)
                 (ispitch
                  (+ p 7)
                  '(0 2 4 5 7 9 11)))
               '(60 62 64 65 67 69 71)))

(print (mapcar (lambda (p)
                 (ispitch
                  (+ p 5)
                  '(0 2 4 5 7 9 11)))
               '(60 62 64 65 67 69 71)))

(print (mapcar (lambda (p)
                 (ispitch
                  (+ p 4)
                  '(0 2 4 5 7 9 11)))
               '(60 62 64 65 67 69 71)))

;; ---

(print (relative 60 1 '(0 2 4 5 7 9 11))); 62
(print (relative 60 4 '(0 2 4 5 7 9 11))); 67
(print (relative 67 0 '(0 2 4 5 7 9 11))); 67
(print (relative 60 -2 '(0 2 4 5 7 9 11))); 57

;; -------------------

;; define a melody
(defvar *melody* nil)
(setf *melody* (loop
                  :for x
                  :from 1
                  :to 24
                  :collect (pcrrandom 60 83 '(0 2 4 5 7 9 11))))

;; Doc: Scheme's named-loops (aka let-loop) can be written with
;;      Lisp's do loops
;;      https://stackoverflow.com/questions/30178852/is-there-a-style-convention-for-common-lisp-recursive-helper-functions

;; define a random walk melody seeded with 60
;; (we remove this at the end with cdr)
;; "random walk"
(setf *melody* (do ((i 0 (1+ i))
                    (lst '(60) (cons
                                (relative (car lst)
                                          (cm:pick -1 1)
                                          '(0 2 4 5 7 9 11))
                                lst)))
                   ((>= i 24)(cdr (reverse lst)))))

(setf *melody* (do ((i 0 (1+ i))
                    (lst '(60) (cons
                                (relative (car lst)
                                          (cm:pick -2 -1 1 2 3)
                                          '(0 2 4 5 7 9 11))
                                lst)))
                   ((>= i 24)(cdr (reverse lst)))))

;; define harmony up a perfect 5th (4 places away in the pitch class set)
(defvar *harmony* nil)
(setf *harmony* (mapcar (lambda (p) (relative p 4 '(0 2 4 5 7 9 11))) *melody*))

;; set c at start and end                                                                                
(setf *melody* (cons 60 *melody*))
(setf *harmony* (cons 60 *harmony*))
(setf *melody* (reverse (cons 60 (reverse *melody*))))
(setf *harmony* (reverse (cons 60 (reverse *harmony*))))

;; random rhythm                                                        
(defvar *rhythm* nil)
(setf *rhythm* (loop :for x :from 1 :to 24 :collect (cm:pick 1/2 1)))
;; set long start and end to rhythm
(setf *rhythm* (cons 2 *rhythm*))
(setf *rhythm* (reverse (cons 2 (reverse *rhythm*))))

(defun organum (time melody harmony rhythm)
  (play-midi-note time (car melody) 30 (car rhythm) 1)
  (play-midi-note time (car harmony) 30 (car rhythm) 2)
  (if (not (null (cdr melody)))
      (aat (+ time #[(car rhythm) b]) #'organum it (cdr melody) (cdr harmony) (cdr rhythm))))

;; start
(fluidsynth:program-change *synth* 1 77)
(fluidsynth:program-change *synth* 2 77)

(organum (now) *melody* *harmony* *rhythm*)        

;;; -----------------------------------------
;; Extempore tutorial - Making chords with PC
;;; -----------------------------------------
(defun crazy-chord (time)
  (play-midi-note time (pcrrandom 24 97 '(0 4 7 10 2 3 5 9 6 11 1)) 40 .3 1)
  (aat (+ time #[.3 b]) #'crazy-chord it))

(defun crazy-chord (time)
  (play-midi-note time (pcrrandom 24 97 '(0 4 7 10)) 40 .3 1)
  (aat (+ time #[.3 b]) #'crazy-chord it))

(crazy-chord (now))

; C-major and repeat 
(defun chords (time)
  (mapcar (lambda (x) (play-midi-note time x 30 1 1))
          (make-chord 60 72 3 '(0 4 7)))
  (aat (+ time #[1 b]) #'chords it))

(chords (now))

;; I IV V
;; I-C IV-F V-E
(defun chords (time chord)
  (mapcar (lambda (x)
            (play-midi-note time x 30 1 1))
          (make-chord 48 90 3 chord))
  (aat (+ time #[1 b]) #'chords
       it
       (if (> (random 1.0) .8)
           (cm:pick '(0 4 7) '(5 9 0) '(7 11 2))
           chord)))

(chords (now) '(0 4 7))

;; ------------------------------
;; Extempore tutorial - Harmony
;; ------------------------------

;; markov chord progression I IV V
(defun exhar (time pc)
  (mapcar (lambda (n c)
            (play-midi-note time n 30 1 c))
          (make-chord 60 73 3
                      (cm:pickl (cdr (assoc pc '(((0 4 7) (5 9 0) (7 11 2))
                                                 ((5 9 0) (7 11 2)(0 4 7))
                                                 ((7 11 2)(0 4 7))) :test #'equal))))
          '(1 2 0))
  (aat (+ time #[1 b]) #'exhar it pc))

(exhar (now) '(0 4 7))

;; markov chord progression I ii iii IV V vi vii
(defun progression (time degree)
  (mapcar
   (lambda (x) (play-midi-note time x 30 1 (random 20)))
   (make-chord 48 77 5 (diatonic 0 '^ degree)))
  (aat (+ time #[1 b]) #'progression it
       (cm:pickl (cdr (assoc degree '((i iv v iii vi)
                                      (ii v vii)
                                      (iii vi)
                                      (iv v ii vii i)
                                      (v i vi)
                                      (vii v i)
                                      (vi ii)))))))
 
(progression (now) 'i)


#|
(define play-note-mord                                                                                   
  (lambda (time inst pitch vol duration pc)                                                               
    (play-note (- time 5000) inst pitch (- vol 10) 2500)                                                 
    (play-note (- time 2500) inst (pc:relative pitch 1 pc) (- vol 10) 2500)                              
    (play-note time inst pitch vol (- duration 5000))))
|#
;; mordant
(defun play-note-mord (time pitch vol dur pc)
  (play-midi-note (- time #[1/4 b])
                  pitch
                  (- vol 10)
                  1/2
                  1)
  (play-midi-note (- time #[1/8 b])
                  (relative pitch 1 pc)
                  (- vol 10)
                  1/2
                  2)
  (play-midi-note time
                  pitch
                  vol dur 3))

;; markov chord progression I ii iii IV V vi vii
(defun progression (time degree)
  (let ((dur (if (member degree '(i iv)) 2 1)))
    (mapcar (lambda (x c) (if (and (> x 70) (> (random 1.0) .7))
                       (play-note-mord time x (cm:pick 30 40) (* .9 dur) '(0 2 4 5 7 9 11))
                       (play-midi-note time x (cm:pick 30 40) (* .9 dur) c)))
            (make-chord 40 78 4 (diatonic 0 '^ degree))
            '(6 7 8 9))
    (aat (+ time #[(* .9 dur) b]) #'progression (+ time #[dur b])
         (cm:pickl (cdr (assoc degree '((i iv v iii vi)
                                        (ii v vii)
                                        (iii vi)
                                        (iv v ii vii i)
                                        (v i vi)
                                        (vii v i)
                                        (vi ii))))))))                                                                                               
(progression (now) 'i)

(fluidsynth:program-change *synth* 0 78)

;;; -------------------------
;; Extempore tutorial - Beats
;;; -------------------------

(fluidsynth:program-change *synth* 2 1)

(defun drum-loop2 (time dur))
  (play-midi-note time 40 50 dur 2)
  (aat (+ time #[(* .5 dur) b]) #'drum-loop2
       (+ time #[dur b])
       (cm:pick 1.25 1.75)))

(drum-loop2 (tempo-sync #[1 b]) 3/4)

;; beat loop at 120bpm
(defun drump-loop (time dur)
  (play-midi-note time 40 50 dur 1)
  (aat (+ time #[dur b]) #'drump-loop
       (+ time #[dur b])
       (cm:pick 1/3 1/2 1/4) ))
  
(drum-loop (now) 1/4)
(defvar *metro* nil)
(setf *metro* (make-metro 60))

;; (funcall *metro* 'get-beat 4)
;; 88 -- BEAT

;; (funcall *metro* 88)
;; 2.88797696d8 -- GLOBAL SAMPLES

;; (funcall *metro* 'dur 1)
;; 57600 -- N SAMPLES
(defun drum-loop (time duration pitch pc)
  (play-midi-note (funcall *metro* time)
                  (round (qcosr pc pitch 5 .9))
                  40
                  (funcall *metro* 'dur duration)
                  1)
  (aat (funcall *metro* (+ time duration)) #'drum-loop
       (+ time duration)
       duration
       pitch
       pc))

(drum-loop (funcall *metro* 'get-beat 4) 1   40 '(0 4 7)) 
(drum-loop (funcall *metro* 'get-beat 4) .75 50 '(0 4 7 9))

(defun tempo-shift (time)
  (funcall *defmash* 'set-tempo (+ 60 (* 40 (cos (* .25 3.141592 time)))))
  (aat (funcall *defmash* (+ time .25)) #'tempo-shift (+ time .25)))

(tempo-shift (funcall *defmash* 'get-beat 1.0))

;; --

(defvar *metre* nil)
(setf *metre* (make-metre '(2 3 2) 0.5))

(defun metre-test (time)
  (if (funcall *metre* time 1.0)
      (play-midi-note (funcall *metro* time) 60 30 .4 5))
  (aat (funcall *metro* (+ time .5)) #'metre-test (+ time .5) ))

(metre-test (funcall *metro* 'get-beat 1.0))

;; ---------

(defvar *metre1* nil)
(defvar *metre2* nil)
(setf *metre1* (make-metre '(3) .5)) ;; 3/8
(setf *metre2* (make-metre '(2) .5)) ;; 2/8

(defun metre-test (time)
  (if (funcall *metre1* time 1.0)
      (play-midi-note (funcall *metro* time) 50 20 1 1))
  (if (funcall *metre2* time 1.0)
      (play-midi-note (funcall *metro* time) 60 25 1 2))
  (at (funcall *metro* (+ time .5)) #'metre-test (+ time .5)))

(metre-test (funcall *metro* 'get-beat 1.0))
;; ---------
(setf *metre1* (make-metre '(2 3 4 3 2) .5))
(setf *metre2* (make-metre '(3 5 7 5 3) .5))

(defvar *p1* nil)
(setf *p1* (cm:new cm:weighting :of `((,*gm-closed-hi-hat* :weight .8) (,*gm-open-hi-hat* :weight .2))))


(defun metre-test (time))
  (play-midi-note (funcall *metro* time) (cm:next *p1*) 10 1 1)
  (if (funcall *metre1* time 1.0)
      (play-midi-note (funcall *metro* time) 53 10 1 2))
  (if (funcall *metre2* time 1.0)
      (play-midi-note (funcall *metro* time) 70 20 1 3))
  (at (funcall *metro* (+ time .25)) #'metre-test (+ time .25) ))

(metre-test (funcall *metro* 'get-beat 1.0))

(defun metre-test (time)
  (play-midi-note (funcall *metro* time)
                  (cm:next (cm:new cm:weighting :of `((,*gm-closed-hi-hat* :weight .8)
                                                      (,*gm-open-hi-hat* :weight .2))))
                  30
                  (cm:next (cm:new cm:weighting :of '((.5 :weight .8)
                                                      (.25 :weight .2)))) 1)
  (if (funcall *metre1* time 1.0)
      (progn
        (play-midi-note (funcall *metro* time) *gm-snare* 20 .5 2)
        (play-midi-note (funcall *metro* time) *gm-pedal-hi-hat* 20 1 3)
    ))
  (if (funcall *metre2* time 1.0)
      (progn
        (play-midi-note (funcall *metro* time) *gm-kick* 10 1 4)
        (play-midi-note (funcall *metro* time) *gm-ride-bell* 20 1 5)
        ))
  (at (funcall *metro* (+ time .25)) #'metre-test (+ time .25) ))

;; -------------------------------------------------
;; Playground
;; -------------------------------------------------

(defun play-midi-note (time pitch velocity dur c)  
  (at time #'fluidsynth:noteon *synth* c pitch velocity)
  (at (+ time #[dur sa]) #'fluidsynth:noteoff *synth* c pitch))

(defvar *metro* nil)
(setf *metro* (make-metro 60))

(defun harmony (time duration pitch)
  (play-midi-note (funcall *metro* time)
                  (round (qcosr '(0 4 7) pitch 5 .3))
                  30
                  (funcall *metro* 'dur duration)
                  1)
  (aat (funcall *metro* (+ time duration)) #'harmony
       (+ time duration)
       duration
       pitch
       ))

(defun melody (time duration)
  (let ((next (funcall *metro* (+ time duration))))
    (play-midi-note (funcall *metro* time)
                    (round (qcosr '(0 4 7) 65 10 .3))
                    (if (funcall *metre2* 1.0) 25 20)
                    (funcall *metro* 'dur duration)
                    2)
    (at next
        #'melody
        (+ time duration)
        (cm:pick .75 .5))))

(harmony (funcall *metro* 'get-beat 2) 1   40) 
(melody  (funcall *metro* 'get-beat 4) .75)

;;; -----------------------------------------------
;; Tone Rows
;; http://www.algorithmiccomposer.com/2011/09/tone-rows-puredata-and-max.html
;;
;; G, Bb, D, F#, A, C, E, G#, B, C#, Eb, F

(defvar *tonerow* nil)
(defvar *notes* nil)
(defvar *pc* nil)
(setf *tonerow* '(g4 bf4 d4 fs4 a4 c4 e4 gs4 b4 cs4 ef4 f4))
(setf *notes*    (cm:keynum *tonerow*))
(setf *pc*       (mapcar #'cm:pitch-class *notes*))

;; Play a random note from the PC
(defun playme (time pc)
  (play-midi-note time (pcrrandom 60 70 pc) 40 1 0)
  (aat (+ time #[1 b]) #'playme it pc))

(playme (tempo-sync #[1 b]) *pc*)

;; Play sequentially
(setf (bpm *tempo*) 35)

(defun playme2 (time notes dur)
  (play-midi-note time (cm:next notes) 40 dur 1)
  (aat (+ time #[dur b]) #'playme2 it notes dur))

;; Phasing (ew)
(playme2 (tempo-sync #[1 b]) (cm:new cm:cycle :of *notes*) .335)
(playme2 (tempo-sync #[1 b]) (cm:new cm:cycle :of *notes*) .338)


(defun playme3 (time notes rhythms)
  (let ((dur (cm:next rhythms)))
    (play-midi-note time (cm:next notes) 40 dur 1)
    (aat (+ time #[dur b]) #'playme2 it notes rhythms)))

(playme3 (tempo-sync #[1 b])
         (cm:new cm:cycle :of *notes*)
         (cm:new cm:weighting :of (cm:rhythm '(0 0 0 0 x x t t s s e q) 30)))

;;;
;; jazz.lisp
;;;

(defun play-midi-note (time pitch velocity dur c)
  (when (> pitch 0)
    (progn
      (at time #'fluidsynth:noteon
          *synth* c pitch velocity)
      (at (+ time #[dur b]) #'fluidsynth:noteoff
          *synth* c pitch))))


(defparameter *jazz-scale* ; dorian with decorated octave
  '(0 2 3 5 7 9 10 12 14)) 

(defparameter *jazz-changes* ; key changes
  '(bf3 ef4 bf3 bf ef4 ef bf3 bf f4 ef bf3 bf))

#|
(defun jazz-high-hat (time &optional notes)
  (when (null notes)
    (setf notes (cm:next
                 (cm:new cm:cycle
                   :of (list 0 *gm-closed-hi-hat*
                             0 *gm-closed-hi-hat*))
                 't)))
  (play-midi-note time (first notes) (round (cosr 35 5 1/2)) .33 1)
  (aat (+ time #[1 b]) #'jazz-high-hat it (rest notes)))
|#

(defun jazz-high-hat (time)
  (play-midi-note time *gm-closed-hi-hat* (round (cosr 35 3 1/2)) .33 0)
  (aat (+ time #[2 b]) #'jazz-high-hat it))

(jazz-high-hat (tempo-sync #[1 b]))

(defun jazz-drums (time &optional notes rhythms amps))
  (when (null notes)
    (setf notes (cm:next
                 (cm:new cm:weighting
                   :of
                   `((0 :weight .25)
                     ,cm::+electric-snare+
                     ,cm::+acoustic-bass-drum+))
                 't)))
  (when (null rhythms)
    (setf rhythms (cm:rhythm '(t4 t8 t4 t8 t4 t8) 30)))

  (when (null amps)
    (setf amps (cm:next
                (cm:new cm:weighting
                  :of '(31 (40 :weight .1)))
                't)))

  (let ((rhythm (first rhythms)))
    (play-midi-note
     time (first notes) (first amps) rhythm 7)
    (aat (+ time #[rhythm b]) #'jazz-drums
         it (rest notes) (rest rhythms) (rest amps))))

(jazz-drums (tempo-sync #[1 b]))

;; wt
;; is weight of resting relative to playing
;; return weighting pattern that slightly prefers

;; playing a ride 1 pattern
;;    over a ride 2 pattern
(defun or12r (wt)
  (cm:new cm:weighting
    :of (list
         (list
          (cm:new cm:weighting
            :of `(51 (0 :weight ,wt))
            :for 1) :weight 1.5)
          (cm:new cm:weighting
            :of `(59 (0 :weight ,wt))
            :for 1))
    :for 1))

(defun lamps (amps)
  (mapcar
   #'round
   (mapcar
    (lambda (x) (cm:interp x 0.0 10 1.0 40))
    (cm:amplitude amps))))

;;  1  -  x    1  -  1    1  x  x    1  x  1
(defun jazz-cymbals (time rhythm &optional notes amps)
  (when (null notes)
    (setf notes (cm:next (cm:new cm:cycle :of (list
                 cm::+ride-cymbal-1+ 0 (or12r 5)
                 cm::+ride-cymbal-1+ 0 cm::+ride-cymbal-1+
                 cm::+ride-cymbal-1+ (or12r 7) (or12r 7)
                 cm::+ride-cymbal-1+ (or12r 3) cm::+ride-cymbal-1+)) 't)))
  (when (null amps)
    (setf amps (lamps '(:mf :mp :fff :f
                        :mp :ffff :mf :mp
                        :fff :f :mp :ffff))))
  (play-midi-note time (first notes) (first amps) rhythm 8)
  (aat (+ time #[rhythm b]) #'jazz-cymbals
       it rhythm (rest notes) (rest amps)))

(fluidsynth:program-change *synth* 8 1)
(jazz-cymbals (tempo-sync #[1 b]) (cm:rhythm 't8 60))

;;;
;; <3
;;;

(defun jazz-piano (time &optional notes rhythms root amps)

  (when (null notes)
    (setf root (alexandria:random-elt *jazz-changes*))
    (setf notes (cm:next
                 (cm:new cm:weighting
                   :of `(0 (,(cm:new cm:heap
                               :of *jazz-scale*
                               :for (cm:new cm:weighting
                                      :of '(1 2 3 4)))
                             :weight ,(cm:new cm:weighting
                                        :of '(1.15 1.65)))))
                 't)))

  (when (null rhythms)
    (if (null (cm:odds .3))
        (setf rhythms (repeat 8 '(1)))
;;        (setf rhythms (cm:rhythm (repeat 8 '(t8)) 60))
        (setf rhythms (cm:rhythm '(t4 t8 t4 t8 t4 t8 t4 t8) 60))))
  
  (let ((rhythm (first rhythms)))
    (play-midi-note
     time (cm:keynum
           (cm:transpose (first notes) root)) (round (cosr 26 3 3/4)) rhythm 1)
    (aat (+ time #[rhythm b])
         #'jazz-piano
         it (rest notes) (rest rhythms) root)))

(jazz-high-hat (tempo-sync #[1 b]))
(jazz-cymbals  (tempo-sync #[1 b]) (cm:rhythm 't8 60))
(jazz-piano    (tempo-sync #[1 b]))

(fluidsynth:program-change *synth* 1 48)
