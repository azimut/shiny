(in-package :somecepl)


(defun play-midi-note (time pitch velocity dur c)  
  (at time #'fluidsynth:noteon *synth* c pitch velocity)
  (at (+ time #[dur b]) #'fluidsynth:noteoff *synth* c pitch))

(defun play-midi-note (time pitch velocity dur c)
  (when (and
         (not (equal pitch "r"))
         (> pitch 0)
         (> velocity 0)
         (> dur 0))
    (progn
      (at time #'fluidsynth:noteon
          *synth* c pitch velocity)
      (at (+ time #[dur b]) #'fluidsynth:noteoff
          *synth* c pitch))))

(defun play-midi-note (time pitch velocity dur c)
  (when (and
         (not (equal pitch "r"))
         (> pitch 0)
         (> velocity 0)
         (> dur 0))
    (progn
      (at time #'fluidsynth:noteon
          *synth* c pitch velocity)
      (at (+ time #[dur sa]) #'fluidsynth:noteoff
          *synth* c pitch))))

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

(defparameter *piece* '(:E4 :F#4 :B4 :C#5 :D5 :F#4 :E4 :C#5 :B4 :F#4 :D5 :C#5))

(repeat 1000 *piece*)

;;(setf (bpm *tempo*) 35)

(defun player ())
(defun player (time speed notes c)
  (let ((note   (first notes))
        (notes  (cdr notes))) 
    (when note
      (p time
         (note-name-to-midi-number (symbol-name note))
         ;;(rcosr 60 5 5)
         55
         (+ -.1 speed)
         c)
      (aat (+ time speed) #'player it speed notes c))))

#|
(player (now) #[.335 b] *piece* 0)
(flush-pending)
|#
(fp 3 108)
(let ((time (quant 4)))
  (player time .338 *piece* 3)
  (player time .335 *piece* 3))

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


;; -----------
;; Playground
;; https://digego.github.io/extempore/note-level-music.html
;; http://impromptu.moso.com.au/tutorials/making_music/
;; -----------
(setf (fluidsynth:setting *fluid-settings* "synth.gain") 1.)

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
            (play-midi-note (+ (now) #[delay b]) note vel 2 c))
          (mapcar (lambda (x) (+ 60 x)) (1-over-f repeats))
          (range (round (/ repeats 2)) :min 1 :step .5)
          (repeat repeats '(60))
          (repeat repeats '(0))))

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


;; All piano
(dotimes (i 32) (fluidsynth:program-change *synth* i 1) )

(fluidsynth:program-change *synth* 4 34)
(fluidsynth:program-change *synth* 2 1)
(fluidsynth:program-change *synth* 3 42)

(fluidsynth:program-change *synth* 4 33)
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
  (let ((note   (cm:keynum (next keys)))
        (rhythm (cm:rhythm (next rhythms) 30)))
    (p time
       note
       40 rhythm chan)
    (aat (+ time rhythm) #'pp chan it keys rhythms)))

(defun pp ())
(defun ppp ())

(pp 1
    (quant 4)
    (make-cycle '(e3 gs4 b4 ds4))
    (make-cycle '(e s q e. s.)))

(pp 2
    (quant 4)
    (make-cycle '(e4 gs4 b4 ds4))
    (make-cycle '(s e s. e. q)))

#|
(flush-pending)
(off-with-the-notes)
|#

(defun p+ ())
(defun p+ (chan vel time keys rhythms amps &key life)
  (if (or (null life)
          (> life 0))
      (let* ((amp    (cm:next amps))
             (note   (if (= amp 1) (cm:keynum (cm:next keys)) 0))
             (rhythm (cm:rhythm (cm:next rhythms) 30))
             (life   (if (integerp life) (1- life) nil)))
        (if (not (= note 0))
            (p time
               (cm:keynum note)
               vel rhythm chan ))
        (aat (+ time rhythm) #'p+ chan vel it keys rhythms amps
             :life life))))

(defun q1 (chan vel time keys &key (life nil))
  (let ((rhythms (make-cycle '(s s s s s s s s+q q)))
        (amps    (make-cycle '(0 1 0 1 0 1 0 1   0))))
    (p+ chan vel time keys rhythms amps :life life)))

(defun q2 (chan vel time keys &key (life nil))
  (let ((rhythms (make-cycle '(e e e e+q q)))
        (amps    (make-cycle '(1 1 1 1 0))))
    (p+ chan vel time keys rhythms amps :life life)))

(defun q3 (chan vel time keys &key (life nil))
  (let ((rhythms (make-cycle '(s q e.+q q)))
        (amps    (make-cycle '(1 1 1 0))))
    (p+ chan vel time keys rhythms amps :life life)))

(defun q4 (chan vel time keys &key (life nil) )
  (let ((rhythms (make-cycle '(e s s s s s s+q q)))
        (amps    (make-cycle '(1 1 1 1 1 1 1 0))))
    (p+ chan vel time keys rhythms amps :life life)))

(q1 1 50 (quant 4) (make-heap '(gs4 b4 a4 fs4)))
(q2 2 50 (quant 4) (make-heap '(gs4 b4 a4 fs4)))
(q3 3 50 (quant 4) (make-heap '(e4 fs5)))
(q4 4 55 (quant 4) (make-heap '(fs3)))

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
    (quant 4)
    (cm:new cm:cycle :of '(e3 gs4 b4 ds4)))

(p+ 4 30 (tempo-sync #[1 b])
   (cm:new cm:cycle :of '(e3 gs4 b4 ds4))
   (cm:new cm:cycle :of '(s e s. e. q))
   (cm:new cm:cycle :of '(1)))

;; emergen.scm - launchOB

(all-piano 0)

(progn
  (q1 1 30 (quant 4)
      (make-heap '(gs4 b4 a4 fs4)) :life 10)
  (q2 2 30 (quant 8)
      (make-heap '(gs4 b4 a4 fs4)) :life 10)
  (q3 4 35 (quant 12)
      (make-heap '(e4 fs5)) :life 10)
  (q4 4 45 (quant 16)
      (make-heap '(fs3)) :life 10) )

(defun q5 (rhythms)
  (let* ((rhythm (cm:rhythm (next rhythms) 30)))
    (print "hey")
    (q1 1 30 (quant 4)
        (make-heap '(gs4 b4 a4 fs4)) :life 10)
    (q2 2 30 (quant 4)
        (make-heap '(gs4 b4 a4 fs4)) :life 10)
    (q3 4 35 (quant 4)
        (make-heap '(e4 fs5)) :life 10)
    (q4 4 45 (quant 4)
        (make-heap '(fs3)) :life 10)
    (aat (+ (now) (* 10 rhythm)) #'q5 rhythms)))

(defun q5 ())

(fg .4)
(all-piano 0)

(q5 (make-cycle '(e s s s s s s+q q)))
(q5 (make-cycle '(w w+h w+w w+w+w)))
    
(q1 1 30 (quant 4) (make-heap '(gs4 b4 as4 fs4)))

#|
(flush-pending)
(off-with-the-notes)
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
      (p (now) (cm:keynum k) vel 1 (random 10)))
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
    (p time (cm:between 50 70) 30 1 10)
    (p time (cm:between 50 70) 30 1 20)
    (p time (cm:between 50 70) 30 1 30)
    (aat (tempo-sync #[rhythm b]) #'q it rhythms)))

(q (tempo-sync #[1 b]) (cm:new cm:cycle :of '(e q s s q)))


;; Data set of Bach
;; https://archive.ics.uci.edu/ml/datasets/Bach+Chorales



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

(defun pw ())
(defun pw (chan time notes amps)
  (let ((amp  (next amps)))
    (if (= amp 1)
        (p time (next notes) 40 1 chan))
    (aat (+ time .5) #'pw chan it notes amps)))

; clav  - 19
; brass - 40
(fluidsynth:program-change *synth* 1 16)
(fluidsynth:program-change *synth* 2 41)

(defvar *chord* nil)
(setf *chord* (make-chord 50 60 3 (scale 0 'aeolian)))
(defvar *cycle* nil)
(setf *cycle* (cm:new cm:cycle :of *chord*))

(pw 1
    (quant 4)
;;        (tempo-sync #[4 b])
;;    (funcall *metro* (funcall *metro* 'get-beat 4.0))
    (cm:new cm:cycle :of *chord*)
    (cm:new cm:cycle :of (rwgen mtrules '(1 0) 2)))
 
(pw 2
    (tempo-sync #[2.5 b])
;;    (funcall *metro* (funcall *metro* 'get-beat 2.5))
    (cm:new cm:cycle :of *chord*)
    (cm:new cm:cycle :of (rwgen mtrules '(1 0) 4)))

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
    (cm:new cm:cycle :of (rwgen mtrules '(1 0) 4)))

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
    (cm:new cm:cycle :of (rwgen mtrules '(1 0) 4)))



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
  (play-midi-note time (pc-random 60 70 pc) 40 1 0)
  (aat (+ time #[1 b]) #'playme it pc))

(playme (tempo-sync #[1 b]) *pc*)

;; Play sequentially
(setf (bpm *tempo*) 35)

(defun playme2 (time notes dur)
  (play-midi-note time (cm:next notes) 60 dur 1)
  (aat (+ time #[dur b]) #'playme2 it notes dur))

;; Phasing (ew)
(playme2 (tempo-sync #[1 b]) (cm:new cm:cycle :of *notes*) .335)
(playme2 (tempo-sync #[1 b]) (cm:new cm:cycle :of *notes*) .338)


(defun playme3 (time notes rhythms)
  (let ((dur  (cm:next rhythms))
        (chan (random 10)))
    (play-midi-note time (cm:next notes) 40 dur chan)
    (aat (+ time #[dur b]) #'playme3 it notes rhythms)))

(playme3 (tempo-sync #[1 b])
         (cm:new cm:cycle :of *notes*)
         (cm:new cm:heap :of (cm:rhythm '(0 0 0 0 x x t t s s e q) 30)))

(fluidsynth:program-change *synth* 1 52)
(fluidsynth:program-change *synth* 0 53)


;; ----------------------------

(defun hithat (time &optional (ry 1))
  (play-midi-note time 40 30 3 5)
  (aat (+ time #[ry b])
       #'hithat it 4))

(hithat (funcall *metro* (funcall *metro* 'get-beat 4)))
(fluidsynth:program-change *synth* 5 1)

;; -------------------------
