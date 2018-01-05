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
;(fluidsynth:sfload *synth* "/home/sendai/Downloads/Sonatina_Symphonic_Orchestra.sf2" 1)
(fluidsynth:sfload *synth* "/home/sendai/Downloads/Nice-Keys-Ultimate-V2.3.sf2" 1)

#|
(fluidsynth:get-active-voice-count *synth*)
(fluidsynth:stop *synth* 1)
(incudine:free 0)
(fluidsynth:delete *synth*)
|#

(fluidsynth:set-reverb *synth* 0.7d0 0.9d0 0.5d0 0.9d0)
(set-rt-block-size 64)

(setf (fluidsynth:setting *fluid-settings* "synth.gain") .9)
(setf (fluidsynth:setting *fluid-settings* "synth.polyphony") 128)
(setf (fluidsynth:setting *fluid-settings* "synth.midi-channels") 24)

(rt-stop)
(rt-start)
(fluid-test *synth*)
;;(fluidsynth:program-change *synth* 24 24)
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

(play-midi-note (now) 60 60 5 0)
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

;; ----------

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
(defvar *notes* nil)
(setf *notes* '(:c3 :g3 :d3))) ; 48 55 50
(setf (bpm *tempo*) 80)

(defun eu (c rythm note)
  (if (= 1 (first rythm))
      (play-midi-note (now) note 70 1 c))
  (at (tempo-sync #[1 b]) #'eu c (alexandria:rotate rythm 1) note)
)

(flush-pending)

(eu 0 (bjorklund 3 8)  (note-name-to-midi-number (symbol-name (nth 0 *notes*))))
(eu 1 (bjorklund 4 4)  (note-name-to-midi-number (symbol-name (nth 1 *notes*))))
(eu 2 (bjorklund 5 13) (note-name-to-midi-number (symbol-name (nth 2 *notes*))))


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
