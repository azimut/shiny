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
;;                  (abs (round (+ 1 (* 40 (cos (* 3.141592 0.0315 (get-internal-real-time)))))))
;;                  (abs (round (+ 1 (* 40 (cos (* 3.141592 0.0315 (get-universal-time)))))))
                  (round (cosr 30 10 .6))
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
(flush-pending)


;;; -------------------------
;; Extempore tutorial - Beats
;;; -------------------------

(fluidsynth:program-change *synth* 2 1)

(defun drum-loop2 (time dur)
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

(funcall *metro* 'set-tempo 40)

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
      (play-midi-note (funcall *metro* time) 50 30 1 1))
  (if (funcall *metre2* time 1.0)
      (play-midi-note (funcall *metro* time) 60 35 1 2))
  (at (funcall *metro* (+ time .5)) #'metre-test (+ time .5)))

(metre-test (funcall *metro* 'get-beat 1.0))
;; ---------
(setf *metre1* (make-metre '(2 3 4 3 2) .5))
(setf *metre2* (make-metre '(3 5 7 5 3) .5))

(defvar *p1* nil)
(setf *p1* (cm:new cm:weighting :of `((,*gm-closed-hi-hat* :weight .8) (,*gm-open-hi-hat* :weight .2))))


(defun metre-test (time)
  (play-midi-note (funcall *metro* time) (cm:next *p1*) 30 1 1)
  (if (funcall *metre1* time 1.0)
      (play-midi-note (funcall *metro* time) 53 40 1 2))
  (if (funcall *metre2* time 1.0)
      (play-midi-note (funcall *metro* time) 70 30 1 3))
  (at (funcall *metro* (+ time .25)) #'metre-test (+ time .25) ))

(metre-test (funcall *metro* 'get-beat 1.0))

(defun metre-test (time)
  (play-midi-note (funcall *metro* time)
                  (cm:next (cm:new cm:weighting :of `((,*gm-closed-hi-hat* :weight .8)
                                                      (,*gm-open-hi-hat* :weight .2))))
                  40
                  (cm:next (cm:new cm:weighting :of '((.5 :weight .8)
                                                      (.25 :weight .2)))) 1)
  (if (funcall *metre1* time 1.0)
      (progn
        (play-midi-note (funcall *metro* time) *gm-snare* 20 .5 2)
        (play-midi-note (funcall *metro* time) *gm-pedal-hi-hat* 20 1 3)
    ))
  (if (funcall *metre2* time 1.0)
      (progn
        (play-midi-note (funcall *metro* time) *gm-kick* 20 1 4)
        (play-midi-note (funcall *metro* time) *gm-ride-bell* 30 1 5)
        ))
  (at (funcall *metro* (+ time .25)) #'metre-test (+ time .25) ))
