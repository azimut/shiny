(in-package :somecepl)

;; https://sourceforge.net/p/incudine/mailman/message/31182999/

;; small sample-player example for incudine. For this to work you'll
;; need to adjust the path to *ssdir* and put the Bosen_mf_... samples
;; into *ssdir*.
;;
;; This file is in the public domain. Use at own risk. No guarantees
;; whatsoever.

;;(defparameter *ssdir* #.(or *compile-file-pathname* *load-pathname*))
(defparameter *ssdir* "/home/sendai/Downloads/morepiano/piano-sampler-incudine/")
(defparameter *ssdir-f* "/home/sendai/Downloads/samples/pianosample/SalamanderGrandPianoV2_44.1khz16bit/44.1khz16bit/")

(setf *ssdir* "/home/sendai/Downloads/morepiano/piano-sampler-incudine/")
(setf *ssdir-f* "/home/sendai/Downloads/samples/pianosample/SalamanderGrandPianoV2_44.1khz16bit/44.1khz16bit/")

(define-vug buffer-loop-play ((buffer buffer)
                              rate
                              start-pos
                              loopstart
                              loopend)
  (buffer-read buffer (phasor-loop rate start-pos loopstart loopend)
               :interpolation :cubic))

(defstruct lsample
  "structure for a sample with two loop-points. The structure also
contains a slot for the sample buffer data."
  file
  buffer
  (keynum +sample-zero+ :type incudine.util:sample)
  (loopstart +sample-zero+ :type incudine.util:sample)
  (loopend +sample-zero+ :type incudine.util:sample))

(defun generate-lsample-map (lsample-defs &key (ssdir *ssdir*)) 
  "map a list of sample defs of the form (filename keymap-upper-bound
original-keynum loopstart loopend) into a freshly allocated lookup
array containing an lsample-struct entry for 127 keynums. The buffer
slots of the lsamples are filled with the sample data. The array is
returned."
  (let ((lsample-map (make-array 128 :adjustable nil :element-type 'lsample)))
    (loop
       for lsample-def in 
	 (sort lsample-defs #'(lambda (x y) (< (second x) (second y))))
       with array-idx = -1
       do (destructuring-bind
		(file keymap-upper-bound keynum loopstart loopend) 
	      lsample-def
	    (let ((lsample ;;; make lsample from lsample-def and fill slots
		   (let ((full-name (merge-pathnames file ssdir)))
		     (make-lsample :file full-name
				   :buffer (buffer-load full-name)
				   :keynum (incudine.util:sample keynum)
				   :loopstart (incudine.util:sample loopstart)
				   :loopend (incudine.util:sample loopend)))))
	      (do () ;;; fill array with references to the lsample
		  ((>= array-idx keymap-upper-bound))
		(setf (aref lsample-map (incf array-idx)) lsample)))))
    (values lsample-map)))

(defparameter *piano-map*
  (generate-lsample-map 
   '(("BOSEN_mf_A0_mn.wav"  36 33 121642 197454)
     ("BOSEN_mf_D1_mn.wav"  43 38  93488 166611)
     ("BOSEN_mf_C2_mn.wav"  51 48  95464 151802)
     ("BOSEN_mf_F2_mn.wav"  58 53  77922 137052)
     ("BOSEN_mf_D#3_mn.wav" 68 63  82246 132839)
     ("BOSEN_mf_C#4_mn.wav" 76 73  64379 104238)
     ("BOSEN_mf_F#4_mn.wav" 81 78  57945  59609)
     ("BOSEN_mf_B4_mn.wav" 127 83  48970  50748)) :ssdir *ssdir*))

(defparameter *piano-map-f*
  (generate-lsample-map
   '(("A0v10.wav"  36 33 121642 197454) ;k
     ("D#1v10.wav"  43 38  93488 166611) ;?
     ("C2v10.wav"  51 48  95464 151802) ;k
     ("F#2v10.wav"  58 53  77922 137052) ;?
     ("D#3v10.wav" 68 63  82246 132839) ;k
     ("C4v10.wav" 76 73  64379 104238) ;?
     ("F#4v10.wav" 81 78  57945  59609) ;k
     ("C5v10.wav" 127 83  48970  50748)) :ssdir *ssdir-f*)) ;?
(defparameter *piano-map-f*
  (generate-lsample-map
   '(("A0v10.wav"  21 21 100000 500000) ;k
     ("A2v10.wav" 45 45 100000 500000 )
     ("A2v10.wav" 45 45 100000 500000 )
     ("A4v10.wav" 69 69 100000 500000 )
     ("A6v10.wav" 93 93 100000 500000 )
     ("C1v10.wav" 24 24 100000 500000 )
     ("C3v10.wav" 48 48 100000 500000 )
     ("C5v10.wav" 72 72 100000 500000 )
     ("C7v10.wav" 96 96 100000 500000 )
     ("D#1v10.wav" 27 27 100000 500000 )
     ("D#3v10.wav" 51 51 100000 500000 )
     ("D#5v10.wav" 75 75 100000 500000 )
     ("D#7v10.wav" 99 99 100000 500000 )
     ("F#2v10.wav" 42 42 100000 500000 )
     ("F#4v10.wav" 66 66 100000 500000 )
     ("F#6v10.wav" 90 90 100000 500000 )
     ("A1v10.wav" 33 33 100000 500000 )
     ("A3v10.wav" 57 57 100000 500000 )
     ("A5v10.wav" 81 81 100000 500000 )
     ("A7v10.wav" 105 105 100000 500000 )
     ("C2v10.wav" 36 36 100000 500000 )
     ("C4v10.wav" 60 60 100000 500000 )
     ("C6v10.wav" 84 84 100000 500000 )
     ("C8v10.wav" 108 108 100000 500000 )
     ("D#2v10.wav" 39 39 100000 500000 )
     ("D#4v10.wav" 63 63 100000 500000 )
     ("D#6v10.wav" 87 87 100000 500000 )
     ("F#1v10.wav" 30 30 100000 500000 )
     ("F#3v10.wav" 54 54 100000 500000 )
     ("F#5v10.wav" 78 78 100000 500000 )
     ("F#7v10.wav" 102 102 100000 500000 )) :ssdir *ssdir-f*)) ;?

(declaim (inline get-lsample))
(defun get-lsample (keynum map)
  (aref map (min keynum 127)))
;; example: (get-lsample 61 *piano-map*)
;; -> #S(LSAMPLE
;;       :FILE "/home/orm/work/snd/csound/BOSEN_mf_D#3_mn.wav"
;       :BUFFER #<BUFFER :FRAMES 132917 :CHANNELS 1 :SR 44100.0>
;;       :KEYNUM 63
;;       :LOOPSTART 82246
;;       :LOOPEND 132839)

(declaim (inline ct->fv))
(defun ct->fv (steps)
  "halfsteps to ratio"
  (expt #.(coerce 2.0 'incudine.util:sample)
        (* steps #.(coerce 1/12 'incudine.util:sample))))

(defparameter *env1* (make-envelope '(0 1 1 0) '(0 .9 .1)))

(dsp! play-lsample (keynum dur amp)
  (with ((lsample (get-lsample (sample->fixnum keynum) *piano-map*)))
;  (with ((lsample (get-lsample keynum *piano-map*)))
    (with-samples ((rate (ct->fv (- keynum (lsample-keynum lsample))))
                   (loopstart (lsample-loopstart lsample))
                   (loopend (lsample-loopend lsample)))
      (with ((buffer (lsample-buffer lsample)))
        (stereo (* amp 
		(envelope *env1* 1 dur #'incudine:stop)
		(buffer-loop-play buffer rate 0 loopstart loopend)))))))

(dsp! play-lsample-f (keynum dur amp)
  (with ((lsample (get-lsample (sample->fixnum keynum) *piano-map-f*)))
;  (with ((lsample (get-lsample keynum *piano-map-f*)))
    (with-samples ((rate      (ct->fv (- keynum (lsample-keynum lsample))))
                   (loopstart (lsample-loopstart lsample))
                   (loopend   (lsample-loopend lsample)))
      (with ((buffer (lsample-buffer lsample)))
        (stereo (* amp 
		(envelope *env1* 1 dur #'incudine:stop)
		(buffer-loop-play buffer rate 0 loopstart loopend)))))))

;; examples: 
#|
 (play-lsample (+ 50 (random 30)) 3 0.6)
 (play-lsample-f (+ 50 (random 30)) 3 .6)

 (play-lsample 60 3 .6)
 (play-lsample-f 60 3 .6)

 (loop
   for x from 1 to 200
   for time = (now) then (+ time (* *sample-rate* (random 0.05)))
   for amp = (+ 0.2 (random 0.2)) then (+ 0.2 (random 0.2))
   do (at time #'play-lsample (+ 70 (random 20.0)) 2 amp))

bouncing to disk:

 (bounce-to-disk ("/tmp/test.wav" :pad 2)
  (loop     for x from 1 to 200
     for time = (now) then (+ time (* *sample-rate* (random 0.05)))
     for amp = (+ 0.2 (random 0.2)) then (+ 0.2 (random 0.2))
     do (at time #'play-lsample (+ 70 (random 20.0)) 2 amp)))
|#


(defvar env1 (make-envelope '(0 1 0) '(.2 .8)))
(defvar env2 (make-perc .01 .9))
(setf (bpm *tempo*) 120)

;; trying "markov chains" from extempore demo

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

(defun beatme (root)
  (play-lsample-f root .35 .3)
  (at (+ (now) #[2 b]) #'beatme 60))

#|
(beatme 60)
|#

;; trying "markov chains" from extempore demo
(defun seq-test (&optional (root 60))
  (let* ((newroot (random-list (cdr (assoc root '((60 58 55)
                                                  (58 60 56)
                                                  (56 55 58)
                                                  (55 60 56)))))))
     (play-lsample-f (+ root -12) .5 .25)
     (at (+ (now) #[3/2 b]) #'play-lsample-f (+ -5  root) .5 .3)
     (at (+ (now) #[4 b]) #'seq-test newroot)))

;; <60> = 60  beats per minute
;; <60>/60 = 1 = beats por sec 
;; <125> = 125 beats per minute
;; <125>/60 = 2.08 beats per second
(defun left (&optional (root 60))
  (let* ((newroot (random-list (cdr (assoc root '((60 58 55)
                                                  (58 60 56)
                                                  (56 55 58)
                                                  (55 60 56)))))))
     (play-lsample-f (+ root -12) 1.9 .25)
     (at (+ (now) #[3/2 b]) #'play-lsample-f (+ -5  root) 1.9 .27)
     (at (+ (now) #[4 b]) #'left newroot)))

(defun right (&optional (oldtime 0) (newtime 0))
  ;(setf newtime (get-internal-real-time))
  ;(print (- newtime oldtime))
  ;(print (cosr 60 7 3/2))
  ;(print (floor (cosr 60 7 3/2)))
  ;(print (get-internal-real-time))
  (print (cosr 60 7 3/2))
  (play-lsample-f (floor (cosr 60 7 3/4)) 1. .2)
  (at (+ (now) #[.5 b]) #'right newtime oldtime)
  )

(defvar *myscale* nil)
(setf *myscale* '(0 2 3 5 7 8 10) )
(setf *myscale* '(0 1 3 5 6 8 10) )
(defun right ()
  (play-lsample-f (qcosr *myscale* 60 7 3/4) 1. .2)
  (at (+ (now) #[.5 b]) #'right)
  )

(defun right ()
  (play-lsample-f (random-list '(53 55 60 65 67)) 1.1 .2)
  (at (+ (now) #[.5 b]) #'right)
  )

(defvar *tun*
  (make-tuning :notes '(16/15 9/8 6/5 5/4 4/3 7/5 3/2 8/5 5/3 9/5 15/8 2/1)
               :description "Basic JI with 7-limit tritone. Robert Rich: Geometry"))
(defun right ()
  (play-lsample-f (random-list (subseq (buffer->list *tun*) 30 37)) 2. .1)
  (at (+ (now) #[.5 b]) #'right)
  )


(defvar *root* 0)
(defvar *degree* 'i)

(defun pchord ()
  (play-lsample-f 60 1. .8)
  (play-lsample-f 64 1. .8)
  (play-lsample-f 67 1. .8)
  )

(pchord)
(defun dancingl (dur)
  (play-lsample-f 36
                  (* dur (cosr .6 .3 1/7))
                  (cosr .6 .3 1/7))
  (aat (+ (now) #[2 b]) #'dancingl (random-list '(1 2/3))))

(dancingl 4)

#|
(left)
(incudine:free 0)
(flush-pending)
(right)
(dancingl 2/3)
|#

#|
src/overtone/examples/compositions/piano_phase.clj

(defn player
  [t speed notes]
  (let [n      (first notes)
        notes  (next notes)
        t-next (+ t speed)]
    (when n
      (at t
        (sampled-piano (note n)))
      (apply-by t-next #'player [t-next speed notes]))))
|#
(defvar *piece* nil)
(setf *piece* '(:E4 :F#4 :B4 :C#5 :D5 :F#4 :E4 :C#5 :B4 :F#4 :D5 :C#5))
(setf (bpm *tempo*) 100)

(defun player (time speed notes)
  (let ((n      (first notes))
        (notes  (cdr notes))
        (t-next (+ time speed)))
    (when n
      (play-lsample-f (note-name-to-midi-number (symbol-name n)) 5.0 .9)
      (at t-next #'player t-next speed notes))))

(progn
  (player (now) #[.338 b] (repeat 1000 *piece*))
  (player (now) #[.335 b] (repeat 1000 *piece*)))

;; --------------

#|
Extempore - An Overview
https://vimeo.com/21956071
at 11:30
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

(defvar *beat-offset* nil)
(setf *beat-offset* '(1/3 1 3/2 1 2 3))
(setf (bpm *tempo*) 100)

(defun sometune (dur root)
  ;(at (+ (now) #[3 b]) #'play-lsample-f 36 5.0 1.0)
  (mapcar (lambda (x y)
            (at (+ (now) #[y b]) #'play-lsample-f x 10.0 1.0))
          (make-chord 40
                      (cosr 75 10 1/32)
                      5
                      (chord root (if (member root '(10 8))
                                      '^7
                                      '-7)))
          *beat-offset*)
  (at (+ (now) #[dur b]) #'sometune
      dur
      (if (member root '(0 8))
          (random-list '(2 7 10))
          (random-list '(0 8)))))

(setf *beat-offset* (reverse *beat-offset*))

(setf *beat-offset* '(0 0.1 1/3 0.7 0.9 0.9))
(setf *beat-offset* '(0 0.2 1/3 0.5 0.8))
(setf *beat-offset* '(0 0.2 0.4 0.6 0.8))
(setf *beat-offset* '(0 0.1 0.2 0.3 0.4))
(setf *beat-offset* '(0 0.1 0.11 0.13 0.15 0.17 0.2 0.4 0.5 0.55 0.6 0.8))

(sometune 4 0)

#|
(incudine:free 0)
(flush-pending)
|#

#|
;; Ben S.
;; A late christmas
(define loop1
    (lambda (beat dur)
      (play piano 60 50 dur)
      (callback (*metro* (+ beat (* .5 dur))) 'loop1 (+ beat dur) dur)))
|#
(defun loop1 (beat dur)
  (play-lsample-f 60 dur .4)
  (print beat)
  (print dur)
  (at (+ (now)
         (+ #[beat b] (* .5 #[dur b]))) #'loop1 (+ beat dur) dur))

(loop1 3 2)

#|
https://gist.github.com/CircuV/0834dbbd2034b82b7706

(define chords
  (lambda (time degree dur)
    (println time degree dur)
    (if (member degree '(i)) (set! dur 3))
    (for-each 
     (lambda (p)    
     (play-midi-note 
      (*metro* time)
      *midi-out*
      p
      (real->integer (+ 50 (* 20 (cos (* pi time)))))
      (*metro* 'dur dur)
      0))

    (pc:make-chord 50 70 2 (pc:diatonic 0 '- degree))
    )
    (callback (*metro* (+ time (* 0.5 dur))) 'chords (+ time dur)
              (random (assoc degree '((i vii)
                                      (vii i))))
              dur)))
|#

(defvar *myscale* nil)
(setf *myscale* (scale 0 'aeolian))
(setf (bpm *tempo*) 100)

(defun chords (degree dur)
  (if (member degree '(i)) (setf dur 3))
  (mapcar
   (lambda (p) (play-lsample-f p 3.0 .4))
   (make-chord 50 70 2 (diatonic 0 '- degree)))
  (at (+ (now) #[dur b]) #'chords
      (random-list (assoc degree '((i vii)
                                   (vii i))))
      dur))

(chords 'i 3)

#|

(define chords
  (lambda (time degree dur)
    (if (member degree '(i)) (set! dur 3.0))
    (println time degree dur)
    (for-each (lambda (p)
    (let* ((dur1 (* dur (random '(0.5 1))))
           (dur2 (- dur dur1)))
    (play-midi-note (*metro* time) *midi-out* p 
          (real->integer(+ 50 (* 20 (cos (* pi time)))))
          (*metro* 'dur dur1) 0)
    (if (> dur2 0)
      (play-midi-note (*metro* (+ time dur1)) *midi-out*
          (pc:relative p (random '(-2 -1 1 2))
                       (pc:scale 0 'aeolian))  
          (real->integer (+ 50 (* 20 (cos (* pi (+ time dur1))))))
          (*metro* 'dur dur2) 0 ))))

    (pc:make-chord 50 70 2 (pc:diatonic 0 '- degree)))
    (callback (*metro* (+ time (* 0.5 dur))) 'chords (+ time dur)
              (random (assoc degree '((i vii)
                                      (vii i))))
              (random (list 1 2 3)))))

|#
(defun chords (degree dur)
;  (if (member degree '(i)) (setf dur 3.0))
  (if (member degree '(i)) (setf dur (random-list '(3.0 6.0))))
  ; 4:00
  (let* ((nnote (car (diatonic 0 '- degree))))
    ;; Hack needed when degree returns 2 , which is converted by pcrrandom in -1
    (if (= nnote 2) (setf nnote (cadr (diatonic 0 '- degree))))
    (play-lsample-f (pcrrandom 40 50 (list nnote))
                  dur .4))
  (mapcar
   (lambda (p) (let* ((dur1 (* dur (random-list '(0.5 1))))
                      (dur2 (- dur dur1)))
                 (play-lsample-f p dur1 .4)
                 (if (> dur2 0)
                     (at (+ (now) #[dur1 b]) #'play-lsample-f
                         (relative p (random-list '(-2 -1 1 2)) (scale 0 'aeolian))
                         dur1
                         .4))))
 ;   (make-chord 50 70 2 (diatonic 0 '- degree)))
  (make-chord 50 70 3 (diatonic 0 '- degree)))
  (at (+ (now) #[dur b]) #'chords
;      (random-list (assoc degree '((i vii)
;                                   (vii i))))
      ;; (random-list (assoc degree '((i vii)
      ;;                              (vii i v)
      ;;                              (v i))))
      (random-list (assoc degree '((i vii)
                                   (vii i v)
                                   (v i vi)
                                   (vi ii)
                                   (ii v vii))))
      (random-list '(1 2 3))))



;; ----------------------
;; DANCING PHALANGES
(setf (bpm *tempo*) 110)
(defvar *root* nil)
(setf *root* 0)
(defvar *degree* nil)
(setf *degree* 'i)
(defvar *scale* nil)
(setf *scale* (scale 0 'phrygian))

(defun melody (ms rs)
  (if (not ms)
      'done
      (let ((p (car ms)))
        (play-lsample-f (quant (+ 48 *root* p) *scale*)
                        (* (car rs) (cosr .9 .2 1/2)) .9)
        (play-lsample-f (quant (+ 55 *root* p) *scale*)
                        (* (car rs) (cosr .9 .2 1/2)) .9)
        (at (+ (now) #[(car rs) b]) #'melody (cdr ms) (cdr rs))
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

(defun left (dur)
  (if (> (random 1.0) .85) (melody '(0 1 0 -1) '(1/3 2/3 2/3 1)))
  (play-lsample-f (+ 48 *root*) (* dur (cosr .9 .3 1/7)) (cosr .6 .3 1/2))
  (play-lsample-f 36 (* dur (cosr .9 .3 1/7)) (cosr .6 .3 1/2))
  (at (+ (now) #[dur b]) #'left (random-list '(1 2/3))))

(left 2/3)
(flush-pending)
