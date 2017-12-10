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

(define-vug buffer-loop-play ((buffer buffer) rate start-pos
                              loopstart loopend)
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
  (aref map (min (round keynum) 127)))

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
  (with ((lsample (get-lsample keynum *piano-map*)))
    (with-samples ((rate (ct->fv (- keynum (lsample-keynum lsample))))
                   (loopstart (lsample-loopstart lsample))
                   (loopend (lsample-loopend lsample)))
      (with ((buffer (lsample-buffer lsample)))
        (out (* amp 
		(envelope *env1* 1 dur #'incudine:stop)
		(buffer-loop-play buffer rate 0 loopstart loopend)))))))

(dsp! play-lsample-f (keynum dur amp)
  (with ((lsample (get-lsample keynum *piano-map-f*)))
    (with-samples ((rate (ct->fv (- keynum (lsample-keynum lsample))))
                   (loopstart (lsample-loopstart lsample))
                   (loopend (lsample-loopend lsample)))
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
     (play-lsample-f (+ root -12) .6 .25)
     (at (+ (now) #[2 b]) #'play-lsample-f (+ -5  root) .55 .3)
     (at (+ (now) #[4 b]) #'seq-test newroot)))

(defun left (&optional (root 60))
  (let* ((newroot (random-list (cdr (assoc root '((60 58 55)
                                                  (58 60 56)
                                                  (56 55 58)
                                                  (55 60 56)))))))
     (play-lsample-f (+ root -12) 1.5 .4)
     (at (+ (now) #[3/2 b]) #'play-lsample-f (+ -5  root) 1.5 .4)
     (at (+ (now) #[4 b]) #'left newroot)))

(defun right ()
  (play-lsample (cosr 60 7 3/4) 1 .1)
  (at (+ (now) #[1 b]) #'right)
  )
#|
(left)
(incudine:free 0)
(flush-pending)
(right)
|#

(defun right ())
