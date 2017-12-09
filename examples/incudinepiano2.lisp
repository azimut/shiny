(in-package :somecepl)

;; https://sourceforge.net/p/incudine/mailman/message/31182999/

;; small sample-player example for incudine. For this to work you'll
;; need to adjust the path to *ssdir* and put the Bosen_mf_... samples
;; into *ssdir*.
;;
;; This file is in the public domain. Use at own risk. No guarantees
;; whatsoever.

(defparameter *ssdir* #.(or *compile-file-pathname* *load-pathname*))
(defparameter *ssdir* "/home/sendai/Downloads/morepiano/piano-sampler-incudine/")

(define-vug phasor-loop (rate start-pos loopstart loopend)
  (with-samples ((pos start-pos)
                 (loopsize (- loopend loopstart)))
    (prog1 pos
      (incf pos rate)
      (if (> pos loopend)
          (decf pos loopsize)))))

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
				   :keynum (coerce keynum 'incudine.util:sample)
				   :loopstart (coerce loopstart 'incudine.util:sample)
				   :loopend (coerce loopend 'incudine.util:sample)))))
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
     ("BOSEN_mf_B4_mn.wav" 127 83  48970  50748))))

(declaim (inline get-lsample))
(defun get-lsample (keynum map)
  (aref map (min (round keynum) 127)))

;; example: (get-lsample 61 *piano-map*)
;; -> #S(LSAMPLE
;;       :FILE "/home/orm/work/snd/csound/BOSEN_mf_D#3_mn.wav"
;;       :BUFFER #<BUFFER :FRAMES 132917 :CHANNELS 1 :SR 44100.0>
;;       :KEYNUM 63
;;       :LOOPSTART 82246
;;       :LOOPEND 132839)

(declaim (inline ct->fv))
(defun ct->fv (steps)
  "halfsteps to ratio"
  (declare (type incudine.util:sample steps))
  (the incudine.util:sample (expt 2 (/ steps 12))))

(defparameter *env1* (make-envelope '(0 1 1 0) '(0 .9 .1)))

(dsp! play-lsample (keynum dur amp)
  (with ((lsample (get-lsample keynum *piano-map*)))
    (with-samples ((rate (ct->fv (- keynum (lsample-keynum lsample))))
                   (loopstart (lsample-loopstart lsample))
                   (loopend (lsample-loopend lsample)))
      (with ((buffer (lsample-buffer lsample)))
        (out (* amp 
		(envgen *env1* 1 dur #'incudine:free)
		(buffer-loop-play buffer rate 0 loopstart loopend)))))))

;; examples: 
#|
 (play-lsample (+ 50 (random 30)) 3 0.5)


 (loop
   for x from 1 to 200
   for time = (now) then (+ time (* *sample-rate* (random 0.05)))
   for amp = (+ 0.2 (random 0.2)) then (+ 0.2 (random 0.2))
   do (at time #'play-lsample (+ 70 (random 20.0)) 2 amp))

bouncing to disk:

 (bounce-to-disk ("/tmp/test.wav" :pad 2)
  (loop
     for x from 1 to 200
     for time = (now) then (+ time (* *sample-rate* (random 0.05)))
     for amp = (+ 0.2 (random 0.2)) then (+ 0.2 (random 0.2))
     do (at time #'play-lsample (+ 70 (random 20.0)) 2 amp)))
|#
