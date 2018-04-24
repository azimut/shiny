(in-package :somecepl)

;; overtone/src/overtone/sc/sample.clj
;; extempore/libs/external/instruments_ext-scm.xtm


(defvar *ssdir* "/home/sendai/samples/piano/Salamander")

(define-vug buffer-loop-play ((buffer buffer)
                              rate
                              start-pos
                              loopstart
                              loopend)
  (buffer-read buffer
               (phasor-loop rate start-pos loopstart loopend)
               :interpolation :cubic))

(dsp! b09-sampler-loop-smooth ((buf buffer) freq chunk-size)
  (with-samples ((phs (phasor freq 0)))
    (stereo (* (cos (* (- phs 0.5d0) pi))
               (buffer-read buf (* phs 441
                                   (samphold chunk-size phs 0 1))
                            :wrap-p t :interpolation :cubic)))))



(declaim (inline get-lsample))
(defun get-lsample (pitch map)
  (aref map (min pitch 127)))
(declaim (inline ct->fv))
(defun ct->fv (steps)
  "halfsteps to ratio"
  (expt #.(coerce 2.0 'incudine.util:sample)
        (* steps #.(coerce 1/12 'incudine.util:sample))))
(defparameter *env1* (make-envelope '(0 1 1 0) '(0 .9 .1)))


(dsp! p (time pitch velocity duration channel)
  (with ((lsample (get-lsample (sample->fixnum pitch) *sam2*)))
    (with-samples ((rate      (ct->fv (- pitch (lsample-keynum lsample))))
                   ;;                   (loopstart (lsample-loopstart lsample))
                   (loopstart 64379d0)
                   (loopend   (lsample-loopend   lsample))
                   )
      (with ((buffer (lsample-buffer lsample)))
        (stereo (* velocity
                   (envelope *env1* 1 duration #'incudine:stop)
                   (buffer-loop-play buffer rate 0 loopstart loopend)))))))


(defstruct lsample
  "structure for a sample with two loop-points. The structure also
contains a slot for the sample buffer data."
  file
  buffer
  (keynum    +sample-zero+ :type incudine.util:sample)
  (loopstart +sample-zero+ :type incudine.util:sample)
  (loopend   +sample-zero+ :type incudine.util:sample))

(defun generate-lsample-map (waves keynums) 
  "map a list of sample defs of the form (filename keymap-upper-bound
original-keynum loopstart loopend) into a freshly allocated lookup
array containing an lsample-struct entry for 127 keynums. The buffer
slots of the lsamples are filled with the sample data. The array is
returned."
  (let* ((tuple  (loop :for w :in waves :for k :in keynums :collect (cons w k)))
         (stuple (sort tuple (lambda (a b) (< (cdr a) (cdr b)))))
         (lsample-map (make-array 128 :adjustable nil :element-type 'lsample)))
    (loop
       :for lsample-def :in stuple
       :with array-idx = -1
       :do (destructuring-bind
                 (wave-path . keynum) 
               lsample-def
             (let ((lsample ;;; make lsample from lsample-def and fill slots
                    (let ((buf (buffer-load wave-path)))
                      (make-lsample :file      (pathname-name wave-path)
                                    :buffer    (buffer-load wave-path)
                                    :keynum    (incudine.util:sample keynum)
                                    :loopstart (incudine.util:sample 1)
                                    :loopend   (incudine.util:sample (buffer-size buf))))))
               (do () ;;; fill array with references to the lsample
                   ((>= array-idx keynum))
                 (setf (aref lsample-map (incf array-idx)) lsample)))))
    (values lsample-map)))

(defun load-wav-samples (dir &optional keynums)
  (declare (type list keynums))
  (let ((wav-files (directory (merge-pathnames dir "/*.wav"))))
    (unless wav-files (error "No WAV files"))
    (unless keynums
      (setf keynums (mapcar (lambda (x) (note-name-to-midi-number (file-namestring x)))
                            wav-files)))
    (or (= (length keynums) (length wav-files)) (error "Wrong number of keynums"))
    (generate-lsample-map wav-files keynums)
    ))

