(in-package :somecepl)

;; TODO: look at implementations:
;; - overtone/src/overtone/sc/sample.clj
;; - extempore/libs/external/instruments_ext-scm.xtm
;; Currently, this uses phasor-loop as the phase function
;;  for buffer-read

(declaim (inline get-lsample))
(defun get-lsample (pitch map)
  (aref map (min pitch 127)))
(declaim (inline ct->fv))
(defun ct->fv (steps)
  "halfsteps to ratio"
  (expt #.(coerce 2.0 'incudine.util:sample)
        (* steps #.(coerce 1/12 'incudine.util:sample))))
(defparameter *env1* (make-envelope '(0 1 1 0) '(0 .9 .1)))

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
  (let* ((tuple  (loop
                    :for w :in waves
                    :for k :in keynums
                    :collect (list w k k)))
         (stuple (sort tuple (lambda (a b) (< (caddr a) (caddr b)))))
         (lsample-map (make-array 128 :adjustable nil
                                  :element-type 'lsample)))
    (setf (caddar (last stuple)) 127)
    (loop
       :for lsample-def :in stuple
       :with array-idx = -1
       :do (destructuring-bind
                 (wave-path keynum lastkeynum) 
               lsample-def
             (let ((lsample ;;; make lsample from lsample-def and fill slots
                    (let ((buf (buffer-load wave-path)))
                      (make-lsample :file      (pathname-name wave-path)
                                    :buffer    (buffer-load wave-path)
                                    :keynum    (incudine.util:sample keynum)
                                    :loopstart (incudine.util:sample 1)
                                    :loopend   (incudine.util:sample
                                                (buffer-size buf))))))
               (do () ;;; fill array with references to the lsample
                   ((>= array-idx lastkeynum))
                 (setf (aref lsample-map (incf array-idx)) lsample)))))
       (values lsample-map)))

(defun load-wav-samples (dir &optional keynums)
  (declare (type list keynums))
  (let ((wav-files (directory (merge-pathnames dir "/*.wav"))))
    (unless wav-files
      (error "No WAV files"))
    (unless keynums
      (setf keynums
            (mapcar (lambda (x) (note-name-to-midi-number (file-namestring x)))
                    wav-files)))
    (unless (= (length keynums) (length wav-files))
      (error "Wrong number of keynums"))
    (generate-lsample-map wav-files keynums)))

#|
(defparameter *sam2*
  (load-wav-samples "/home/sendai/samples/piano/bosen/"))
|#

;; --------------------
;; Helpers
;; --------------------

(defun set-start (keynum map value)
  "sets the loop startime on lsample map with value <=1d0"
  (assert (and (typep value 'double-float) (<= value 1d0)
               (typep keynum 'integer) (<= keynum 128)))
  (with-slots (loopstart loopend) (aref map keynum)
    (let ((real-value (* value loopend)))
      (setf loopstart real-value))))

(defun get-start (keynum map)
  (lsample-loopstart (aref map keynum)))

;; ------------------------
;; Players
;; -----------------------

(define-vug buffer-loop-play ((buffer buffer)
                              rate
                              start-pos
                              loopstart
                              loopend)
  (buffer-read buffer
               (phasor-loop rate start-pos loopstart loopend)
               :interpolation :cubic))

(dsp! pf (pitch velocity duration)
  (with ((lsample (get-lsample (sample->fixnum pitch) *sam2*)))
    (with-samples ((rate      (ct->fv (- pitch (lsample-keynum lsample))))
                   (loopstart (lsample-loopstart lsample))
                   (velocity  (/ velocity 128d0))
                   (loopend   (lsample-loopend lsample)))
      (with ((buffer (lsample-buffer lsample)))
        (stereo (* velocity
                   (envelope *env1* 1 duration #'incudine:stop)
                   (buffer-loop-play buffer rate 0 loopstart loopend)))))))

;; --------------------------------------------------
;; NOTE: the idea for now is send the lsample hash as the channel
;; this is not backwards compatible :|
(defgeneric p (time pitch velocity duration channel)
  (:method (time (pitch list) velocity duration channel)
    "Play chord of notes"
    (mapcar (lambda (x) (p time x velocity duration channel))
            pitch))
  (:method (time (pitch list) velocity duration (channel list))
    "Play chord of notes, on provided channels"
    (mapcar (lambda (x y) (p time x velocity duration y))
            pitch
            channel))
  (:method (time (pitch integer) velocity (duration number) channel)
    "Play given numerical pitch"
    (at time #'pf pitch velocity duration))
  (:method (time (pitch integer) velocity (duration symbol) channel)
    "Play given numerial pitch, at CM rythm"
    (let ((d (cm:rhythm duration)))
      (at time #'pf pitch velocity d)))
  (:method (time (pitch symbol) velocity (duration symbol) channel)
    "Play given note, at CM rhythm"
    (unless (eql :_ pitch)
      (let ((n (note pitch))
            (d (cm:rhythm duration)))
        (at time #'pf n velocity d))))
  (:method (time (pitch symbol) velocity duration channel)
    "Play given note"
    (unless (eql :_ pitch)
      (let ((n (note pitch)))
        (at time #'pf n velocity duration)))))



