(in-package :shiny)
;; Only works when aubio is compiled to use double for samples
(ql:quickload :aubio/double)
(ql:quickload :cl-gme/incudine)

(incudine:free (node 0))
(bbuffer-load "/home/sendai/quicklisp/local-projects/aubio/static/loop_amen.wav")
(gmeplay "/home/sendai/Downloads/sf2/ff3.nsf" 2 2
         :amp .01
         :load-only t
;;         :loop-p nil
         :length 20
         :offset 20
         :voices '(2 3 1)
         :rate 1
         :fade-time 1
         :fade-curve 2)

(cffi:defcstruct fvec_t
	(length :unsigned-int)
	(data :pointer))

(defparameter *fvec*
  (cffi:foreign-alloc '(:pointer (:struct fvec_t))))

(setf (cffi:foreign-slot-value *fvec* '(:struct fvec_t) 'data)
      (buffer-data (gethash "/home/sendai/Downloads/sf2/ff3.nsf2" *playing*)))

(setf (cffi:foreign-slot-value *fvec* '(:struct fvec_t) 'length)
      (buffer-size (gethash "/home/sendai/Downloads/sf2/ff3.nsf2" *playing*)))

(defun test-onset (source-buffer)
  "returns a list of the seconds where a set is found"
  (declare (incudine:buffer source-buffer))
  (let* ((buffer-frames (buffer-frames source-buffer))
         (slices (loop :for slice :from 0 :by 512 :to buffer-frames
                    :collect slice))
         (sets '())
         (total-seconds (/ buffer-frames 44100d0)))
    (assert (= 44100d0 (buffer-sample-rate source-buffer)))
    (aubio:with-onset (onset :buf-size 1024 :hop-size 512)
      (aubio:with-fvec (out-fvec 1)
        (cffi:with-foreign-object (fvec '(:pointer (:struct fvec_t)))
          (cffi:with-foreign-slots ((data length) fvec (:struct fvec_t))
            (setf length 512)
            (loop
               :for slice :in slices
               :with times = '()
               :finally (setf sets (reverse times))
               :do
               (setf data (cffi:inc-pointer (buffer-data source-buffer)
                                            (* 8 2 slice)))
               ;; Perform onset calculation
               (sb-int:with-float-traps-masked (:divide-by-zero)
                 (aubio:aubio_onset_do onset fvec out-fvec))
               ;; Retrieve result
               (let ((onset-new-peak (aubio:fvec_get_sample out-fvec 0)))
                 (when (> onset-new-peak 0)
                   (push (aubio:aubio_onset_get_last_s onset) times))))
            (when(< (alexandria:lastcar sets) total-seconds)
              (alexandria:appendf sets (list total-seconds)))))))))

(defun test-beats (source-buffer)
  "returns a list of the seconds where a set is found"
  (declare (incudine:buffer source-buffer))
  (let* ((buffer-frames (buffer-frames source-buffer))
         (slices (loop :for slice :from 0 :by 512 :to buffer-frames
                    :collect slice))
         (sets '())
         (total-seconds (/ buffer-frames 44100d0)))
    (aubio::with-tempo (tempo :buf-size 1024 :hop-size 512)
      (aubio:with-fvec (out-fvec 1)
        (cffi:with-foreign-object (fvec '(:pointer (:struct fvec_t)))
          (cffi:with-foreign-slots ((data length) fvec (:struct fvec_t))
            (setf length 512)
            (loop
               :for slice :in slices
               :with times = '()
               :finally (setf sets (reverse times))
               :do
               (setf data (cffi:inc-pointer (buffer-data source-buffer)
                                            (* 8 2 slice)))
               ;; Perform onset calculation
               (sb-int:with-float-traps-masked (:divide-by-zero)
                 (aubio:aubio_tempo_do tempo fvec out-fvec))
               ;; Retrieve result
               (let ((in-beat (aubio:fvec_get_sample out-fvec 0)))
                 (when (> in-beat 0)
                   (push (aubio:aubio_tempo_get_last_s tempo) times))))
            (when (< (alexandria:lastcar sets) total-seconds)
              (alexandria:appendf sets (list total-seconds)))))))))


(defun get-sets (l)
  (declare (list l))
  (assert (>= (length l) 4))
  (let* ((sets (butlast (cdr l))) ;; remove non-real sets
         (set-pairs
          (loop
             :for (x y) :on sets :by #'cddr
             :collect (list x y))))
    (mapcar (lambda (x)
              (destructuring-bind (sec1 sec2) x
                  (list (round (* 44100 2 sec1)) (round (* 44100 2 sec2)))))
            set-pairs)))

(defun get-longest-set (l)
  (let* ((sets (butlast (cdr l)))
         (sec1 (car sets))
         (sec2 (lastcar sets)))
    (mapcar (lambda (x) (round (* x 44100))) (list sec1 sec2))))

(defun play-n-set (buffer sets n)
  (declare (incudine:buffer buffer) (list sets) (integer n))
  (assert (< n (length sets)))
  (let* ((set (nth n sets))
        (frame-start (first set))
        (frame-end (lastcar set)))
    (play-lsample-f
     buffer
     :id 100
     :dur (/ (- frame-end frame-start) 44100)
     :amp .01
     :start-pos frame-start
     :loopstart frame-start
     :loopend frame-end)))

(defun play-set (buffer frame-start frame-end)
  (declare (incudine:buffer buffer))
  (play-lsample
   buffer
   :id 100
   :amp .01
   :start-pos frame-start
   :loopend frame-end))

(defun f (time)
  (play-n-set (gethash "/home/sendai/Downloads/sf2/ff3.nsf2" *playing*)
            *sets* (pick 2 3 4))
  (aat (+ time #[.2 b]) #'f it))

(defun f ())
(incudine:free (node 0))
(f (now))
