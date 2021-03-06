(in-package :shiny)

;; ## Using with incudine

;; Most straightforward usage I can think of is having aubio read an incudine buffer. Just have aubio and incudine use the same type for samples. And do something like:
;; ```
;; (defun test-onset (source-buffer)
;;   "returns a list of the seconds where a set is found"
;;   (declare (incudine:buffer source-buffer))
;;   (let* ((buffer-frames (buffer-frames source-buffer))
;;          (sample-rate (round (buffer-sample-rate source-buffer)))
;;          (slices (loop :for slice :from 0 :by 512 :to buffer-frames
;;                     :collect slice))
;;          (sets '())
;;          (frames '())
;;          (total-seconds (coerce (/ buffer-frames sample-rate) 'double-float)))
;;     (assert (= 44100 sample-rate))
;;     (aubio:with-onset (onset :sample-rate sample-rate)
;;       (aubio:with-fvec (out-fvec 1)
;;         (cffi:with-foreign-object (fvec '(:pointer (:struct fvec_t)))
;;           (cffi:with-foreign-slots ((data length) fvec (:struct fvec_t))
;;             (setf length 512)
;;             (loop
;;                :for slice :in slices :do
;;                (setf data (cffi:inc-pointer (buffer-data source-buffer)
;;                                             (* 8 2 slice)))
;;                ;; Perform onset calculation
;;                (sb-int:with-float-traps-masked (:divide-by-zero)
;;                  (aubio:aubio_onset_do onset fvec out-fvec))
;;                ;; Retrieve result
;;                (let ((onset-new-peak (aubio:fvec_get_sample out-fvec 0)))
;;                  (when (> onset-new-peak 0)
;;                    (let ((set (aubio:aubio_onset_get_last_s onset)))
;;                      (push set sets)
;;                      (push (round (* set sample-rate)) frames)))))
;;             (push total-seconds sets)
;;             (push buffer-frames frames)
;;             (values (reverse sets) (reverse frames))))))))
;; ```


;; Only works when aubio is compiled to use double for samples
;;(ql:quickload :aubio/double)

;; Define aubio fvec_t struct to hack a new one using Incudine's buffers
(cffi:defcstruct fvec_t
  (length :unsigned-int)
  (data :pointer))

(defun test-onset (source-buffer)
  "returns a 2 VALUES lists of the seconds where a set is found and
   the buffer frame where it happened"
  (declare (incudine:buffer source-buffer))
  (let* ((buffer-frames (buffer-frames source-buffer))
         (sample-rate (round (buffer-sample-rate source-buffer)))
         (slices (loop :for slice :from 0 :by 512 :to buffer-frames
                    :collect slice))
         (sets '())
         (frames '())
         (total-seconds (coerce (/ buffer-frames sample-rate) 'double-float)))
    (assert (= 44100 sample-rate))
    (aubio:with-onset (onset :sample-rate sample-rate)
      (aubio:with-fvec (out-fvec 1)
        (cffi:with-foreign-object
            (fvec '(:pointer (:struct fvec_t)))
          (cffi:with-foreign-slots
              ((data length) fvec (:struct fvec_t))
            (setf length 512)
            (loop
               :for slice :in slices :do
                 (setf data (cffi:inc-pointer (buffer-data source-buffer)
                                              (* 8 2 slice)))
               ;; Perform onset calculation
                 (sb-int:with-float-traps-masked (:divide-by-zero)
                   (aubio:aubio_onset_do onset fvec out-fvec))
               ;; Retrieve result
                 (let ((onset-new-peak (aubio:fvec_get_sample out-fvec 0)))
                   (when (> onset-new-peak 0)
                     (let ((set (aubio:aubio_onset_get_last_s onset)))
                       (push set sets)
                       (push (round (* set sample-rate)) frames)))))
            (push total-seconds sets)
            (push buffer-frames frames)
            (values (reverse sets) (reverse frames))))))))

(defun test-beats (source-buffer)
  "returns a list of the seconds where a set is found"
  (declare (incudine:buffer source-buffer))
  (let* ((buffer-frames (buffer-frames source-buffer))
         (n-channels (buffer-channels source-buffer))
         (sample-rate (buffer-sample-rate source-buffer))
         (slices (loop :for slice :from 0 :by 512 :to buffer-frames
                    :collect slice))
         (total-seconds (/ buffer-frames sample-rate))
         (sets '())
         (frames '()))
    (aubio::with-tempo (tempo :sample-rate sample-rate)
      (aubio:with-fvec (out-fvec 1)
        (cffi:with-foreign-object (fvec '(:pointer (:struct fvec_t)))
          (cffi:with-foreign-slots ((data length) fvec (:struct fvec_t))
            (setf length 512)
            (loop
               :for slice :in slices :do
               (setf data (cffi:inc-pointer (buffer-data source-buffer)
                                            (* 8 n-channels slice)))
               ;; Perform onset calculation
               (sb-int:with-float-traps-masked (:divide-by-zero)
                 (aubio:aubio_tempo_do tempo fvec out-fvec))
               ;; Retrieve result
               (let ((in-beat (aubio:fvec_get_sample out-fvec 0)))
                 (when (> in-beat 0)
                   (push (aubio:aubio_tempo_get_last_s tempo) sets)
                   (push (aubio:aubio_tempo_get_last tempo) frames))))
            (push total-seconds sets)
            (push (round (* sample-rate total-seconds)) frames)
            (values (reverse sets) (reverse frames))))))))

(defun test-pitches (source-buffer)
  "returns a list of the seconds where a set is found"
  (declare (incudine:buffer source-buffer))
  (let* ((buffer-frames (buffer-frames source-buffer))
         (n-channels (buffer-channels source-buffer))
         (sample-rate (buffer-sample-rate source-buffer))
         (slices (loop :for slice :from 0 :by 512 :to buffer-frames
                    :collect slice))
         (total-seconds (/ buffer-frames sample-rate))
         (pitches '())
         (confidences '()))
    (aubio::with-pitch (pitch :sample-rate sample-rate :confidence .8)
      (aubio:with-fvec (out-fvec 1)
        (cffi:with-foreign-object (fvec '(:pointer (:struct fvec_t)))
          (cffi:with-foreign-slots ((data length) fvec (:struct fvec_t))
            (setf length 512)
            (loop
               :for slice :in slices
               :with last-pitch = (sample 0)
               :do
               (setf data (cffi:inc-pointer (buffer-data source-buffer)
                                            (* 8 n-channels slice)))
               ;; Perform pitch calculation
               (sb-int:with-float-traps-masked (:divide-by-zero)
                 (aubio:aubio_pitch_do pitch fvec out-fvec))
               ;; Retrieve result
               (let ((p (round (aubio:fvec_get_sample out-fvec 0)))
                     (confidence (aubio:aubio_pitch_get_confidence pitch)))
                 (when (and (not (= last-pitch p))
                            ;;(> confidence .9)
                            ) 
                   (push confidence confidences)
                   (push p pitches))
                 (setf last-pitch p)))
            (values (reverse pitches) (reverse confidences))))))))
;;--------------------------------------------------

(defun remove-sides (l &optional (n 1))
  (declare (list l) (alexandria:non-negative-integer n))
  (let ((len (length l)))
    (subseq l (+ 0 n) (- len n))))

(defun get-sets (l &optional (slicer #'cddr))
  "Takes list L of frames and groups them in pairs.
   Examples:
  (get-sets (nth-value 1 (test-onset ...)))
  (get-sets (nth-value 1 (test-onset ...)) #'cdr)"
  (declare (type list l) (type function slicer))
  (assert (>= (length l) 4))
  (let* ((sets (remove-sides l)) ;; remove non-real sets
         (set-pairs
          (loop :for (x y) :on sets :by slicer :collect
               (list x y))))
    ;; Remove sets with NIL
    (remove-if (lambda (x) (member NIL x)) set-pairs)))


(defun save-sets (buf &optional (slicer #'cddr))
  "saves provided buffer OR buffer-name sets into *BUFFER-SETS*
   as an array"
  (declare (function slicer))
  (when (or (symbolp buf) (stringp buf))
    (let ((b (gethash buf *buffers*)))
      (when b
        (setf buf b))))
  (when-let* ((buffer-p (buffer-p buf))
              (buffer-name (file-namestring (buffer-file buf)))
              (sets (get-sets
                     (remove-sides
                      (nth-value 1 (test-onset buf)))
                     slicer))
              (lsets (length sets))
              (sets-array (make-array (list lsets 2) :initial-contents sets)))
    (setf (gethash buffer-name *buffer-sets*) sets-array)))

(defun save-beats (buf &optional (slicer #'cddr))
  "saves provided buffer OR buffer-name sets into *BUFFER-SETS*
   as an array"
  (declare (function slicer))
  (when (or (symbolp buf) (stringp buf))
    (let ((b (gethash buf *buffers*)))
      (when b
        (setf buf b))))
  (when-let* ((buffer-p (buffer-p buf))
              (buffer-name (file-namestring (buffer-file buf)))
              (sets (get-sets
                     (remove-sides
                      (nth-value 1 (test-beats buf)))
                     slicer))
              (lsets (length sets))
              (sets-array (make-array (list lsets 2) :initial-contents sets)))
    (setf (gethash buffer-name *buffer-sets*) sets-array)))

(defun get-longest-set (l)
  (let* ((sets (remove-sides l))
         (sec1 (first-elt sets))
         (sec2 (last-elt  sets)))
    (mapcar (lambda (x) (round (* x 44100)))
            (list sec1 sec2))))

;;--------------------------------------------------

(defun play-n-set (buffer sets n)
  (declare (incudine:buffer buffer)
           (list sets)
           (integer n))
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

(defun slice-buffer (source-buffer start-frame end-frame)
  "creates a new buffer, with frames from SOURCE-BUFFER,
  between START-FRAME and END-FRAME, destroys SOURCE-BUFFER"
  (declare (incudine:buffer source-buffer)
           (alexandria:non-negative-integer start-frame end-frame))
  (assert (> end-frame start-frame))
  (let* ((sample-rate (buffer-sample-rate source-buffer))
         (channels (buffer-channels source-buffer))
         (frames (- end-frame start-frame))
         (dest-buffer (make-buffer frames
                      :channels channels
                      :sample-rate sample-rate)))
    (incudine.external:foreign-copy
     (buffer-data dest-buffer)
     (cffi:inc-pointer (buffer-data source-buffer) (* start-frame 8 channels))
     (* channels 8 frames))
    (incudine:free source-buffer)
    dest-buffer))

(defun in-times (l f)
  (declare (function f) (list l))
  (mapcar (lambda (x) (at (+ (now) #[x b])
                     f))
          l))
