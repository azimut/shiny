(in-package :somecepl)

;; Needs (rt-start) first.

;; (dsp! bplay
;;     ((buf buffer) rate start-pos
;;      (loop-p boolean) attenuation rms (cfreq fixnum))
;;   (:defaults 0d0 -1 0 nil .00001 0 10)
;;   (with-samples
;;       ((in (incudine.vug:buffer-play
;;             buf rate start-pos loop-p #'stop))
;;        (inn (* in attenuation))
;;       ;; (inn (incudine.vug:downsamp cfreq inn))
;;        )
;;     (out inn inn)))

(dsp! bplay
    ((buf buffer) rate start-pos
     (loop-p boolean) attenuation rms (cfreq fixnum))
  (:defaults 0d0 1 0 nil .00001 0 10)
  (with-samples
      ((in (incudine.vug:buffer-play
            buf rate start-pos loop-p #'incudine:free))
       (inn (* in attenuation))
       (inn (+ inn
;;               (incudine.vug:buzz 2 .1 2)
             ;; (incudine.vug:downsamp 10 inn)
             ;; (incudine.vug:lpf inn 100 1)
             ;; (incudine.vug:hpf inn 200 10)
             ;; (incudine.vug:butter-hp inn 1000)
               ))       
       )
    (out inn inn)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Description: These one help slicing buffers into smaller pieces and
;; reference those by a shortname.

;; TODO:
;;  - right now it uses (at) to stop (word-play),
;;    might be buffer-play can support other thing
;;    ... I could have used (buffer-read) and (phasor-loop)
;;    ... on the other hand it makes easy to play backwards
;;  - Make it quickload-able
;;  - Add slice support. With start and stop as relative values

(defvar *buffers* (make-hash-table :test #'equal))
(defvar *sample-words* (make-hash-table :test #'equal))

(defun corrupt-node
    (time id &key (center 1f0) (radius .1) (period 1) (life 3))
  (declare (sample time) (integer id))
  "sends random rate changes to the node ID
   LIFE is needed to avoid early check of the node"
  (let ((alive (node-id (node id))))
    (when (or alive (> life 0))
      (set-control
       id
       :rate (funcall (pick '+ '+ '+ '+ '+ '+ '-)
                      (cosr center radius 10)))
      (aat (+ time #[period b])
           #'corrupt-node it id :life (- life 1)))))

(defun pitch-to-ratio (p)
  (declare (integer p))
  "relative midi pitch to frequency ratio"
  (expt 2 (/ p 12)))

(defun beat-to-ratio (beat-length sample-rate dur)
  (declare (sample sample-rate) (number beat-length dur))
  "given a BEAT-LENGTH and DURation in seconds returns a rate"
  (let* ((samples-to-play (* dur sample-rate))
         (samples-per-beat (/ samples-to-play beat-length))
         (rate (/ samples-per-beat sample-rate)))
    rate))

(defun bbuffer-load (filename)
  (declare (string filename))
  "buffer-load wrapper for global hash caching and easy access
  FILENAME is a normal string of a path where to load the file
  (bbuffer-load \"/home/sendai/curso/furi-dream-cc.wav\")"
  (let* ((hkey (file-namestring filename))
         (buf  (gethash hkey *buffers*)))
    (if (not buf)
        (setf (gethash hkey *buffers*)
              (buffer-load filename))
        buf)))

;;--------------------------------------------------
;; Single buffer play

(defun bbplay (buf &key (rate 1 rate-p) (rpitch 0 rpitch-p)
                     (beat-length 4 beat-length-p)
                     (start-pos 0)
                     (loop-p nil) (attenuation 2) (id 2 id-p))
  (declare (buffer buf) (integer rpitch id) (boolean loop-p))
  "plays the provided buffer either by
   RATE plays the buffer to play at rate
   RPITCH bends the whole buffer rate to play to the new pitch offset
   BEAT-LENGTH stretch the whole buffer to play for N beats"
  ;; big check so we can send nil without crashing
  (when (or (and rate-p rate)
            (and beat-length-p beat-length)
            (and rpitch-p rpitch))
    (let ((sample-rate (buffer-sample-rate buf))
          (size   (buffer-size buf))
          (frames (buffer-frames buf)))
      (when rpitch-p
        (setf rate (pitch-to-ratio rpitch)))
      (when beat-length-p
        ;; need to calculate the total duration in sec
        ;; to provide it to (beat-to-ratio
        (let ((dur (/ size sample-rate)))
          (setf rate (beat-to-ratio beat-length
                                    sample-rate
                                    dur))))
      ;; fix to work with buffer-play vug
      (when (< rate 0)
        (setf start-pos (- frames (/ frames 20))))
      (if id-p
          (bplay buf rate start-pos loop-p
                 :attenuation attenuation
                 :id id)
          (bplay buf rate start-pos loop-p
                 :attenuation attenuation)))))

;;------------------------------------------------------
;; Slice of a single buffer, dedicated to words or phrases

(defstruct phrase
  filename
  start-at
  dur)

(defun put-phrase (word filename start-at dur)
  (declare (string word filename) (number start-at dur))
  "creates a reference on global hash for WORD in FILENAME
   begining at START-AT for DUR
  (put-phrase \"nothing\" \"furi-better-cc.wav\" 1.7 1.9)"
  (setf (gethash word *sample-words*)
        (make-phrase :filename filename
                     :start-at start-at
                     :dur dur)))

(defun word-play (phrase
                  &key (loop-p nil) (attenuation 1f0)
                    (rate 1) (rpitch 1 rpitch-p)
                    (beat-length 1 beat-length-p) (id 2)
                    corrupt
                    (corrupt-center 1f0)
                    (corrupt-radius .1)
                    (corrupt-period 1))
  (declare (integer rpitch id) (string phrase) (boolean loop-p))
  "bplay wrapper to play the phrase provided
  (word-play \"time\")
  (word-play \"time\" :rate 2)
  (word-play \"time\" :rpitch 7)
  (word-play \"time\" :beat-length 4)"
  (when phrase
    (let* ((obj (gethash phrase *sample-words*))
           (dur (phrase-dur obj))
           (buf (gethash (phrase-filename obj)
                         *buffers*))
           (start-pos (* (buffer-sample-rate buf)
                         (phrase-start-at obj))))
      ;; pitch to ratio - from sonic-pi
      (if rpitch-p
          (setf rate (pitch-to-ratio rpitch)))
      ;; play for beat-length
      (if beat-length-p
          (setf rate (beat-to-ratio beat-length
                                    (buffer-sample-rate buf)
                                    dur)))
      ;; start voice
      (bplay buf rate start-pos loop-p
             :attenuation attenuation
             :id id)
      (when corrupt
        (corrupt-node (now)
                      id
                      :center corrupt-center
                      :radius corrupt-radius
                      :period corrupt-period))
      ;; kill voice
      (let ((dur (* (/ (abs rate)) dur)))
        (at (+ (now) #[dur b]) #'incudine:free (node id))))))


;;--------------------------------------------------
;; An instrument is composed of different sound files
;; that can be referenced by calling one function with
;; different pitches. They also might need a sustain to
;; play longer than designed without change pitch.
;; NOTE: Using some of ormf's code.

(defparameter *instruments* (make-hash-table))

(defclass instrument ()
  ((name   :initarg :name)
   (dir    :initarg :dir)
   (lnotes :initarg :lnotes :initform '())
   (keys   :initarg :keys   :accessor keys)))

(defun make-instrument (name directory)
  (declare (string directory) (symbol name))
  (setf (gethash name *instruments*)
        (make-instance 'instrument
                       :name name
                       :dir directory
                       :keys (make-array 127))))

(defun list-instruments ()
  (alexandria:hash-table-keys *instruments*))

(define-vug buffer-loop-play
    ((buffer buffer) rate start-pos loopstart loopend)
  (with-samples
      ((frame (phasor-loop rate start-pos loopstart loopend)))
    (buffer-read buffer frame :interpolation :cubic)))

(dsp! play-lsample-f
    ((buf buffer) dur amp loopstart loopend rate)
  (:defaults nil 1 1 0 0 1)
  (with-samples ((in (buffer-loop-play buf rate 0 loopstart loopend))
                 (in (+ (* .6 in)
                        (incudine.vug:resonr in 400 .9)
                        )))
    (stereo (* amp
               (envelope (make-envelope '(0 1 1 0) '(0 .9 .1))
                         1 dur #'incudine:free)
               in))))

(defclass key ()
  ((filename  :initarg :filename)
   (keynum    :initarg :keynum)
   (startpos  :initarg :startpos)
   (loopstart :initarg :loopstart)
   (loopend   :initarg :loopend)))

(defun push-note (name keynum filename
                  &optional (startpos 0) loopstart loopend)
  (declare (symbol name) (integer keynum) (string filename))
  (let ((instrument (gethash name *instruments*)))
    (when instrument
      (with-slots (dir keys lnotes) instrument
        (let* ((fullpath (concatenate 'string dir filename))
               (buf (bbuffer-load fullpath))
               (buf-size (buffer-size buf)))
          (pushnew keynum lnotes)
          (setf loopend buf-size
                loopstart (/ buf-size 10))
          (setf (aref keys keynum)
                (make-instance 'key
                               :filename filename
                               :keynum keynum
                               :startpos startpos
                               :loopstart loopstart
                               :loopend loopend)))))))

(defun play-instrument (name keynum
                        &optional (dur 1) (amp 1) (rate 1))
  (with-slots (keys) (gethash name *instruments*)
    (with-slots (filename loopend loopstart) (aref keys keynum)
      (let ((buf (gethash filename *buffers*)))
        (play-lsample-f buf dur amp loopstart loopend rate)))))
