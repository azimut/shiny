(in-package :shiny)

;; Needs (rt-start) first.

(dsp! bplay
    ((buf buffer) rate start-pos
     (loop-p boolean) amp left right)
  (:defaults (incudine:incudine-missing-arg "BUFFER")
      1 0 nil 1 1 1)
  (with-samples
      ((in (* amp
              (buffer-play
               buf rate start-pos loop-p #'incudine:free))))
    (out (* left in) (* right in))))

(dsp! bplay-downsamp
    ((buf buffer) rate start-pos
     (loop-p boolean) amp left right (downsamp fixnum))
  (:defaults (incudine:incudine-missing-arg "BUFFER")
      1 0 nil 1 1 1 1)
  (with-samples
      ((in (* amp
              (buffer-play
               buf (* downsamp rate) start-pos loop-p #'incudine:free)))
       (in (incudine.vug:downsamp downsamp in)))
    (out (* left in) (* right in))))

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

(defun list-buffers ()
  (alexandria:hash-table-keys *buffers*))

(defun list-words ()
  (alexandria:hash-table-keys *sample-words*))

(defun node-alive (id)
  (node-id (node id)))

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
      (aat (+ time (* *sample-rate* period))
           #'corrupt-node it id :life (- life 1)))))

(defun pitch-to-ratio (p)
  (declare (integer p))
  "relative midi pitch to frequency ratio"
  (expt 2 (/ p 12)))

(defun beat-to-ratio (beat-length sample-rate dur)
  (declare (sample sample-rate) (number beat-length dur))
  "given a BEAT-LENGTH and DURation in seconds returns a rate"
  (let* ((samples-to-play  (* dur sample-rate))
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

(defun bbplay
    (buf &key (rate 1d0 rate-p) (rpitch 1 rpitch-p) (beat-length 1f0 beat-length-p)
           (start-pos 0d0) (loop-p nil) (amp 1d0) (id 2 id-p)
           (left 1d0) (right 1d0) (downsamp 1 downsamp-p) (pan .5 pan-p))
  (declare (integer rpitch id downsamp) (boolean loop-p) (float pan))
  "plays the provided buffer either by
   RATE plays the buffer to play at rate
   RPITCH bends the whole buffer rate to play to the new pitch offset
   BEAT-LENGTH stretch the whole buffer to play for N beats"
  (when (stringp buf)
    (let ((b (gethash buf *buffers*)))
      (if b
          (setf buf b)
          (error "No buffer with that name on *buffers*"))))
  ;; TODO: big check so we can send nil to beat-length without crashing
  (let ((sample-rate (buffer-sample-rate buf))
        (frames (buffer-frames buf)))
    (when rpitch-p
      (mulf rate (pitch-to-ratio rpitch)))
    (when beat-length-p
      ;; need to calculate the total duration in sec
      ;; to provide it to (beat-to-ratio
      (let ((dur (/ frames sample-rate)))
        (mulf rate (beat-to-ratio beat-length
                                  sample-rate
                                  dur))))
    ;; "hack" to work with buffer-play vug
    (when (< rate 0)
      (setf start-pos (- frames (/ frames 20))))

    (when pan-p
      (setf pan (alexandria:clamp pan 0f0 1f0))
      (cond ((> pan .5) (decf left  (* 2 (- pan .5))))
            ((< pan .5) (decf right (* 2 (abs (- pan .5)))))))
    
    (if downsamp-p
        (if id-p
            (bplay-downsamp buf rate start-pos loop-p
                            :amp amp
                            :left left :right right
                            :downsamp downsamp)
            (bplay-downsamp buf rate start-pos loop-p
                            :amp amp
                            :left left :right right
                            :downsamp downsamp
                            :id id))
        (if id-p
            (bplay buf rate start-pos loop-p
                   :amp amp
                   :left left :right right
                   :id id)
            (bplay buf rate start-pos loop-p
                   :amp amp
                   :left left :right right)))))

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

(defun word-play
    (phrase &key (rate 1) (rpitch 1 rpitch-p) (beat-length 1 beat-length-p)
              (loop-p nil) (amp 1f0) (id 2)
              corrupt
              (downsamp 1 downsamp-p)
              (left 1f0) (right 1f0) (pan 1f0 pan-p)
              (corrupt-center 1f0)
              (corrupt-radius .1)
              (corrupt-period 1))
  (declare (integer rpitch id) (boolean loop-p))
  "bplay wrapper to play the phrase provided
  (word-play \"time\")
  (word-play \"time\" :rate 2)
  (word-play \"time\" :rpitch 7)
  (word-play \"time\" :beat-length 4)"
  (when (or (and phrase (stringp phrase))
            (and phrase (not (eq phrase :end-of-data))))
    (let* ((obj (gethash phrase *sample-words*))
           (dur (phrase-dur obj))
           (buf (gethash (phrase-filename obj)
                         *buffers*))
           (sample-rate (buffer-sample-rate buf))
           (start-pos (* sample-rate
                         (phrase-start-at obj))))
      ;; pitch to ratio - from sonic-pi
      (if rpitch-p
          (mulf rate (pitch-to-ratio rpitch)))
      ;; play for beat-length
      (if beat-length-p
          (mulf rate (beat-to-ratio beat-length
                                    sample-rate
                                    dur)))
      ;; change where to start when played backwards
      (when (< rate 0)
        (incf start-pos (* dur sample-rate)))

      (when pan-p
        (setf pan (alexandria:clamp pan 0f0 1f0))
        (cond ((> pan .5) (decf left  (* 2 (- pan .5))))
              ((< pan .5) (decf right (* 2 (abs (- pan .5)))))))
          
      ;; start voice
      (if downsamp-p
          (bplay-downsamp buf rate start-pos loop-p
                          :left left
                          :right right
                          :amp amp
                          :downsamp downsamp
                          :id id)
          (bplay buf rate start-pos loop-p
                 :left left
                 :right right
                 :amp amp
                 :id id))
      (when corrupt
        (corrupt-node (now)
                      id
                      :center corrupt-center
                      :radius corrupt-radius
                      :period corrupt-period))
      ;; kill voice
      (let ((dur (* (/ (abs rate)) dur)))
        (at (+ (now) (* *sample-rate* dur)) #'incudine:free (node id)))
      T)))


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

(defun play-instrument
    (name keynum &key (dur 1) (amp 1) (rate 1))
  (declare (symbol name) (integer keynum) (number dur amp rate))
  (with-slots (keys) (gethash name *instruments*)
    (with-slots (filename loopend loopstart) (aref keys keynum)
      (let ((buf (gethash filename *buffers*)))
        (if (> rate 0)
            (play-lsample-f buf dur amp loopstart loopend rate)
            (play-lsample-f buf dur amp loopend loopstart rate))))))
