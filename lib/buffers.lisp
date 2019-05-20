(in-package :shiny)

;; TODO:
;;  - right now it uses (at) to stop (word-play),
;;    might be buffer-play can support other thing
;;    ... I could have used (buffer-read) and (phasor-loop)
;;    ... on the other hand it makes easy to play backwards
;;  - Make it quickload-able
;;  - Add slice support. With start and stop as relative values

(defvar *buffers* (make-hash-table :test #'equal)
  "Global cache for all buffers loaded through (bbufer-load).
   Key is the (file-namestring). Example: \"sample.wav\"
   Value is the actual buffer object.")
(defvar *sample-words* (make-hash-table :test #'equal))
(defvar *instruments* (make-hash-table))
(defvar *buffer-sets* (make-hash-table :test #'equal)
  "stores the sets of each buffer, KEY is the file-namestring VALUE
   is an (N 2) array")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DSPs helpers - These assume a blocksize of 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-vug buffer-loop-play ((buffer buffer)
                              rate
                              start-pos
                              loopstart loopend)
  (with-samples ((frame (phasor-loop rate start-pos loopstart loopend)))
    (buffer-read buffer frame :interpolation :cubic)))

(dsp! play-lsample-f ((buf buffer) dur amp start-pos loopstart loopend rate
                      left right
                      lpf hpf bpf
                      lpr hpr bpr
                      reson resonq
                      (downsamp fixnum)
                      atk sus rel)
  "Plays given buffer for DUR in seconds. If greater than buffer loops
   from LOOPSTART to LOOPEND frame. It can be used to play an instrument
   sample and sustain the looped part. Or to play a slice of audio alone from
   the rest of the buffer."
  (:defaults nil 1 .5 0 0 0 1
             1 1
             0 0 0
             1 1 1
             0 2
             0
             0 .9 .1)
  (with-samples ((in (buffer-loop-play buf rate start-pos loopstart loopend))
                 (in (* in (envelope (make-envelope '(0 1 1 0) (list atk sus rel))
                                     1 dur #'incudine:free))))
    (incudine.vug:maybe-expand in)
    ;;
    (unless (= 0d0 reson)
      (setf in (incudine.vug:reson in reson resonq)))
    ;;
    (unless (= 0 downsamp)
      (setf in (incudine.vug:downsamp downsamp in)))
    ;;
    (unless (= 0d0 lpf)
      (setf in (incudine.vug:lpf in lpf lpr)))
    (unless (= 0d0 hpf)
      (setf in (incudine.vug:hpf in hpf hpr)))
    (unless (= 0d0 bpf)
      (setf in (incudine.vug:hpf in bpf bpr)))
    (out (* amp (* left in))
         (* amp (* right in)))))

(dsp! bplay ((buf buffer)
             rate start-pos
             (loop-p boolean) amp left right custom-id
             lpf hpf bpf
             lpr hpr bpr end-frame
             (downsamp fixnum) reson resonq)
  "Plays given buffer without control for how long is played, it gracefully
   accepts negative rates or audio filters."
  (:defaults (incudine:incudine-missing-arg "BUFFER")
      1 0
      NIL 1 1 1 1
      0 0 0
      2 2 2 0
      0 0 2)
  (with-samples
      ((in (buffer-play buf rate start-pos loop-p #'incudine:free end-frame)))
    (incudine.vug:maybe-expand in)
    (unless (= 0d0 reson)
      (setf in (incudine.vug:reson in reson resonq)))
    ;;
    (unless (= 0 downsamp)
      (setf in (incudine.vug:downsamp downsamp in)))
    ;;
    (unless (= 0d0 lpf)
      (setf in (incudine.vug:lpf in lpf lpr)))
    (unless (= 0d0 hpf)
      (setf in (incudine.vug:hpf in hpf hpr)))
    (unless (= 0d0 bpf)
      (setf in (incudine.vug:hpf in bpf bpr)))
    (out (* amp (* left in))
         (* amp (* right in)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun list-buffers ()
  (alexandria:hash-table-keys *buffers*))

(defun clean-buffers ()
  (alexandria:maphash-values (lambda (x) (incudine:free x)) *buffers*)
  (clrhash *buffers*))

(defun list-words ()
  (alexandria:hash-table-keys *sample-words*))

(defun node-alive (id)
  (node-id (node id)))

(defun amp-node
    (time id &key (center 1f0) (radius .1) (period 1) (life 3))
  (declare (sample time) (integer id))
  "sends random rate changes to the node ID
   LIFE is needed to avoid early check of the node"
  (let ((alive (node-id (node id))))
    (when (or alive (> life 0))
      (set-control
       id
       :amp (funcall (cm:pick '+ '+ '+ '+ '+ '+ '-)
                      (cosr center radius 10)))
      (aat (+ time (* *sample-rate* period))
           #'amp-node it id :life (- life 1)))))

(defun corrupt-node
    (time id &key (center 1f0) (radius .1) (period 1) (life 3))
  (declare (sample time) (integer id))
  "sends random rate changes to the node ID
   LIFE is needed to avoid early check of the node"
  (let ((alive (node-id (node id))))
    (when (or alive (> life 0))
      (set-control
       id
       :rate (funcall (cm:pick '+ '+ '+ '+ '+ '+ '-)
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

(defun bbuffer-load (filename &optional alias-key)
  "buffer-load wrapper for global hash caching and easy access
  FILENAME is a normal string of a path where to load the file
  (bbuffer-load \"/home/sendai/curso/furi-dream-cc.wav\")"
  (declare (string filename))
  (let* ((hkey (if alias-key
                   alias-key
                   (file-namestring filename)))
         (buf  (gethash hkey *buffers*)))
    (if (not buf)
        (setf (gethash hkey *buffers*)
              (buffer-load filename))
        buf)))

;;--------------------------------------------------
;; Single buffer play

(defun bbplay
    (buf &key (rate 1d0 rate-p) (rpitch 0 rpitch-p) (beat-length 1f0 beat-length-p)
           (start-pos 0d0) (loop-p nil) (amp 1d0) (id 2 id-p)
           (lpf 0) (hpf 0) (bpf 0)
           (left 1d0) (right 1d0) (downsamp 1 downsamp-p) (pan .5 pan-p)
           (lpr 2) (hpr 2) (bpr 2)
           (reson 0) (resonq 2)
           (set 0 set-p))
  (declare (fixnum rpitch)
           (boolean loop-p)
           (number lpf hpf bpf lpr hpr bpr pan
                   reson resonq
                   rate beat-length
                   amp left right)
           (unsigned-byte downsamp set id))
  "plays the provided buffer either by
   PAN value between 0f0 and 1f0
   RATE plays the buffer to play at rate
   RPITCH bends the whole buffer rate to play to the new pitch offset
   BEAT-LENGTH stretch the whole buffer to play for N beats"
  (when (or (symbolp buf) (stringp buf))
    (let ((b (gethash buf *buffers*)))
      (when b
        (setf buf b))))
  ;; TODO: big check so we can send nil to beat-length without crashing
  (when (incudine:buffer-p buf)
    (let* ((sample-rate (buffer-sample-rate buf))
           (frames (buffer-frames buf))
           (sets (if set-p
                     (gethash (file-namestring (buffer-file buf))
                              *buffer-sets*)
                     NIL))
           (set  (if (and set-p sets)
                     (mod set (array-dimension sets 0))
                     NIL))
           (start-pos (if (and set-p sets)
                          (aref sets set 0)
                          start-pos))
           (end-pos   (if (and set-p sets)
                          (aref sets set 1)
                          frames))
           (dur (/ (- end-pos start-pos) sample-rate)))

      (when rpitch-p
        (mulf rate (pitch-to-ratio rpitch)))

      (when beat-length-p
        ;; need to calculate the total duration in sec
        ;; to provide it to (beat-to-ratio
        (mulf rate (beat-to-ratio beat-length
                                  sample-rate
                                  dur)))

      (setf dur (/ dur (abs rate)))

      ;; "hack" to work with buffer-play vug
      (when (< rate 0)
        (setf start-pos (- frames (/ frames 20))))

      (when pan-p
        (setf pan (alexandria:clamp pan 0f0 1f0))
        (cond ((> pan .5) (decf left  (* 2 (- pan .5))))
              ((< pan .5) (decf right (* 2 (abs (- pan .5)))))))

      (if (and set-p sets)
          (if id-p
              (play-lsample-f buf dur amp start-pos :rate rate
                                                    :lpf lpf :hpf hpf :bpf bpf
                                                    :lpr lpr :hpr hpr :bpr bpr
                                                    :left left :right right
                                                    :reson reson :resonq resonq
                                                    :sus 1 :rel 0
                                                    :downsamp downsamp
                                                    :id id)
              (play-lsample-f buf dur amp start-pos :rate rate
                                                    :lpf lpf :hpf hpf :bpf bpf
                                                    :lpr lpr :hpr hpr :bpr bpr
                                                    :reson reson :resonq resonq
                                                    :sus 1 :rel 0
                                                    :downsamp downsamp
                                                    :left left :right right))
          (if id-p
              (bplay buf rate start-pos loop-p amp left right
                     :lpf lpf :hpf hpf :bpf bpf
                     :lpr lpr :hpr hpr :bpr bpr
                     :reson reson :resonq resonq
                     :downsamp downsamp
                     :id id)
              (bplay buf rate start-pos loop-p amp left right
                     :lpf lpf :hpf hpf :bpf bpf
                     :lpr lpr :hpr hpr :bpr bpr
                     :reson reson :resonq resonq
                     :downsamp downsamp)))
      dur)))

;;------------------------------------------------------
;; Description: These one help slicing buffers into smaller pieces and
;; reference those by a shortname.
;;
;; Slice of a single buffer, dedicated to words or phrases
(defstruct phrase
  "file-namestring and time in seconds at which start playing
   and for how long"
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
              (loop-p nil) (amp 1f0) (id 2 id-p)
              corrupt
              (downsamp 0 downsamp-p)
              (left 1f0) (right 1f0) (pan 1f0 pan-p)
              (lpf 0) (hpf 0) (bpf 0)
              (lpr 1) (hpr 1) (bpr 1)
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
           ;; Seconds
           (str (phrase-start-at obj))
           (dur (phrase-dur obj))
           (buf (gethash (phrase-filename obj) *buffers*))
           (sample-rate (buffer-sample-rate buf))
           ;; Frames
           (start-pos (* sample-rate str))
           (end-pos   (+ start-pos (* sample-rate dur))))

      ;; pitch to ratio - from sonic-pi
      (when rpitch-p
        (mulf rate (pitch-to-ratio rpitch)))

      ;; Update duration now based on the rate
      (setf dur (/ dur (abs rate)))

      ;; play for beat-length
      (when beat-length-p
        (mulf rate (beat-to-ratio beat-length
                                  sample-rate
                                  dur)))

      ;; change where to start when played backwards
      (when (< rate 0)
        (rotatef start-pos end-pos))

      (when pan-p
        (setf pan (alexandria:clamp pan 0f0 1f0))
        (cond ((> pan .5) (decf left  (* 2 (- pan .5))))
              ((< pan .5) (decf right (* 2 (abs (- pan .5)))))))

      ;; start voice
      (if id-p
          (play-lsample-f buf dur amp start-pos
                          :downsamp downsamp
                          :rate rate
                          :left left
                          :right right
                          :lpf lpf :hpf hpf :bpf bpf
                          :lpr lpr :hpr hpr :bpr bpr
                          :id id)
          (play-lsample-f buf dur amp start-pos
                          :downsamp downsamp
                          :rate rate
                          :left left
                          :right right
                          :lpf lpf :hpf hpf :bpf bpf
                          :lpr lpr :hpr hpr :bpr bpr
                          :amp amp))
      (when corrupt
        (corrupt-node (now)
                      id
                      :center corrupt-center
                      :radius corrupt-radius
                      :period corrupt-period))
      T))) ;; ?

;;--------------------------------------------------
;; An instrument is composed of different sound files
;; that can be referenced by calling one function with
;; different pitches. They also might need a sustain to
;; play longer than designed without change pitch.
;; NOTE: Using some of ormf's code.
;;--------------------------------------------------

(defclass instrument ()
  ((name  :initarg :name
          :documentation "relative filename path")
   (dir   :initarg :dir
          :documentation "absolute directory path where to look for samples")
   (keys  :initarg :keys :initform (make-array 127 :initial-element nil)
          :documentation "stores key class elements")))

(defun make-instrument (name directory)
  (declare (symbol name) (string directory))
  (unless (gethash name *instruments*)
    (setf (gethash name *instruments*)
          (make-instance 'instrument
                         :name name
                         :dir directory))))

(defun list-instruments ()
  (alexandria:hash-table-keys *instruments*))

(defun clean-instruments ()
  (clean-buffers)
  (clrhash *instruments*))

;;--------------------------------------------------

(defclass key ()
  ((filename  :initarg :filename
              :documentation
              "just filename without path, used to lookup into *buffers* hash")
   (keynum    :initarg :keynum :documentation "midi keynum")
   (startpos  :initarg :startpos :documentation "initial start position")
   (loopstart :initarg :loopstart :documentation "loop start position")
   (loopend   :initarg :loopend :documentation "loop end position")))

(defun get-pure-keys (keys)
  "Returns a list of keys that match the array index and thus
   won't be pitchbended."
  (loop
     :for i :from 0
     :for ckey :across keys :when (and ckey (= i (slot-value ckey 'keynum)))
     :collect i))

(defun get-new-mappings-hash (akeys)
  (loop
     :for (x y) :on (get-pure-keys akeys)
     :with first-p = T
     :with hkeys = (make-hash-table)
     :finally (return hkeys)
     :do
     (cond ((and first-p x y)
            (setf first-p nil)
            ;; At least 2 notes
            (appendf (gethash x hkeys)
                     (append (iota x)
                             (iota (round (/ (- y x) 2))
                                   :start (+ 1 x))))
            (appendf (gethash y hkeys)
                     (iota (- y (+ x (round (/ (- y x) 2))))
                           :start (+ x (round (/ (- y x) 2))))))
           ;; Only 1 note
           ((and first-p x)
            (appendf (gethash x hkeys)
                     (append (iota x :start 0)
                             (iota (- 126 x) :start (1+ x)))))
           ;; Middle
           ((and x y)
            (appendf (gethash x hkeys)
                     (iota (round (/ (- y x) 2))
                           :start x))
            (appendf (gethash y hkeys)
                     (iota (round (/ (- y x) 2))
                           :start (+ x (round (/ (- y x) 2))))))
           ;; Final sequence
           ((and (not y) (not first-p) x)
            (appendf (gethash x hkeys)
                     (iota (- 126 x) :start (1+ x)))))))

(defun fill-notes (keynum iname)
  (let* ((akeys (slot-value (gethash iname *instruments*) 'keys)))
    (loop :for k :in (gethash keynum (get-new-mappings-hash akeys))
       :do (setf (aref akeys k) (aref akeys keynum)))))

(defun push-note (iname keynum filename &optional (startpos 0) loopstart loopend)
  (declare (symbol iname) (unsigned-byte keynum) (string filename))
  (when-let ((instrument (gethash iname *instruments*)))
    (with-slots (dir keys) instrument
      (let* ((fullpath (concatenate 'string dir filename))
             (buf (bbuffer-load fullpath))
             (buf-size (buffer-size buf)))
        (setf loopend   buf-size
              loopstart buf-size)
        (setf (aref keys keynum)
              (make-instance 'key
                             :filename filename
                             :keynum keynum
                             :startpos startpos
                             :loopstart loopstart
                             :loopend loopend))
        (fill-notes keynum iname)))))

(defun push-note-parsed (iname filename &optional (startpos 0) loopstart loopend)
  (declare (symbol iname) (string filename))
  (when-let ((instrument (gethash iname *instruments*))
             (keynum (note-name-to-midi-number filename)))
    (with-slots (dir keys) instrument
      (let* ((fullpath (concatenate 'string dir filename))
             (buf (bbuffer-load fullpath))
             (buf-size (buffer-size buf)))
        (setf loopend   buf-size
              loopstart buf-size)
        (setf (aref keys keynum)
              (make-instance 'key
                             :filename filename
                             :keynum keynum
                             :startpos startpos
                             :loopstart loopstart
                             :loopend loopend))
        (fill-notes keynum iname)))))

(defun load-instrument (iname)
  (when-let ((instrument (gethash iname *instruments*)))
    (with-slots (dir) instrument
      (loop :for f :in (directory (concatenate 'string dir "*.wav"))
         :do (push-note-parsed iname (file-namestring f))))))

(defun play-instrument (name nkeynum
                        &key (dur 1) (amp 1) (rpitch 0)
                          (id 222 id-p)
                          (downsamp 0)
                          (reson 0)
                          (left 1) (right 1)
                          (lpf 0) (hpf 0) (bpf 0)
                          (lpr 1) (hpr 1) (bpr 1))
  (declare (symbol name) (integer nkeynum) (number dur amp rpitch))
  (with-slots (keys) (gethash name *instruments*)
    (with-slots (filename loopend loopstart keynum) (aref keys nkeynum)
      (let* ((buf (gethash filename *buffers*))
             (relative-pitch (- nkeynum keynum))
             (rate (pitch-to-ratio (if (not (= 0 rpitch))
                                       rpitch
                                       relative-pitch))))
        (if id-p
            (play-lsample-f buf dur amp
                            :id id
                            :rate rate
                            :downsamp downsamp
                            :reson reson
                            :lpf lpf :hpf hpf :bpf bpf
                            :lpr lpr :hpr hpr :bpr bpr
                            :left left :right right)
            (play-lsample-f buf dur amp
                            :rate rate
                            :downsamp downsamp
                            :reson reson
                            :lpf lpf :hpf hpf :bpf bpf
                            :lpr lpr :hpr hpr :bpr bpr
                            :left left :right right))))))
