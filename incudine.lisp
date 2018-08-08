(in-package :somecepl)

;; Needs (rt-start) first.

(dsp! bplay
    ((buf buffer) rate start-pos
     (loop-p boolean) attenuation rms (cfreq fixnum))
  (:defaults 0d0 1 0 nil .00001 0 10)
  (with-samples
      ((in (incudine.vug:buffer-play
            buf rate start-pos loop-p #'stop))
       (inn (* in attenuation))
       (inn (+ inn
             ;; (incudine.vug:downsamp 10 inn)
             ;; (incudine.vug:lpf inn 100 1)
             ;; (incudine.vug:hpf inn 200 10)
             ;; (incudine.vug:butter-hp inn 1000)
;;             (incudine.vug:downsamp cfreq inn)
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
;;  - Make it quickload-able
;;  - Add slice support. With start and stop as relative values

(defvar *buffers* (make-hash-table :test #'equal))
(defvar *sample-words* (make-hash-table :test #'equal))

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

(defun bbplay (buf &key (rate 1) (rpitch 0 rpitch-p)
                     (beat-length 4 beat-length-p)
                     (loop-p nil) (attenuation 2) (id 2 id-p))
  (declare (buffer buf) (integer rpitch id) (boolean loop-p))
  "plays the provided buffer either by
   RATE plays the buffer to play at rate
   RPITCH bends the whole buffer rate to play to the new pitch
   BEAT-LENGTH stretch the whole buffer to play for N beats"
  (if rpitch-p
      (setf rate (pitch-to-ratio rpitch)))
  (if beat-length-p
      ;; need to calculate the total duration in sec
      ;; to provide it to (beat-to-ratio
      (let* ((sample-rate (buffer-sample-rate buf))
             (size (buffer-size buf))
             (dur (/ size sample-rate)))
        (setf rate (beat-to-ratio beat-length
                                  sample-rate
                                  dur))))
  (if id-p
      (bplay buf rate 0 loop-p
          :attenuation attenuation
          :id id)
      (bplay buf rate 0 loop-p
          :attenuation attenuation)))

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
                    (beat-length 1 beat-length-p) (id 2))
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
      ;; kill voice
      (let ((dur (* (/ rate) dur)))
        (at (+ (now) #[dur b]) #'free (node id))))))


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
   (keys   :initarg :keys)))

(defun make-instrument (name directory)
  (declare (string directory) (symbol name))
  (setf (gethash name *instruments*)
        (make-instance 'instrument
                       :name name
                       :dir directory
                       :keys (make-array 127))))

(define-vug buffer-loop-play
    ((buffer buffer) rate start-pos loopstart loopend)
  (with-samples
      ((frame (phasor-loop rate start-pos loopstart loopend)))
    (buffer-read buffer frame :interpolation :cubic)))

(dsp! play-lsample-f ((buf buffer) dur amp loopstart loopend rate)
  (:defaults nil 1 1 0 0 1)
  (stereo (* amp
             (envelope (make-envelope '(0 1 1 0) '(0 .9 .1))
                       1 dur #'incudine:stop)
             (buffer-loop-play buf rate 0 loopstart loopend))))

(defclass key ()
  ((filename :initarg :filename)
   (keynum :initarg :keynum)
   (startpos :initarg :startpos)
   (loopstart :initarg :loopstart)
   (loopend :initarg :loopend)))

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

(defun play-instrument (name keynum &optional (dur 1) (amp 1) (rate 1))
  (with-slots (keys) (gethash name *instruments*)
    (with-slots (filename loopend loopstart) (aref keys keynum)
      (let ((buf (gethash filename *buffers*)))
        (play-lsample-f buf dur amp loopstart loopend rate)))))

;;::::::::::::::::::::::::::::::::::::::::::::::::::
;; Game-Music-Emu - cl-gme - helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is a way to load and play around with gme
;; Another approach possible might be load the whole file as many times
;; as voices are on the file and then use different helpers to play them.
;; Might be like that the transitions between adding voices and removing them
;; would be more seemles...dunno.

;; TODO: shorted hash keys

(ql:quickload :cl-gme/incudine)

(defparameter *loading* (make-hash-table :test #'equal))
(defparameter *playing* (make-hash-table :test #'equal))

(defun gmeclean ()
  "frees global hashes of buffers and recreates them"
  (loop :for key :in (alexandria:hash-table-keys *playing*)
     :do (incudine:free (gethash key *playing*)))
  (setf *playing* (make-hash-table :test #'equal))
  (loop :for key :in (alexandria:hash-table-keys *loading*)
     :do (incudine:free (gethash key *loading*)))
  (setf *loading* (make-hash-table :test #'equal)))

(defun gmebuffer (filename &key (len 1) (track-number 0)
                             (rate 44100) (offset 0)
                             (voices '()))
  "returns a incudine buffer with the gme FILENAME loaded into it"
  (let* ((frames (* len    10 4410)) ;; need to be multiple of 4410
         (offset (* offset 10 4410)) ;; it's hardcoded on the .c util...
         (buf    (make-buffer (/ frames 2)
                              :channels 2
                              :sample-rate rate)))
    (cl-gme:with-track
        (gmefile filename track-number :rate rate :voices voices)
      (cl-gme::gmefile-to-buffer (buffer-data buf)
                                 (cffi:mem-ref gmefile :pointer)
                                 frames
                                 offset))
    buf))

(defun gmeplay (filename node track-number
                &key (attenuation .00001) (rate 1f0) (start-pos 0)
                  (fade-curve 3) (fade-time 0f0)
                  (length 1) (offset 0) (voices '())
                  (load-only nil))
  "plays provided FILENAME TRACK-NUMBER in NODE id,
   takes care of loading the file and swapping buffers for live coding

   LOAD-ONLY just put the buffer in the global hash, do not play it

   (gmeplay \"/home/sendai/Downloads/sf2/ff3.nsf\" 2 15
         :attenuation .00002
         :length 20
         :offset 10
         :voices '(2)
         :rate 1
         :fade-time 1
         :fade-curve 2)"
  (declare (integer node track-number length offset))
  (let ((alive   (node-id (node node)))
        (hashkey (concatenate 'string filename (write-to-string node))))
    (setf (gethash hashkey *loading*)
          (gmebuffer filename
                     :track-number track-number
                     :len length
                     :voices voices
                     :offset offset))
    (unless load-only
      (if alive
          (set-controls
           node
           :buf (gethash hashkey *loading*)
           :rate rate
           :fade-curve fade-curve
           :fade-time fade-time
           :attenuation attenuation)
          (bplay (gethash hashkey *loading*) rate 0 t
                 :id node
                 :fade-curve fade-curve
                 :fade-time fade-time
                 :attenuation attenuation)))
    (rotatef (gethash hashkey *loading*)
             (gethash hashkey *playing*))
    (incudine:free (gethash hashkey *loading*))))
