(in-package :somecepl)

;; Needs (rt-start) first.

(dsp! bplay ((buf buffer) rate start-pos (loop-p boolean) attenuation rms cfreq)
  (:defaults 0d0 1 0 nil .00001 0 100)
  (with-samples ((in (incudine.vug:buffer-play
                      buf rate start-pos loop-p #'stop))
                 (inn (* in attenuation))
                 (inn (+ inn
;;                        (incudine.vug:lpf inn 500 50)
;;                        (incudine.vug:hpf inn 100 20)
;;                        (incudine.vug:butter-hp inn 1020)
                        )))
    (out inn inn)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Description: These one help slicing buffers into smaller pieces and
;; reference those by a shortname.

;; TODO:
;;  - right now it uses (at) to stop (word-play),
;;    might be buffer-play can support other thing

(defvar *buffers* (make-hash-table :test #'equal))
(defvar *sample-words* (make-hash-table :test #'equal))

(defun bbuffer-load (filename)
  "buffer-load wrapper for global hash caching and easy access
  FILENAME is a normal string of a path where to load the file
  (bbuffer-load \"/home/sendai/curso/furi-dream-cc.wav\")"
  (let ((hkey (file-namestring filename)))
    (when (not (gethash hkey *buffers*))
      (setf (gethash hkey *buffers*)
            (buffer-load filename)))))

(defun bplay-beat (buf beat-length loop-p &rest bplay-args)
  "stretch the whole buffer BUF to play for BEAT-LENGTH,
   useful mostlya with short buffers.
   (buffer-play-beat buf 1 nil)"
  (declare (buffer buf) (integer beat-length) (boolean loop-p))
  (let* ((rate (/ (buffer-size buf)
                  #[beat-length b])))
    (apply #'bplay buf rate 0 loop-p :attenuation 2d0 bplay-args)))

(defstruct phrase
  filename
  start-at
  dur)

(defun put-phrase (word filename start-at dur)
  "creates a reference on global hash for WORD in FILENAME
   begining at START-AT for DUR
  (put-phrase \"nothing\" \"furi-better-cc.wav\" 1.7 1.9)"
  (setf (gethash word *sample-words*)
        (make-phrase :filename filename
                     :start-at start-at
                     :dur dur)))

(defun word-play (phrase rate
                  &key (loop-p nil) (attenuation 1f0) (id 2))
  "bplay wrapper to play the phrase provided
  (word-play \"time\" 1)"
  (when phrase
    (let* ((obj (gethash phrase *sample-words*))
           (dur (* (/ rate) (phrase-dur obj)))
           (buf (gethash (phrase-filename obj)
                         *buffers*))
           (start-pos (* (buffer-sample-rate buf)
                         (phrase-start-at obj))))
      ;; start voice
      (bplay buf rate start-pos loop-p
             :attenuation attenuation
             :id id)
      ;; kill voice
      (at (+ (now) #[dur b]) #'free (node id)))))

#|
(bbuffer-load "/home/sendai/Downloads/sample/EM0903.wav")
(bplay-beat (gethash "EM0903.wav" *buffers*) 8 nil)
(put-phrase "nuisance" "EM0903.wav" 20 2)
(word-play "nuisance" .8)
(word-play-beat "nuisance" 3)
|#

(defun word-play-beat (phrase beat-length
                       &key (loop-p nil) (attenuation 1f0)
                         (id 2))
  "play and stretch buffer slice assigned to PHRASE
   for the duration provided

   (word-play-beat \"nuisance\" 3)"
  (when phrase
    (let* ((obj (gethash phrase *sample-words*))
           (buf (gethash (phrase-filename obj)
                         *buffers*))
           ;; in buf sample-rate
           (start-pos (* (buffer-sample-rate buf)
                         (phrase-start-at obj)))
           (samples-to-play
            (* (print (phrase-dur obj))
               (print (buffer-sample-rate buf))))
           ;; to slice sample-rate
           (samples-per-beat (/ samples-to-play
                                beat-length))
           (rate (/ samples-per-beat
                    (buffer-sample-rate buf))))
      ;; start phrase
      (bplay buf rate start-pos loop-p
             :attenuation attenuation
             :id id)
      ;; kill phrase
      (at (+ (now) #[beat-length b]) #'free (node id)))))

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

(defparameter *old* (make-hash-table :test #'equal))
(defparameter *new* (make-hash-table :test #'equal))

(defun gmeclean ()
  "frees global hashes of buffers and recreates them"
  (loop :for key :in (alexandria:hash-table-keys *new*)
     :do (incudine:free (gethash key *new*)))
  (setf *new* (make-hash-table :test #'equal))
  (loop :for key :in (alexandria:hash-table-keys *old*)
     :do (incudine:free (gethash key *old*)))
  (setf *old* (make-hash-table :test #'equal)))

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
    (setf (gethash hashkey *old*)
          (gmebuffer filename
                     :track-number track-number
                     :len length
                     :voices voices
                     :offset offset))
    (unless load-only
      (if alive
          (set-controls
           node
           :buf (gethash hashkey *old*)
           :rate rate
           :fade-curve fade-curve
           :fade-time fade-time
           :attenuation attenuation)
          (bplay (gethash hashkey *old*) rate 0 t
                 :id node
                 :fade-curve fade-curve
                 :fade-time fade-time
                 :attenuation attenuation)))
    (rotatef (gethash hashkey *old*)
             (gethash hashkey *new*))
    (incudine:free (gethash hashkey *old*))))
