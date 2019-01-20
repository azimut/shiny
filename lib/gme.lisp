(in-package :shiny)

;;::::::::::::::::::::::::::::::::::::::::::::::::::
;; Game-Music-Emu - cl-gme - helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is a way to load and play around with gme
;; Another approach possible might be load the whole file as many times
;; as voices are on the file and then use different helpers to play them.
;; Might be like that the transitions between adding voices and removing them
;; would be more seemles...dunno.

;; TODO: shorted hash keys

;;(ql:quickload :cl-gme/incudine)

;; Flip-flop buffer to avoid cuts while loading AND
;; ...still being able to free old cuts
(defvar *loading* (make-hash-table :test #'equal)
  "temporal cache were new gme tracks are initially loaded
   before being send to a dsp. KEY is the filename concatenated
   with the number of node where is playing. VALUE are the actual
   buffer objects.")
(defvar *playing* (make-hash-table :test #'equal)
  "temporal cache")

(defun list-loading ()
  (alexandria:maphash-keys #'print *loading*))
(defun list-playing ()
  (alexandria:maphash-keys #'print *playing*))

(defun gmeclean ()
  "frees global hashes of buffers and recreates them"
  (alexandria:maphash-values #'incudine:free *playing*)
  (clrhash *playing*)
  (alexandria:maphash-values #'incudine:free *loading*)  
  (clrhash *loading*))

(defun gmebuffer (filename &key (len 1) (track-number 0) (offset 0) voices)
  "returns a incudine buffer with the gme FILENAME loaded into it"
  (assert (probe-file filename))
  (let* ((rate   44100);; v 
         (chunk  4410) ;; This value is hardcoded on the .c file
         (frames (* len    10 chunk)) 
         (offset (* offset 10 chunk)) 
         (buf    (make-buffer (/ frames 2) :channels 2 :sample-rate rate))
         (voices (ensure-list voices)))
    (setf (buffer-file buf) (pathname filename))
    (cl-gme:with-track (gmefile filename track-number :rate rate :voices voices)
      (cl-gme::gmefile-to-buffer (buffer-data buf)
                                 (cffi:mem-ref gmefile :pointer)
                                 frames
                                 offset))
    (normalize-buffer buf .5)))

(defun gmeplay (filename node track-number
                &key (amp 1) (rate 1)
                  (dur 1) (offset 0) voices
                  (left 1) (right 1)
                  (lpf 0) (hpf 0) (bpf 0)
                  (lpr .5) (hpr .5) (bpr .5)
                  (downsamp 0)
                  (loop-p t) load-only
                  (fade-curve 3) (fade-time 0f0))
  "plays provided FILENAME TRACK-NUMBER in NODE id,
   takes care of loading the file and swapping buffers for live coding

   LOAD-ONLY just put the buffer in the global hash, do not play it

   (gmeplay \"/home/sendai/Downloads/sf2/ff3.nsf\" 2 15
         :amp .1
         :dur 20
         :offset 10
         :voices 2
         :rate 1
         :fade-time 1
         :fade-curve 2)"  
  (declare (type integer node track-number dur offset)
           (type number rate amp)
           (type boolean loop-p load-only))
  (let ((alive   (node-id (node node)))
        (hashkey (format nil "~a~a" filename node))
        (voices  (ensure-list voices)))
    (setf (gethash hashkey *loading*)
          (gmebuffer filename
                     :track-number track-number
                     :len dur
                     :voices voices
                     :offset offset))
    (unless load-only
      (if alive
          (set-controls
           node
           :buf (gethash hashkey *loading*)
           :rate rate
           :loop-p loop-p
           :fade-curve fade-curve
           :fade-time fade-time
           :left left
           :right right
           :lpf lpf :hpf hpf :bpf bpf
           :lpr lpr :hpr hpr :bpr bpr
           :downsamp downsamp
           :amp amp)
          (bplay (gethash hashkey *loading*) rate 0 loop-p
                 :id node
                 :left left
                 :right right
                 :lpf lpf :hpf hpf :bpf bpf
                 :lpr lpr :hpr hpr :bpr bpr
                 :downsamp downsamp
                 :fade-curve fade-curve
                 :fade-time fade-time
                 :amp amp)))
    (rotatef (gethash hashkey *loading*)
             (gethash hashkey *playing*))
    (incudine:free (gethash hashkey *loading*))))
