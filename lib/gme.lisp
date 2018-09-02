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

(ql:quickload :cl-gme/incudine)

;; Flip-flop buffer to avoid cuts while loading AND
;; ...still being able to free old cuts
(defvar *loading* (make-hash-table :test #'equal))
(defvar *playing* (make-hash-table :test #'equal))

(defun list-playing ()
  (map nil #'print (alexandria:hash-table-keys *playing*)))

(defun gmeclean ()
  "frees global hashes of buffers and recreates them"
  (loop :for key :in (alexandria:hash-table-keys *playing*)
     :do (incudine:free (gethash key *playing*)))
  (setf *playing* (make-hash-table :test #'equal))
  (loop :for key :in (alexandria:hash-table-keys *loading*)
     :do (incudine:free (gethash key *loading*)))
  (setf *loading* (make-hash-table :test #'equal)))

(defun gmebuffer (filename
                  &key (len 1) (track-number 0)
                    (rate 44100) (offset 0)
                    (voices '()))
  "returns a incudine buffer with the gme FILENAME loaded into it
  FIXME: need to be multiple of 4410...hardcoded on the .c"
  (let* ((frames (* len    10 4410)) 
         (offset (* offset 10 4410)) 
         (buf    (make-buffer (/ frames 2)
                              :channels 2
                              :sample-rate rate)))
    (cl-gme:with-track
        (gmefile filename track-number
                 :rate rate
                 :voices voices)
      (cl-gme::gmefile-to-buffer (buffer-data buf)
                                 (cffi:mem-ref gmefile :pointer)
                                 frames
                                 offset))
    (incudine:normalize-buffer buf 1f0)))

(defun gmeplay
    (filename node track-number
     &key (attenuation 1) (rate 1f0) (start-pos 0)
       (fade-curve 3) (fade-time 0f0)
       (length 1) (offset 0) (voices '())
       (load-only nil))
  (declare (integer node track-number length offset) (boolean load-only))
  "plays provided FILENAME TRACK-NUMBER in NODE id,
   takes care of loading the file and swapping buffers for live coding

   LOAD-ONLY just put the buffer in the global hash, do not play it

   (gmeplay \"/home/sendai/Downloads/sf2/ff3.nsf\" 2 15
         :attenuation .1
         :length 20
         :offset 10
         :voices '(2)
         :rate 1
         :fade-time 1
         :fade-curve 2)"
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
