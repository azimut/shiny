(in-package :shiny)

;;--------------------------------------------------
;;(ql:quickload :cl-gme/incudine)

(defun gmeplay-beat
    (filename node track-number
     &key (amp 1) (rate 1f0) (start-pos 0)
       (fade-curve 3) (fade-time 0f0) (loop-p t)
       (left 1f0) (right 1f0)
       (length 1) (offset 0) (voices '())
       ;; Filters
       (lpf 0d0) (hpf 0d0) (bpf 0d0)
       ;; controlers
       load-only control-only aubio-p)
  (declare (integer node track-number length offset) (boolean load-only))
  (let ((alive   (node-id (node node)))
        (hashkey (concatenate 'string filename (write-to-string node))))
    ;; fill empty slot
    (unless control-only
      (setf (gethash hashkey *loading*)
            (gmebuffer filename
                       :track-number track-number
                       :len length
                       :voices voices
                       :offset offset))
      (when aubio-p
        (multiple-value-bind (_ frames)
            (test-beats (gethash hashkey *loading*))
          (declare (ignore _))
          (let* ((frames (remove-sides frames))
                 (start-frame (first frames))
                 (end-frame (lastcar frames)))
            (setf (gethash hashkey *loading*)
                  (slice-buffer (gethash hashkey *loading*)
                                start-frame end-frame))))))

    (when alive 
      (if control-only
          (at (tempo-sync #[1 b])
              #'set-controls
              node
              :rate rate
              :loop loop-p
              :left left
              :right right
              :lpf lpf
              :hpf hpf
              :bpf bpf
              :fade-curve fade-curve
              :fade-time fade-time
              :amp amp)
          (at (tempo-sync #[1 b])
              #'set-controls
              node
              :buf (gethash hashkey *loading*)
              :rate rate
              :loop loop-p
              :left left
              :right right
              :lpf lpf
              :hpf hpf
              :bpf bpf
              :fade-curve fade-curve
              :fade-time fade-time
              :amp amp)))
    
    (unless load-only
      (at (tempo-sync #[1 b])
          #'bplay
          (gethash hashkey *loading*) rate 0 loop-p
          :id node
          :left left
          :hpf hpf
          :lpf lpf
          :bpf bpf
          :custom-id node
          :right right
          :fade-curve fade-curve
          :fade-time fade-time
          :amp amp))
    
    (unless control-only
      (rotatef (gethash hashkey *loading*)
               (gethash hashkey *playing*))
      (incudine:free (gethash hashkey *loading*)))))


(progn
  (incudine.external:foreign-zero-sample (buffer-data *buf1*) 512)
  (incudine.external:foreign-zero-sample (buffer-data *buf2*) 512)
  (incudine.external:foreign-zero-sample (buffer-data *buf3*) 512)
  (incudine.external:foreign-zero-sample (buffer-data *buf4*) 512))

(incudine:free (node 0))
(gmeclean)
(defparameter *gmesong*
  "/home/sendai/Downloads/sf2/contra.nsf")
(defparameter *gmetrack* 2)
(defparameter *gmerate* .7)

(gmeplay-beat
 *gmesong* 1 *gmetrack*
 :amp .1
 :aubio-p t
 :length 40 :offset 70
 :voices '(0)
 :rate *gmerate*)

(gmeplay-beat
 *gmesong* 2 *gmetrack*
 :amp .1 :aubio-p t :length 40 :offset 70
 :voices '(1)
 :rate *gmerate*)

(gmeplay-beat
 *gmesong* 3 *gmetrack*
 :amp .4 :aubio-p t
 :length 40 :offset 30 :left .2
 :voices '(2)
 :rate *gmerate*)

(gmeplay-beat
 *gmesong* 4 *gmetrack*
 :amp .3 :aubio-p t :length 40 :offset 30 :right .2
 :voices '(3)
 :rate *gmerate*)

;;--------------------------------------------------
;;--------------------------------------------------
;;--------------------------------------------------
(gmeplay-beat
 "/home/sendai/Downloads/sf2/mother.nsf" 3 19
 :amp .01
 :aubio-p t
 :length 30
 :offset 20
 :right .2
 :voices '(1)
 :rate .7)

;;--------------------------------------------------

(gmeplay-beat
 "/home/sendai/Downloads/sf2/ff3.nsf" 3 13
 :amp .05
 :aubio-p t
 ;;         :load-only t
 ;;         :loop-p nil
 :length 20
 :offset 10
 :left .2
 :voices '(2)
 :rate .7)

(gmeplay-beat
 "/home/sendai/Downloads/sf2/ff3.nsf" 2 13
 :amp .01
 :aubio-p t
 :load-only t
 ;;         :loop-p nil
 :length 20
 :offset 70
 :voices '(3 1)
 :rate .7)w

;;--------------------------------------------------


(incudine:free (node 0))

(gmeplay-beat
 "/home/sendai/Downloads/chrono/111 Secret of the Forest.spc" 2 1
 :amp .5
 :aubio-p t
 :control-only t
;; :load-only t
 :length 20
 :offset 80
 :lpf 200
;; :right .2
;; :left .3
;; :voices '(4 5 6 7)
 :voices '(0 1 2 3)
 :rate .5)

(incudine:free (node 2))
(gmeplay-beat
 "/home/sendai/Downloads/chrono/111 Secret of the Forest.spc" 3 9
 :amp .2
;; :control-only t
;; :aubio-p t
 :load-only t
 :length 30
 :hpf 400
 :lpf 400
 :offset 120
 :left .9
 :voices '(0)
 :rate 1)

(gmeplay-beat
 "/home/sendai/Downloads/chrono/111 Secret of the Forest.spc" 4 9
 :amp .2
 :control-only t
;; :aubio-p t
;; :load-only t
 :length 30
 :hpf 400
 :lpf 800
 :offset 120
 :left .9
 :voices '(6 7)
 :rate 1)

;;----------------------------------------
(incudine:free (node 0))
(gmeplay-beat
 "/home/sendai/Downloads/sf2/ff3.nsf" 2 7
 :amp .2
 :aubio-p t
 ;;         :load-only t
 ;;         :loop-p nil
 :length 40
 :offset 10
;; :left .2
 :voices '(2 3)
 :rate .7)

(gmeplay-beat
 "/home/sendai/Downloads/sf2/ff3.nsf" 3 7
 :amp .2
 :aubio-p t
 ;;         :load-only t
 ;;         :loop-p nil
 :length 40
 :offset 70
;; :left .2
 :voices '(2)
 :rate .7)

(gmeclean)
(defparameter *buf*
  (gethash "loop_amen.wav" *buffers*))
(defparameter *buf*
  (gethash "/home/sendai/Downloads/sf2/ff3.nsf3" *playing*))
(defparameter *buf*
  (gethash "/home/sendai/Downloads/chrono/111 Secret of the Forest.spc2" *playing*))

(defparameter *buf*
  (bbuffer-load "/home/sendai/quicklisp/local-projects/aubio/static/loop_amen.wav"))

(defparameter *sets*
  (loop :for (x y)
     :on (multiple-value-bind (_ frames)
             (test-onset *buf*)
           (declare (ignore _))
           (remove-sides frames))
     :when (and x y)
     :collect (list x y)))

(let ((sets (make-heap *sets*)))
  (defun f (time)
    (let* ((set (next sets))
           (start-pos (car set))
           (loopend (lastcar set))
           (rate 1)
           (dur (* (/ 1 rate) (/ (- loopend start-pos) 44100f0))))
      (play-lsample-f
       *buf*
       :amp .1 :start-pos start-pos :loopend loopend
       :dur dur :rate rate)
      (aat (+ time #[dur b]) #'f it))))

(incudine:free (node 0))
(defun f ())
(f (now))
