(in-package :somecepl)

;; --------------------------------------------------------------------
;; Gloriette for John Cage - Translated from notes from metalevel
;; TODO: stop using a global and try to use a local variable instead
;; --------------------------------------------------------------------
(defun play-midi-note (time pitch velocity dur c)
  (when (and
         (not (equal pitch "r"))
         (> pitch 0)
         (> velocity 0)
         (> dur 0))
    (progn
      (at time #'fluidsynth:noteon
          *synth* c pitch velocity)
      (at (+ time #[dur b]) #'fluidsynth:noteoff
          *synth* c pitch))))


(defvar *c0* 0)
(setf *c0* 0)

(defun bird (time offset vel chan p q i &optional (w 0))
;;  (if (= chan 3) (setf vel 40))
;;  (if (not (= i 200))
  (let* ((i    (1+ i))
         (note (cm:next p))
         (dur  (cm:next q))
         (mul  12))
    (setf *c0* (cm:interp i 0 .5 90 4))
    ;; (setf *c0* 0)
    ;; (setf i 0)
    (play-midi-note
     time
     (cm:keynum (cm:transpose note offset))
     vel
     (* dur (- mul 3) )
     chan))
  (aat (tempo-sync #[(* mul dur) b]) #'bird it offset vel chan p q i w)))

(defun cage (offset vel chan)
  (let* ((d (cm:pval *c0*))
         (p (cm:new cm:weighting :of `((g3 :weight ,d)
                                       (a3 :weight ,d)
                                       bf3
                                       (c4 :weight ,d)
                                       d4
                                       (e4 :weight ,d)
                                       f4
                                       (g4 :weight ,d)
                                       (r :max 1 :weight .25))
                    :for 1))
         (q (cm:new cm:weighting :of (list 1/16
                                           1/8
                                           (cm:new cm:cycle :of 1/32 :for 2)))))
    (aat (tempo-sync #[1 b]) #'bird it offset vel chan p q 0)))

;; 10 violin
;; 11 pizzicato (?)
;; 15 wind

(fluidsynth:program-change *synth* 3 10)
(fluidsynth:program-change *synth* 1 33)
(fluidsynth:program-change *synth* 2 10)

(fluidsynth:program-change *synth* 3 26)
(fluidsynth:program-change *synth* 1 77)
(fluidsynth:program-change *synth* 2 33)

(at (funcall *metro* (funcall *metro* 'get-beat 4))    #'cage -12 40 3)
(at (funcall *metro* (funcall *metro* 'get-beat 4.5))  #'cage 0 45 1)
(at (funcall *metro* (funcall *metro* 'get-beat 1.75)) #'cage 12 45 2)

(at (tempo-sync #[4 b]) #'cage -12 40 3)
(at (tempo-sync #[4.5 b]) #'cage 0 45 1)
(at (tempo-sync #[1.75 b]) #'cage 12 45 2)

(cage   0 50 1)
(cage  12 30 2)

(progn
  (at (tempo-sync #[1 b])  #'cage -12 30 3)
  (at (tempo-sync #[32 b]) #'cage 0 50 1)
  (at (tempo-sync #[64 b]) #'cage 12 40 2))

(flush-pending)
(off-with-the-notes *synth*)

#||
(process repeat 100
             for n = (next p)
             for r = (rhythm (next q) 65)
             for i from 0
             set w = (interp i 0 .5 90 4)
             output (new midi :time (now)
                         :duration r 
                         :keynum (transpose n offset))
             wait r)
||#

