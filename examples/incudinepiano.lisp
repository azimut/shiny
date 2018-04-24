(in-package :somecepl)

;(sort (loop for x in (directory (pathname "/home/sendai/Downloads/samples/pianosample/SalamanderGrandPianoV2_44.1khz16bit/44.1khz16bit/*v10*.wav")) collect (note-name-to-midi-number (uiop:unix-namestring x))) #'<)

(defvar *pianosamples* nil)
(defvar env1 (make-envelope '(0 1 0) '(.2 .8)))
(defvar env2 (make-perc .01 .4))
(setf (bpm *tempo*) 120)

;(bplay (aref *pianosamples* 57) 1 0 nil :id 1)
;(loop for i from 1 to 10 do (sleep 1) (a (aref *pianosamples* (nth (random 3) '(54 57 60))) :id 1))

;; ---------------------------

(defun loadpianosamples ()
  (unless *pianosamples*
    (setf *pianosamples* (make-array 122 :initial-element nil)))
  (loop :for x :in
     (directory (pathname "~/Downloads/samples/pianosample/SalamanderGrandPianoV2_44.1khz16bit/44.1khz16bit/*v10*.wav"))
     :collect
     (let* ((filename  (uiop:unix-namestring x))
            (note      (note-name-to-midi-number filename)))
       (setf (aref *pianosamples* note) (buffer-load filename)))))

(loadpianosamples)

;; ---------------------------

(dsp! bplay ((buf buffer) rate start-pos (loop-p boolean))
  (foreach-channel
    (cout (buffer-play buf rate start-pos loop-p #'stop))))

(dsp! a ((buf buffer) atk rel amp)
  (:defaults 0 .01 .4 1.)
  (foreach-channel
    (cout (* amp
            (envelope (make-local-perc atk rel) 1 1 #'stop)
            (cos (* pi (- (phasor .9 0) 0.5d0)))
            (buffer-play buf 1 0 nil #'stop)))))

;; ---------------------------

(defun seq-test-inf (index)
    (dsp-seq (a (aref *pianosamples* index))
             (a (aref *pianosamples* (+ 3 index)))
             (a (aref *pianosamples* (+ 12 index)))
             (seq-test-inf index)))

; trying "markov chains" from extempore demo
(defun seq-test (root)
  (let* ((newroot (random-list (cdr (assoc root '((60 57 54)
                                                  (57 60 54)
                                                  (54 42 60)
                                                  (42 60 57)))))))
  (dsp-seq (a (aref *pianosamples* root)        .01 .4 :id 1)
           (a (aref *pianosamples* (- root 12)) .01 .4 :id 2)
           (a (aref *pianosamples* (+ 12 root)) .01 .4 1.5 :id 3)
           (seq-test newroot))))

(defun seq-test (rep index)
  (when (plusp rep)
    (dsp-seq (bplay (aref *pianosamples* index) 1 0 nil)
             (bplay (aref *pianosamples* (+ (* 3 5) index)) 1 0 nil)
             (bplay (aref *pianosamples* (- (* 3 5) index)) 1 0 nil)
             (seq-test (1- rep) index))))

(defun seq-test (rep index)
  (let* ((root index)
         (newroot (- index 3))))
  (when (plusp rep)
    (dsp-seq (bplay (aref *pianosamples* index) 1 0 nil)
             (bplay (aref *pianosamples* (+ (* 3 5) index)) 1 0 nil)
             (bplay (aref *pianosamples* (- (* 3 5) index)) 1 0 nil)
             (seq-test (1- rep) index))))

(defun phr1 (time)
  (at time #'seq-test 8 200 .3 .5)
  (at (+ time #[2 b]) #'seq-test 6 400 .3 .4)
  (at (+ time #[4 b]) #'seq-test 4 600 .3 .6))

(defun phr1 (time)
  (at time #'seq-test 8 30)
  (at (+ time #[2 b]) #'seq-test 6 45)
  ;(at (+ time #[4 b]) #'seq-test 4 45)
  )


(defun phr1-inf (time)
  (at time #'seq-test-inf 30)
  (at (+ time #[2 b]) #'seq-test-inf 45)
  ;(at (+ time #[4 b]) #'seq-test 4 45)
  )

;; ---------------------------

;; trying "markov chains" from extempore demo
(defun seq-test (root)
  (let* ((newroot (random-list (cdr (assoc root '((60 57 54)
                                                  (57 60 54)
                                                  (54 42 60)
                                                  (42 60 57)))))))
  (dsp-seq (a (aref *pianosamples* root)        .01 .4 :id 1)
           (a (aref *pianosamples* (- root 12)) .01 .4 :id 2)
           (a (aref *pianosamples* (+ 12 root)) .01 .4 1.5 :id 3)
           (seq-test newroot))))

;; ---------------------------

;; Feeding a beat
(defun beatme (root)
  (a (aref *pianosamples* root) :id 1)
  (at (+ (now) #[2 b]) #'beatme 60))

;; Feeding a list of beats / fmsynth.xtm
;; (beatme '(60 63 63 66))
;; (beatme '(60 63 63 66 72 66 84 78 66 69))
(defun beatme (rootl)
  (a (aref *pianosamples* (car rootl)) :id 1)
  (at (+ (now) #[1/2 b]) #'beatme (rotate rootl 1)))

;; Feeding beats with varying amp
(defun beatme (rootl)
  (a (aref *pianosamples* (car rootl)) .01 .4 (cosr 1.5 .2 1) :id 1)
  (at (+ (now) #[1/2 b]) #'beatme (rotate rootl 1)))
