(in-package :somecepl)

;; ----------------------------
;; Euclidean Rythms
;; https://github.com/overtone/overtone/blob/master/src/overtone/examples/compositions/euclidean_rhythms.clj
;; ----------------------------

(defvar *metro* nil)
(setf *metro* (make-metro 90))
(setf (bpm *tempo*) 60)

(defun eu (beat time vel chan r rhythm notes)
  (let ((n-beat (+ beat r))
        (note   (first notes)))
    (when (= (first rhythm) 1)
      (p
       time
       note
       (rcosr vel 3 1/2)
       (cosr 1 .6 3/4)
       chan))
    (aat (funcall *metro* n-beat) #'eu
         n-beat it vel chan r
         (alexandria:rotate rhythm -1)
         (alexandria:rotate notes -1))))

(eu (funcall *metro* 'get-beat 4)
    (funcall *metro* (funcall *metro* 'get-beat 4))
    30
    1
    1
    (bjorklund 4 4) '(60 62 64))

(eu (funcall *metro* 'get-beat 4)
    (funcall *metro* (funcall *metro* 'get-beat 4))
    40
    2
    1/2
    (bjorklund 3 8) '(66 68 70))

(fluidsynth:program-change *synth* 1 1)

(flush-pending)

(eu (funcall *metro* 'get-beat 4)
    (funcall *metro* (funcall *metro* 'get-beat 4))
    40
    2
    1/2
    (bjorklund 5 12) '(66 68 70))

(eu (funcall *metro* 'get-beat 4)
    (funcall *metro* (funcall *metro* 'get-beat 4))
    40
    2
    1/2
    (bjorklund 4 12) '(66 68 70))
