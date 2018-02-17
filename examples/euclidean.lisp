(in-package :somecepl)

;; ----------------------------
;; Euclidean Rythms
;; https://github.com/overtone/overtone/blob/master/src/overtone/examples/compositions/euclidean_rhythms.clj
;; ----------------------------

(setf (bpm *tempo*) 60)

(defun eu (time vel chan beat rythm notes)
  ;; If rythm is 1 we play the note
  (if (= 1 (first rythm))
      (let ((note (first notes)))
        (play-midi-note time
                        (cm:keynum note)
                        (round (cosr vel 5 .4))
                        beat
                        chan)
        (setf notes (alexandria:rotate notes 1))))
  (aat (+ time #[beat b]) #'eu
       it
       vel
       chan
       beat
       (alexandria:rotate rythm 1)
       notes))

(flush-pending)

(eu (tempo-sync #[1 b])   50 0 1/2 (bjorklund 5 13) '(:c3 :g3 :d3))
(eu (tempo-sync #[2 b])   40 1 1   (bjorklund 4 4)  '(:c3 :g3 :d3))
(eu (tempo-sync #[2.5 b]) 40 2 1/2 (bjorklund 3 8)  '(:c3 :g3 :d3))

(eu (tempo-sync #[2.5 b]) 50 2 1/2 (bjorklund 5 12)  '(:c3 :g3 :d3))
(eu (tempo-sync #[2 b])   50 1 1   (bjorklund 4 12)  '(:c3 :g3 :d3))

(eu (funcall *metro* (funcall *metro* 'get-beat 1.0)) 60 0 1/2 (bjorklund 5 13) '(:c3 :g3 :d3))
(eu (funcall *metro* (funcall *metro* 'get-beat 2.0)) 50 1 1   (bjorklund 4 4)  '(:c3 :g3 :d3))
(eu (funcall *metro* (funcall *metro* 'get-beat 2.5)) 50 2 1/2 (bjorklund 3 8)  '(:c3 :g3 :d3))
