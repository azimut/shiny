(in-package :shiny)

;; --------------------------------------------------------------------
;; The rules for the algorithm are as follows :
;;
;;     two different note lengths need to be defined, e.g. "4" and "3"
;;     a scale needs to be defined, e.g. C major (the white keys on a piano), let's say we start on the E note, the list of notes will then contain : E, F, G, A, B, C
;;     a pattern length needs to be defined, e.g. 4 bars
;;
;; The algorithm will then function like so (keeping the above definitions in mind) :
;;
;;     the first note of the scale (E) is played at the length of the first defined note length (4)
;;     each time the duration of the played note has ended, the NEXT note in the scale (F) is played
;;     once the first pattern length has been reached (4 bars), a new pattern will start
;;     the previously "recorded" pattern will loop its contents indefinitely while the new patterns are created / played
;;     if a newly played note sounds simultaneously with another note from a PREVIOUS pattern, the note length will change (in above example from 4 to 3).
;;     this will be the new note length to use for ALL SUBSEQUENT added notes, until another simultaneously played note is found, leading it to switch back to the previous note length (in above example, back to 4).
;;     as the pattern is now played over an existing one, it is likely that notes will be played in unison, leading to the switching of note length
;;     as more patterns are accumulated, a perfectly mathematical pattern of notes are weaving in and out of the notes of the other patterns
;;
;; https://github.com/igorski/molecular-music-generator
;; --------------------------------------------------------------------

(defvar *mtempos* '())
(defvar *p* '())
(defvar *n* nil)

(defun mbox-show ()
  (loop
     :for i :in (reverse *p*)
     :for ii :below 100
     :do (destructuring-bind (n d c b) i
           (print (format nil "~5A~10A~10A~10A~A" ii n d c b)))))

;; (defun spattern ())
;; (defun ppattern ())

(defun spattern (time notes pattern lengths r)
  "Repeats the given pattern infinitly. You might want to comment the push to *mtempos* once the pattern are all done. Also you can play around by adding a probabily to NOT play the pattern. Instead of just fade away.

TIME the (now) time in samples where the sample will begin.
NOTES a list midi notes to play
PATTERN a list with 0's and 1's that indicate when to play the beat  
LENGTHS a list of duration in beats a note should play
R is the midi channel used."
  (let ((pbeat (loop :for beat :in pattern
                  :and nbeat :upto 64
                  :when (= 1 beat)
                  :collect nbeat)))
    ;; Take the list of beats where a note is played
    ;; pbeat = '(0 4 8 32) and schedule it
    (loop :for cbeat :in pbeat
       :do (push cbeat *mtempos*))
    (push (list (next notes 't) (next lengths 't) r pattern) *p*)))

;; I need to use (mod) to get the next beat where there is a note
(defun ppattern (time lpattern notes length1 length2 &key
                                                       (cbeat 0)
                                                       (ibeat 0)
                                                       (chan 2)
                                                       (pchan 0)
                                                       accumbeats
                                                       accumnotes
                                                       accumlengths
                                                       play
                                                       pbeat)
  (if (not (null notes))
      (let ((nbeat   (+ .5 cbeat))
            (nibeat  (+  1 ibeat))
            (nchan   (if (= 9 (+ 1 chan)) (+ 2 chan) (+ 1 chan)))
            (npchan  (mod (+ pchan 1) 2))
            (t-nbeat (+ time .5))
            (note    (first notes)))
        ;; play now
        (if (or (eq play 'yes)
                (= pbeat ibeat))
            (progn
              (setf notes   (cdr notes)
                    pbeat   (mod (+ ibeat (* length1 2))
                                 lpattern))
              (push note    accumnotes)
              (push 1       accumbeats)
              (push length1 accumlengths))
            (push 0 accumbeats))
        ;; reset when the next .5 beat is the last beat of the pattern
        ;; if not is business as usual and we stick with this pattern
        (if (= lpattern nibeat)
            (progn
;;              (print "endpattern")
              (spattern time
                   (new cycle :of (reverse accumnotes) :repeat 1)
                   (reverse accumbeats)
                   (new cycle :of (reverse accumlengths) :repeat 1)
                   chan)
              ;; This works to match agains the prev NOT the global
              (if (and (= 1 (first accumbeats))
                       (= pbeat 0))
                  (progn
                    (print "endswap")
                    (ppattern time lpattern notes length2 length1
                              :pbeat pbeat
                              :chan nchan
                              :pchan npchan))
                  (ppattern time lpattern notes length1 length2
                            :pbeat pbeat
                            :chan nchan
                            :pchan npchan)))
            (if (and (= pbeat (mod nibeat lpattern))
                     (find pbeat *mtempos*))
                (progn
;;                  (print "middle swap")
                  (ppattern time lpattern notes length2 length1
                            :accumnotes accumnotes
                            :accumbeats accumbeats
                            :accumlengths accumlengths
                            :cbeat nbeat
                            :chan chan
                            :ibeat nibeat
                            :pbeat pbeat))
                (progn
                  (ppattern time lpattern notes length1 length2
                            :accumnotes accumnotes
                            :accumbeats accumbeats
                            :accumlengths accumlengths
                            :cbeat nbeat
                            :chan chan
                            :ibeat nibeat
                            :pbeat pbeat)))))))

(defun mbox (time lpattern note length1 length2 bottom up pc
             &optional startchan)
  (setf *mtempos* nil)
  (setf *p* nil)
  (setf *n* nil)
  (let* ((midinote (cm:keynum note))
         (notes    (loop
                      :for x :from bottom :to up
                      :collect (pc-relative midinote x pc)))
         (notes    (rest notes)))
    (setf *n* notes)
    (ppattern time lpattern notes length1 length2 :play 'yes)
    (setf *p* (reverse *p*))))


;; Macro for common play
(defmacro mbox-play (index volume beat-duration note-duration-mul channel)
  (let ((fname (intern (format nil "~A-~A" 'mplay index)))
        (fname-loop (intern (format nil "%~A-~A" 'mplay index))))
    `(let ((p-volume ,volume)
           (p-note-duration-mul ,note-duration-mul)
           (p-beat-duration ,beat-duration)
           (index ,index)
           (p-channel ,channel)
           (p-durations nil))
       (defun ,fname-loop (time
                           notes beats durations raw-durations)
         (let ((beat (next beats)))
           (when (= 1 beat)
             (let ((note     (next notes))
                   (duration (next durations)))
               (p time note p-volume (* duration p-note-duration-mul) p-channel))))
         (aat (+ time p-beat-duration) #',fname-loop
              it
              notes beats durations raw-durations))
       (defun ,fname ()
         (destructuring-bind (notes durations channel beats) (nth index (reverse *p*))
           (declare (ignore channel))
           (setf p-durations durations)
           (,fname-loop (quant (* (length (cadddr (first *p*))) ;; length pattern
                                  p-beat-duration))
                        (make-cycle notes) (make-cycle beats)
                        (make-cycle durations) durations))))))
;; macro for custom play
(defmacro mbox-custom (index
                       ;;volume
                       beat-duration
                       ;;note-duration-mul channel
                       &body body)
  (let ((fname (intern (format nil "~A-~A" 'mplay index)))
        (fname-loop (intern (format nil "%~A-~A" 'mplay index))))
    `(let (;;(p-volume ,volume)
           ;;(p-note-duration-mul ,note-duration-mul)
           (p-beat-duration ,beat-duration)
           (index ,index)
           ;;(p-channel ,channel)
           (p-durations nil))
       (defun ,fname-loop (time
                           notes beats durations raw-durations)
         (let ((beat (next beats)))
           (when (= 1 beat)
             (let ((note     (next notes))
                   (duration (next durations)))
               ,@body)))
         (aat (+ time p-beat-duration) #',fname-loop
              it
              notes beats durations raw-durations))
       (defun ,fname ()
         (destructuring-bind (notes durations channel beats) (nth index (reverse *p*))
           (declare (ignore channel))
           (setf p-durations durations)
           (,fname-loop (quant (* (length (cadddr (first *p*))) ;; length pattern
                                  p-beat-duration))
                        (make-cycle notes)
                        (make-cycle beats)
                        (make-cycle durations)
                        durations))))))


(defun merge-beats (beats &optional merged)
  "SLIME> (merge-beats '((0 0 0 1) (1 0 0 0)))
   (1 0 0 1)"
  (let ((a (car beats))
        (b (if merged
               merged
               (cadr beats))))
    (if (and a b)
        (merge-beats
         (cddr beats)
         (loop
            :for x :in a
            :for y :in b
            :collect
            (if (or (= x 1) (= y 1))
                1
                0)))
        merged)))

(defun get-mbeats (start end)
  (declare (integer start end))
  (merge-beats (mapcar #'cadddr (subseq *p* start end))))

(defun get-mnotes (start end)
  (declare (integer start end))
  (mapcar #'caar (subseq *p* start end)))

