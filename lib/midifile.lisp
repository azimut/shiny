(in-package :shiny)
;; TODO: go-track loop end check for outer range.
;;       function that returns pairs (50 .5) per measure
;;
;; NOTE
;; ----
;; In order to use the (group-measures-pair) function you need to know
;; the midi song bar length. Ex: 4/4 or 6/8.  Then provide the
;; MEASURE-LENGTH, here is measure so .25 is 1 black note.  a
;; MEASURE-LENGTH of 2 is then a 4/4 due (= 2 (* .25 4))
;;
;; General Midi Info
;; -----------------
;; Midi Tracks:
;; (Sometimes)One per line you see on a sheet.
;; Starting from 0, from the top OR bottom line.
;;
;; Midi Events:
;; List of midi events in hexadecimal
;; http://www.onicos.com/staff/iz/formats/midi-event.html
;;
;; 144 - 0x90 - Chan 1 Note on
;; 176 - 0xB0 -	Chan 1 Control mode change
;; 192 - 0xC0 - Chan 1 Program change
;; 255 - 0xFF -	Sys real time sys reset
;;
;; Midi Notes:
;; There are no explicit silence/rests on the midifile format
;;
;; Example
;; -------
;; (defparameter *notes* nil)
;; (setf *notes* (subseq (get-measures-pair *mf* 10 2 1) 6))
;; (let ((measures (make-heap *notes*)))
;;   (defun f (time)
;;     (let* ((measure (next measures))
;;            (notes (first measure))
;;            (durations (second measure)))
;;       (play-midi-arp time notes 50 durations 0 (d2o durations)))
;;     (aat (+ time #[2 b]) #'f it)))

(defvar *qnotes* (make-hash-table))

(defun go-track (to-track mf)
  "Skip ahead TO-TRACK"
  (declare (unsigned-byte to-track) (midifile:input-stream mf))
  (let* ((number-of-tracks (1- (midifile:number-of-tracks mf)))
         (current-track    (midifile:current-track mf))
         (to-track         (max (min to-track number-of-tracks) 0)))
    (loop :repeat (- to-track current-track)
       :do (midifile:next-track mf))))

(defun get-notes (filename &optional (track-number 0))
  "Just return the midi notes, useful with bars without chords.
   > (get-notes *mf*)
   (60 64 69 34 59 30 49 59 29)"
  (declare (string filename))
  (midifile:with-open-midifile (mf filename)
    (go-track track-number mf)
    (loop
       :for ev = (midifile:read-event mf)
       :while ev
       :when (and (= ev 144)
                  (= track-number (midifile:current-track mf))
                  (plusp (midifile:message-data2 mf)))
       :collect (midifile:message-data1 mf))))

(defun push-midi-note
    (note time-seconds velocity &optional start-time coerce)
  "helper for (get-notes-duration)"
  (when coerce
    (setf time-seconds (coerce time-seconds 'single-float)))
  (let ((current (gethash note *qnotes*)))
    ;; NOTE: some midi files have two hits without release between them,
    ;;       avoiding those for now
    (if current
        (when (zerop velocity)
          ;; Remove cache for note
          (setf (gethash note *qnotes*) NIL)
          (let ((result (list note (- time-seconds (cadr current)))))
            (when start-time
              (alexandria:appendf result (list (cadr current))))
            result))
        (when (plusp velocity)
          (setf (gethash note *qnotes*)
                (list note time-seconds))
          NIL))))

(defun get-notes-durations
    (filename &optional (track-number 0) start-time (coerce t))
  "get notes and duration as pairs, useful for bars with only one note
   > (get-notes-duration *mf*)
   ((50 .2) (60 .2) (90 1.3))"
  (declare (string filename))
  (clrhash *qnotes*)
  (midifile:with-open-midifile (mf filename)
    (go-track track-number mf)
    (remove-if
     #'null
     (loop
        :for ev = (midifile:read-event mf)
        :while ev
        :with previous-time = 0f0
        :when (and (= ev 144)
                   (= track-number (midifile:current-track mf)))
        :collect
          (let ((seconds (midifile:event-seconds mf)))
            (when (not (= previous-time seconds))
              (setf previous-time seconds))
            (push-midi-note (midifile:message-data1 mf)
                            seconds
                            (midifile:message-data2 mf)
                            start-time
                            coerce))))))

(defun get-notes-durations-chords
    (filename &optional (track-number 0) start-time (coerce t))
  "sorts and groups get-notes-duration to get the notes on chords"
  (declare (string filename))
  (remove-if
   #'null
   (loop
      :for (note duration time)
      :in  (sort (get-notes-durations filename track-number T coerce)
                 #'< :key #'caddr)
      :with queue
      :with last-time
      :collect
      (cond ((not queue)
             (prog1 NIL
               (setf last-time time)
               (setf queue (list note duration))))
            ((and queue (= last-time time))
             (prog1 NIL
               (let ((notes     (append (ensure-list (car  queue))
                                        (list note)))
                     (durations (append (ensure-list (cadr queue))
                                        (list duration))))
                 (setf queue (list notes durations)))))
            ((and queue (not (= last-time time)))
             (let ((result queue))
               (when start-time
                 (alexandria:appendf result (list last-time)))
               (setf last-time time)
               (setf queue `(,note ,duration))
               result))))))

(defun get-notes-chords (filename &optional (track-number 0))
  "get notes grouped by time they were triggered
   > (get-notes-list *mf*)
   ((60 62 65) (60 62 69) (79)"
  (declare (string filename))
  (midifile:with-open-midifile (mf filename)
    (go-track track-number mf)
    (remove-if
     #'null
     (loop
        :for ev = (midifile:read-event mf)
        :while ev
        :with queue = '()
        :when (and (= 144 ev)
                   (= track-number (midifile:current-track mf))
                   (> (midifile:message-data2 mf) 0))
        :collect
          (let* ((time (midifile:event-seconds mf))
                 (now  (list (midifile:message-data1 mf)
                             time)))
            (cond ((not queue)
                   (push now queue) NIL)
                  ((and queue (= time (cadar queue)))
                   (push now queue) NIL)
                  ;; Time to return the queue
                  ((and queue (not (= time (cadar queue))))
                   (prog1 (mapcar #'car queue)
                     (setf queue NIL)
                     (push now queue)))))))))

(defun get-notes-durations-chords-silences
    (filename &optional (track-number 0) start-time (coerce t))
  "Returns pais of notes/durations on the current TRACK-NUMBER.
   Including any silence period. As a pair with 0 as the midi-note
   and the duration of the silence.
   > (get-notes-durations-chords-silences *mf*)
   ((40 0.49895832) (42 0.49895835) (43 0.49895835) (47 0.49895835)
    (45 0.49895835) (43 0.49895835) (38 0.99895835) (40 0.4989581))"
  (declare (string filename))
  (loop
     :for (notes durations time)
     :in  (get-notes-durations-chords filename track-number T coerce)
     :with next-time
     :appending
       (let ((current
              (if start-time
                  (list notes durations time)
                  (list notes durations))))
         (cond
           ;; First run
           ((not next-time)
            (setf next-time
                  (+ time (extremum (ensure-list durations) #'>)))
            (list current))
           ;; Silence in between notes
           ((and next-time
                 (< next-time time)
                 ;; NOTE: Hacks! basically we skip silences too small product
                 ;;       of the flaky math
                 (> (- time next-time) .02))
            (let* ((zero-duration (- time next-time))
                   (zero-start    (- time zero-duration)))
              (setf next-time
                    (+ time (extremum (ensure-list durations) #'>)))
              (if start-time
                  (list (list 0 zero-duration zero-start) current)
                  (list (list 0 zero-duration) current))))
           ;; No silence in between (?
           (t
            (setf next-time
                  (+ time (extremum (ensure-list durations) #'>)))
            (list current))))))

;;--------------------------------------------------

(defun group-by-measure (mf &optional (measure-length 2) (track-number 0))
  "returns 2 values as lists. First list of values are the notes per measure.
   Second list of values is for the durations.
   > (group-by-measure *mf* 2 1)
   ((40 42 43 47) (45 43) (40 42))
   ((0.49895832 0.49895835 0.49895835 0.49895835)
    ((0.9989586 0.9989586) (0.9989586 0.9989586)))"
  (loop
     :for (notes durations)
     :in (get-notes-durations-chords-silences mf track-number)
     :with acc-notes
     :with acc-durations
     :with ret-notes
     :with ret-durations
     :summing (if (listp durations)
                  (first durations)
                  durations) :into total-duration
     :do
       (cond ((> total-duration measure-length)
              (setf total-duration (if (listp durations)
                                       (first durations)
                                       durations))
              ;;
              (push (reverse acc-notes) ret-notes)
              (push (reverse acc-durations) ret-durations)              
              (setf acc-notes     NIL)
              (setf acc-durations NIL)
              ;;
              (push notes     acc-notes)
              (push durations acc-durations))
             (t
              (push notes     acc-notes)
              (push durations acc-durations)))
     :finally (return (values (reverse ret-notes)
                              (reverse ret-durations)))))

(defun get-measures-pair
    (mf &optional (n-measures 4) (measure-length 2) (track-number 0))
  "Returns a list of pairs of notes and durations of the midi file MF
   and TRACK-NUMBER. Up to N-MEASURES measure by MEASURE-LENGTH.
   Re-arrange output of group-by-measure to make it easier to
   shuffle measures around.
   > (get-measures-pair *mf* 2 2 1)
   (((40 42 43 47) (0.49895832 0.49895835 0.49895835 0.49895835))
    ((45 43 38)    (0.49895835 0.49895835 0.99895835)))"
  (multiple-value-bind (notes durations)
      (group-by-measure mf measure-length track-number)
    (loop
       :for measure-notes :in notes
       :for measure-durations :in durations
       :repeat n-measures
       :collect (list measure-notes
                      measure-durations))))
