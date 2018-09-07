(in-package :shiny)

(defvar *qnotes* nil)

(defun go-track (to-track mf)
  "skip ahead to-track"
  (declare (integer to-track) (midifile:input-stream mf))
  (let* ((number-of-tracks (midifile:number-of-tracks mf))
         (current-track    (midifile:current-track mf))
         (to-track         (max 0 (min to-track (1- number-of-tracks)))))
    (loop :repeat (- to-track current-track)
       :do (midifile:next-track mf))))

(defun get-notes (filename &optional (track-number 0))
  "just return the midi notes, MOSTLY useful with bars without chords
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
                  (> (midifile:message-data2 mf) 0))
       :collect (midifile:message-data1 mf))))

(defun push-midi-note
    (note time-seconds velocity &optional start-time coerce)
  "helper for (get-notes-duration)"
  (let ((current (gethash note *qnotes*)))
    (when coerce
      (setf time-seconds (coerce time-seconds 'single-float)))
    ;; NOTE: some midi files have two hits without release between them,
    ;;       avoiding those for now
    (if current
        (when (= velocity 0)
          (setf (gethash note *qnotes*) nil)
          (let ((result (list note (- time-seconds (cadr current)))))
            (when start-time
              (alexandria:appendf result (list time-seconds)))
            result))
        (when (not (= 0 velocity))
          (setf (gethash note *qnotes*)
                (list note time-seconds))
          nil))))

(defun get-notes-durations
    (filename &optional (track-number 0) start-time coerce)
  "get notes and duration as pairs, ONLY useful for bars with only one note
   > (get-notes-duration *mf*)
   ((50 .2) (60 .2) (90 1.3))"
  (declare (string filename))
  (setf *qnotes* (make-hash-table))
  (midifile:with-open-midifile (mf filename)
    (go-track track-number mf)
    (remove-if
     #'null
     (loop
        :for ev = (midifile:read-event mf)
        :while ev
        :with previous-time = -1
        :when (and (= ev 144) (= track-number (midifile:current-track mf)))
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
    (filename &optional (track-number 0) start-time coerce)
  "sorts and groups get-notes-duration to get the notes on chords"
  (declare (string filename))
  (remove-if
   #'null
   (loop
      :for (note duration time)
      :in  (sort (get-notes-durations filename track-number t coerce)
                 #'< :key #'caddr)
      :with queue
      :with last-time
      :collect
      (cond ((not queue)
             (prog1 nil
               (setf last-time time)
               (setf queue `(,note ,duration))))
            ((and queue (= last-time time))
             (prog1 nil
               (let ((notes     (append (alexandria:ensure-list (car  queue)) (list note)))
                     (durations (append (alexandria:ensure-list (cadr queue)) (list duration))))
                 (setf queue (list notes durations)))))
            ((and queue (not (= last-time time)))
             (let ((result queue))
               (when start-time
                 (alexandria:appendf result (list last-time)))
               (setf last-time  time)
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
                 (push now queue) nil)
                ((and queue (= time (cadar queue)))
                 (push now queue) nil)
                ;; Time to return the queue
                ((and queue (not (= time (cadar queue))))
                 (prog1 (mapcar #'car queue)
                   (setf queue nil)
                   (push now queue)))))))))

(defun get-notes-durations-chords-silences
    (filename &optional (track-number 0) start-time (coerce t))
  (declare (string filename))
  (remove-if
   #'null
   (loop
      :for (notes durations time)
      :in  (get-notes-durations-chords filename track-number t coerce)
      :with next-time
      :appending
      (let ((current (list notes durations)))
        (when start-time
          (alexandria:appendf current (list time)))
        (cond
          ((not next-time)
           (setf next-time (+ time (first (sort (alexandria:ensure-list durations) #'>))))
           (list current))
          ((and next-time
                (< next-time time)
                ;; NOTE: Hacks! basically we skip silences too small product
                ;;       of the flaky math
                (> (- time next-time) .02))
           (let* ((zero-duration (- time next-time))
                  (zero-start    (- time zero-duration)))
             (setf next-time (+ time (first (sort (alexandria:ensure-list durations) #'>))))
             (if start-time
                 (list current (list 0 zero-duration zero-start))
                 (list current (list 0 zero-duration)))))
          (t
           (setf next-time (+ time (first (sort (alexandria:ensure-list durations) #'>))))
           (list current)))))))
