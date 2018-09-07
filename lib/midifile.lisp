(in-package :shiny)

;; TODO:
;; - It would be nice parse and return chords with time
;; - Identify rests as a new event?
;; - For some reason, there is no reliable global index, event-seconds
;;   Might get reset in the middle of the file. So, I need to make one.

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
       :when (and (= ev 144) ;;(< ev 240) ;; #xf0
                  (> (midifile:message-data2 mf) 0))
       :collect (midifile:message-data1 mf))))

(defun push-midi-note
    (note time-seconds velocity &optional (start-time nil))
  "helper for (get-notes-duration)"
  (let ((current (gethash note *qnotes*))
        (time-seconds (coerce time-seconds 'single-float)))
    (if current
        (progn (assert (= 0 velocity))
               (setf (gethash note *qnotes*) nil)
               (if start-time
                   (list note (- time-seconds (cadr current))
                         time-seconds)
                   (list note (- time-seconds (cadr current)))))
        (progn (assert (not (= 0 velocity)))
               (setf (gethash note *qnotes*)
                     (list note time-seconds))
               nil))))

(defun get-notes-duration
    (filename &optional (track-number 0) (start-time nil))
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
;;        :when (= ev 144)
        :collect
        (list ev  (midifile:event-seconds mf) (midifile:event-beats mf)
              (midifile:event-delta-time mf) (midifile:event-time mf))
        ;; (push-midi-note (midifile:message-data1 mf)
        ;;                          (midifile:event-seconds mf)
        ;;                          (midifile:message-data2 mf)
        ;;                          start-time)
        ))))
(defun get-notes-list-duration
    (filename &optional (track-number 0) (start-time nil))
  "sorts and groups get-notes-duration to get the notes on chords"
  (declare (string filename))
  (remove-if
   #'null
   (loop
      :for (note duration time)
      :in  (sort (get-notes-duration filename track-number t)
                 #'< :key #'caddr)
      :with queue
      :with last-time
      :collect
      (cond ((not queue)
             (prog1 nil
               (setf last-time time)
               (setf queue `((,note) (,duration)))))
            ((and queue (= last-time time))
             (prog1 nil
               (let ((notes     (append (car  queue) (list note)))
                     (durations (append (cadr queue) (list duration))))
                 (setf queue (list notes durations)))))
            ((and queue (not (= last-time time)))
             (prog1 (if start-time
                        (append queue (list last-time))
                        queue)
               (setf last-time time)
               (setf queue `((,note) (,duration)))))))))

(defun get-notes-list (filename &optional (track-number 0))
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
        :when (and (= 144 ev) (> (midifile:message-data2 mf) 0)) ;; (< ev 240) ;; #xf0
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

(defun get-notes-durations-list-silences
    (filename &optional (track-number 0))
  (declare (string filename))
  (remove-if
   #'null
   (loop
      :for (notes durations start-time)
      :in (get-notes-list-duration filename track-number t)
      :with next-time
      :appending
      (let ((current (list notes durations start-time)))
        (cond
          ((not next-time)
           (setf next-time (+ start-time (first (sort durations #'>))))
           (list current))
          ((and next-time (< next-time start-time))
           (let* ((zero-duration (- start-time next-time))
                  (zero-start (- start-time zero-duration)))
             (setf next-time (+ start-time (first (sort durations #'>))))
             (list current (list 0 zero-duration zero-start))))
          ((and next-time (> next-time start-time))
           (list current)))))))
