(in-package :shiny)

;; TODO:
;; - It would be nice parse and return chords with time
;; - Identify rests as a new event?

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

(defun push-midi-note (note time-seconds)
  "helper for (get-notes-duration)"
  (let ((current (gethash note *qnotes*)))
    (if current
        (progn (setf (gethash note *qnotes*) nil)
               (list note (coerce (- time-seconds (cadr current))
                                  'single-float)))
        (progn (setf (gethash note *qnotes*)
                     (list note time-seconds))
               nil))))

(defun get-notes-duration (filename &optional (track-number 0))
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
        :when (= ev 144) ;;(< ev 240) ;; #xf0
        :collect (push-midi-note (midifile:message-data1 mf)
                                 (midifile:event-seconds mf))))))

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
                   (push now queue))))))))))

