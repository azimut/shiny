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
  "just return the midi notes"
  (declare (string filename))
  (midifile:with-open-midifile (mf filename)
    (go-track track-number mf)
    (loop
       :for ev = (midifile:read-event mf)
       :while ev
       :when (and (< ev 240) ;; #xf0
                  (> (midifile:message-data2 mf) 0))
       :collect (midifile:message-data1 mf))))

(defun push-midi-note (note time-seconds)
  "helper for (get-notes-and-velocity)"
  (let ((current (gethash note *qnotes*)))
    (if current
        (progn (setf (gethash note *qnotes*) nil)
               (list note (- time-seconds (cadr current))))
        (progn (setf (gethash note *qnotes*)
                     (list note time-seconds))
               nil))))

(defun get-notes-and-velocity (filename &optional (track-number 0))
  "get notes and velicity as pairs, ONLY useful for bars with only one note"
  (declare (string filename))
  (setf *qnotes* (make-hash-table))
  (midifile:with-open-midifile (mf filename)
    (go-track track-number mf)
    (remove-if
     #'null
     (loop
        :for ev = (midifile:read-event mf)
        :while ev
        :when (< ev 240) ;; #xf0
        :collect (push-midi-note (midifile:message-data1 mf)
                                 (midifile:event-seconds mf))))))
