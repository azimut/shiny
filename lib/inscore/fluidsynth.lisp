(in-package :shiny)

;;----------------------------------------
;; FLUIDSYNTH helpers
(defmethod p :before
    ((time double-float) (pitch integer)
     (velocity integer) (duration number)
     (channel integer) &key pan)
  "single note helper"
  (declare (optimize speed))
  (let ((c (gethash *window-name* *bar-counter*)))
    (when (or (not c) (>= c *bar-length*))
      (inscore-stream :meter *meter*)
      (setf (gethash *window-name* *bar-counter*) 0)))
  (let ((keynum)
        (rhythm (inscore-rhythm duration)))
    (if (= pitch 0)
        (setf keynum "_")
        (setf keynum (inscore-reverse-notes pitch)))
    (inscore-write (format nil "~a~a" keynum rhythm))
    (incf (gethash *window-name* *bar-counter*)
          (read-from-string
           (format nil "1~d" rhythm)))))

(defmethod p :before
    ((time double-float) (pitch list)
     (velocity integer) (duration number)
     (channel integer) &key pan)
  "chord helper"
  (let ((c (gethash *window-name* *bar-counter*)))
    (when (or (not c) (>= c *bar-length*))
      (inscore-stream :meter *meter*)
      (setf (gethash *window-name* *bar-counter*) 0)))  
  (let ((rhythm (inscore-rhythm duration))
        (keynums))
    (setf keynums
          (mapcar
           (lambda (pitch)
             (format nil "~a~a"
                     (if (= pitch 0)
                         "_"
                         (inscore-reverse-notes pitch))
                     rhythm))
           pitch))
    (inscore-write        
        (format nil "{~{~a~^,~}}" keynums))
    ;; regardless of being a chord we only provide one length
    (incf (gethash *window-name* *bar-counter*)
          (read-from-string (format nil "1~d" rhythm)))))
