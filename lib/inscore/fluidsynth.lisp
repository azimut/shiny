(in-package :shiny)

;;----------------------------------------
;; FLUIDSYNTH helpers
(defmethod p :before
    ((time double-float) (pitch integer)
     (velocity integer) (duration number)
     (channel integer) &key pan)
  "single note helper"
  (unless (gethash *window-name* *bar-counter*)
    (setf (gethash *window-name* *bar-counter*) 100))
  (let ((keynum)
        (rhythm))
    (if (= pitch 0)
        (setf keynum "_")
        (progn
          (setf keynum (inscore-reverse-notes pitch))
          (setf rhythm (inscore-rhythm duration))))
    (when (>= (gethash *window-name* *bar-counter*) 4)
      (inscore-stream :meter "4/4" :clef *clef*)
      (setf (gethash *window-name* *bar-counter*) 0))
    (inscore-write (format nil "~a~a" keynum rhythm))
    (incf (gethash *window-name* *bar-counter*)
          (read-from-string
           (format nil "1~d" rhythm)))))

(defmethod p :before
    ((time double-float) (pitch list)
     (velocity integer) (duration number)
     (channel integer) &key pan)
  "chord helper"
  ;; reset state
  (when (>= (gethash *window-name* *bar-counter*) 4)
    (inscore-stream)
    (setf (gethash *window-name* *bar-counter*) 0))
  (let ((rhythm (inscore-rhythm duration))
        (keynums))
    ;; regardless of being a chord we only provide one length
    (incf (gethash *window-name* *bar-counter*)
          (read-from-string (format nil "1~d" rhythm)))
    (setf keynums
          (mapcar
           (lambda (pitch)
             (format nil "~a~a"
                     (if (= pitch 0)
                         "_"
                         (inscore-reverse-notes pitch))
                     rhythm))
           pitch))
    (at time #'inscore-write        
        (format nil "{~{~a~^,~}}" keynums))))
