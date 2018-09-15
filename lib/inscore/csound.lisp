(in-package :shiny)

(defmethod playcsound-key :before
    ((iname string) (duration number)
     (keynum integer) &rest rest)
  (let ((pitch)
        (rhythm))
    (if (= keynum 0)
        (setf pitch "_")
        (progn
          (setf pitch  (inscore-reverse-notes keynum))
          (setf rhythm (inscore-rhythm duration))))
    (when (>= (gethash *window-name* *bar-counter*) 4)
      (inscore-stream :meter "4/4" :clef *clef*)
      (setf (gethash *window-name* *bar-counter*) 0))
    (inscore-write (format nil "~a~a" pitch rhythm))
    (incf (gethash *window-name* *bar-counter*)
          (read-from-string (format nil "1~d" rhythm)))))

(defmethod playcsound-key :before
    ((iname string) (duration number)
     (keynum list) &rest rest)
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
           keynum))
    (inscore-write (format nil "{~{~a~^,~}}" keynums))))
