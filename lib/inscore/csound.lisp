(in-package :shiny)

;; NOTE: one of the limitations of the design of using &rest
;;       for arbitrary params is (i think) that i cannot ask for
;;       additional &key params i think...
;; Well, even if i could i won't see them on the created macro above
;; NOTE: Might be I need to add time after all...it would be nice for
;;       arpgeggios

(defmethod playcsound-key :before
    ((iname string) (duration number)
     (keynum integer) &rest rest)
  ;; Reset the stream if either, there is no stream or
  ;; is over the max lenght
  (let ((c (gethash *window-name* *bar-counter*)))
    (when (or (not c) (>= c 4))
      (inscore-stream :meter *meter*)
      (setf (gethash *window-name* *bar-counter*) 0)))
  (let ((pitch)
        (rhythm (inscore-rhythm duration)))
    ;; PITCH & RHYTHM
    (if (= keynum 0)
        (setf pitch "_")
        (setf pitch (inscore-reverse-notes keynum)))
    ;; COUNTER
    (incf (gethash *window-name* *bar-counter*)
          (read-from-string (format nil "1~a" rhythm)))
    ;; WRITE: rhythm can be NIL
    (inscore-write (format nil "~a~a" pitch rhythm))))

(defmethod playcsound-key :before
    ((iname string) (duration number)
     (keynum list) &rest rest)
  ;; Reset the stream if either, there is no stream or
  ;; is over the max lenght
  (let ((c (gethash *window-name* *bar-counter*)))
    (when (or (not c) (>= c 4))
      (inscore-stream :meter *meter*) 
     (setf (gethash *window-name* *bar-counter*) 0)))
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
    ;; WRITE
    (inscore-write (format nil "{~{~a~^,~}}" keynums))))
