(in-package #:shiny)

;; General incudine helpers go here

;; (format) took from fmt.pdf
(defun vug-values (sym &optional (brief T))
  "1 COLUMN - pretty prints the parameters of a vug named SYM"
  (multiple-value-bind (names values) (incudine.vug:vug-lambda-list sym)
    (loop
       :for index :from 0
       :for name  :in names
       :for value :in values :do
         (if brief
             (when (and (not (search "UNUSE" (string (first name))))
                        (not (search "PLUGIN" (string (first name)))))
               (format T
                       "~& ~2D ~20@A ~8D"
                       ;;"~a ~a ~a~%"
                       index (first name) value))
             (format T
                     "~& ~2D ~20@A ~8D"
                     ;;"~a ~a ~a~%"
                     index (first name) value))))
  (values))

;; TODO: remove UNUSED
(defun vug-values-2 (sym)
  "2 COLUMN - pretty prints the parameters of a vug named SYM"
  (multiple-value-bind (names values) (incudine.vug:vug-lambda-list sym)
    (format T "~:{~& ~3D ~20@A ~2D   -   ~3D ~20@A ~2D~}"
            (loop
               :for index :from 0 :by 2
               :for (name1 name2) :on names :by #'cddr
               :for (value1 value2) :on values :by #'cddr :collect
                 (list index (first name1) value1
                       (1+ index) (first name2)  value2))))
  (values))

;; BEATS
(declaim (inline calc-beats))
(defun calc-beats (offset)
  (declare ;;(type alexandria:non-negative-real offset)
   (optimize (speed 3)))
  (the double-float (* *SAMPLE-RATE* (* (sample offset) (spb *TEMPO*)))))


;; SECONDS
;; (declaim (inline calc-beats))
;; (defun calc-beats (offset)
;;   (declare ;;(type alexandria:non-negative-real offset)
;;    (optimize (speed 3)))
;;   (the double-float (* *SAMPLE-RATE* offset)))

(defun beat ()
  "returns the current beat, double precision"
  (declare (type double-float *sample-rate*))
  (/ (now) (* *sample-rate* (spb *tempo*))))
(defun rbeat ()
  "returns the current beat, floor to integer"
  (floor (beat)))
