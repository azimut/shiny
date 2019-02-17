(in-package #:shiny)

(defun morse-to-times (morse-string)
  "(let ((pat (make-cycle
            (morse-to-times (cl-morse:string->morse \"race\")))))
  (defun f (time)
    (destructuring-bind (p r) (next pat)
      (when p (p time 60 40 (* .2 p) 5))
      (aat (+ time #[(* .2 r) b]) #'f it))))"
  (declare (type string morse-string))
  (loop :for char :across morse-string :collect
       (eswitch (char :test #'char=)
         (#\. '(1   2))
         (#\- '(3   6))
         (#\  '(NIL 6)))))
