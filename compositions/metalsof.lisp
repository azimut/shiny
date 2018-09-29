(in-package :shiny)

;; Trying to understand the code in:
;; https://github.com/namin/metasolfeggio/tree/master/overtone

(defvar *harmony-degrees*
  (mapcar (lambda (x) (mapcar #'1- x))
          '((3 6 2 4 5)
            (5 7)
            (6)
            (5 7)
            (1)
            (2 4)
            (1))))

(defun degree-chord (d)
  (mapcar (lambda (x) (mod (+ d x) 7))
          (let ((ds '(0 2 4)))
            (if (member (1+ d) '(5 7))
                (append ds '(6))
                ds))))

(defun degree-contains? (a b)
  (when (find b (degree-chord a))
    t))

(defun harmony-tab (ds)
  (mapcar (lambda (d) (filter (lambda (x) (degree-contains? x d)) ds))
          (loop for i upto 87 collect i)))

(defparameter *harmony* (mapcar #'harmony-tab *harmony-degrees*))
