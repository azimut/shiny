(in-package #:somecepl)

(defun mynow ()
  ;; Just some number that increases over time that we use
  ;; in a bunch of places
  (/ (float (get-internal-real-time))
     1000))
