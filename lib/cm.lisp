(in-package :shiny)

;;--------------------------------------------------
;; CM helpers
;;--------------------------------------------------
(defun make-cycle (elements &optional (for-elements 1 for-elements-p))
  (when elements ;; do not make a cycle of just nil
    (if for-elements-p
        (new cycle :of elements :for for-elements)
        (new cycle :of elements))))

(defun make-heap (elements &optional (for-elements 1 for-elements-p))
  (if for-elements-p
      (new heap :of elements :for for-elements)
      (new heap :of elements)))

(defun make-line (elements)
  (new cm:line :of elements))

(defun make-weighting
    (elements &optional (for-elements 1 for-elements-p))
  ;; try to add :weight keyword if only provided 2 elements
  ;; we push since order doesn't really matter...
  (let ((sane-elements '()))
    (loop for e in elements do
         (if (and (listp e) (= 3 (length e)))
             (push e sane-elements))
         (if (not (listp e))
             (push e sane-elements))
         (if (and (listp e) (= 2 (length e)))
             (push (list (first e)
                         :weight
                         (first (last e)))
                   sane-elements)))
    (if for-elements-p
        (new weighting :of sane-elements :for for-elements)
        (new weighting :of sane-elements))))

(defun make-palindrome (elements)
  (cm:new cm:palindrome :of elements :elide t))

;; took from jazz.cm
(defun rancyc (data prob)
  (list (make-cycle data) :weight prob))
