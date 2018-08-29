(in-package :shiny)

;; http://ericsams.org/index.php/on-music/essays/on-schumann/97-did-schumann-use-cyphers-aug-1965
(defparameter *schumann*
  '((d . 0) (i . 0) (q . 0)
    (h . 1) (j . 1) (r . 1)
    (a . 2) (k . 2) (s . 2)
    (b . 3) (l . 3) (t . 3)
    (c . 4) (m . 4) (u . 4)
    (e . 5) (n . 5) (v . 5)
    (f . 6) (o . 6) (w . 6)
    (g . 7) (p . 7) (x . 7)))

;; FIXME!!!
(defun crypt-melody (l)
  "seemless show the melody meaning"
  (declare (list l))
  (values
   (coerce
    (loop :for e :in l :collect
       (coerce (cm:note-name e) 'character))
    'string)
   l))

(defun decrypt-melody (s &optional (dictionary *schumann*))
  "returns offsets from a string for a schumann alist"
  (declare (string s))
  (labels ((f (x y)
             (equalp (string x) (symbol-name y))))
    (let ((slist (coerce s 'list)))
      (loop :for c :in slist :collect
         (cdr (assoc c dictionary
                     :test #'f))))))


