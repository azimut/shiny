(in-package :shiny)

;; June
;; Verse: Dm, Em, C, Am
;; Chorus: F, Dm, Am, F, Am, F

(defparameter *chords*
  (mapcar (lambda (x) (apply #'chord x))
          '((:D4 :minor) (:E4 :minor) (:C4 :major) (:A4 :minor))))

(defparameter *chords*
  (mapcar (lambda (x) (apply #'chord x))
          '((:F4 :major) (:D4 :minor) (:A4 :minor)
            (:F4 :major) (:A4 :minor) (:F4 :major))))

(defparameter *chords*
  '((55 65 71) (57 65 71) (57 63 71) (53 63 71) (53 63 69) (53 59 69) (53 59 67)
    (61 59 67) (61 57 67) (61 57 63) (59 57 63) (59 53 63) (57 53 63) (57 55 63)
    (61 55 63) (61 55 65) (61 59 65) (61 59 67) (65 59 67) (65 59 69)))

(defparameter *chords*
  (cm::transp (cm::tzrandchain '(0 3 4) 20) 50))

(let ((chord (make-cycle *chords*)))
  (defun f (time)
    (destructuring-bind (x y z)
        (cm:transpose (next chord) -24)
      (play-organ x .3 :pan .9)
      (at (+ time #[.333 b]) #'play-organ y .3 :pan .7)
      (at (+ time #[.666 b]) #'play-organ z .3))
    (aat (+ time #[1 b]) #'f it)))
(defun f ())
(f (now))
