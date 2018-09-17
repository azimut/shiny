(in-package :shiny)

;; NOTE: all this is kind of flaky. As it works mostly with one voice
;; per score. And that won't change until all notes go to a common place
;; where they then can be converted into chords (events from different
;; sources happening at the same time).
;; And might be not even that will help for livecoding. As there is no time
;; index or anything being send on a stream so, everything happens serially
;; a voice at the time, without a possible whole note followed by a quarted
;; before the whole finish...

;; TODO:
;; - use more (par)

;; Reference:
;; http://www.grame.fr/ressources/publications/inscore-tenor-2016.pdf

(defvar *oscout* nil)
(defvar *window-name* "scene")
(defvar *layer-name* "l")
(defvar *bar-counter* (make-hash-table :test #'equal))
(defvar *meter* "4/4")
(defvar *clef* "f")

(defun inscore-reset
    (&optional (window-name *window-name*) delete)
  (let* ((root   (concatenate 'string "/ITL/" window-name))
         (childs (concatenate 'string root "/*")))
    (setf (gethash window-name *bar-counter*) 30)
    (osc:message *oscout* childs "s" "del")
    (when delete
      (osc:message *oscout* root "s" "del"))))

(defun inscore-init
    (&optional (window-name *window-name*) (layer-name *layer-name*))
  (declare (string layer-name))
  (let* ((root (concatenate 'string "/ITL/" window-name))
         (layer (concatenate 'string root "/" layer-name))
         (background (concatenate 'string layer "/background")))
    (osc:message *oscout* root "s" "new")
    (osc:message *oscout* layer "ss" "set" "layer")
    (osc:message *oscout* background "ssff" "set" "rect" 10f0 10f0)
    (osc:message *oscout* background "siii" "color" 0 0 0)))

(defun make-inscore ()
  (setf *oscout*
        (osc:open :host "127.0.0.1"
                  :port 7000
                  :direction :output))
  (inscore-reset)
  (inscore-init))

;; GMNSTREAM
;; write: add the gmn code to the current gmn stream
;; clear: reinitialize the stream

;; EXAMPLE
;; Writing a score in 3 steps:

;; /ITL/scene/myScore set gmnstream "[ c";
;; /ITL/scene/myScore write " d e";
;; /ITL/scene/myScore write " f]";

(defun inscore-score-debug
    (score &key (window-name *window-name*) (layer-name *layer-name*))
  (let ((score-path
         (concatenate 'string "/ITL/" window-name "/" layer-name "/score")))
    (osc:message
     *oscout* score-path "sss" "set" "gmn"
     score)
    ;;COLOR WHITE
    (osc:message *oscout* score-path "siii" "color" 240 240 240)))

(defun inscore-score
    (score &key
             (meter *meter*) (clef *clef*) (key 0)
             (window-name *window-name*) (layer-name *layer-name*)             )
  "static score"
  (let ((score-path
         (concatenate 'string "/ITL/" window-name "/" layer-name "/score")))
    (osc:message
     *oscout* score-path "sss" "set" "gmn"
     (format nil "[ \\meter<\"~a\"> \\clef<\"~a\"> \\key<~a> ~a ]"
             meter clef key score))
    ;;COLOR WHITE
    (osc:message *oscout* score-path "siii" "color" 240 240 240)))

(defun inscore-stream
    (&key
       (meter *meter* meter-p) (clef *clef* clef-p) (key 0 key-p)
       (window-name *window-name*) (layer-name *layer-name*))
  "stream score"
  (let* ((final-score "[ ")
         (score-path
          (concatenate 'string "/ITL/" window-name "/" layer-name "/score")))
    (inscore-init window-name layer-name)
    (when clef-p
      (setf final-score (concatenate 'string final-score
                                     (format nil "\\clef<\"~a\"> " clef))))
    (when meter-p
      (setf final-score (concatenate 'string  final-score
                                     (format nil "\\meter<\"~a\"> " meter))))
    (when key-p
      (setf final-score (concatenate 'string final-score
                                     (format nil "\\key<~d> " key))))
    (osc:message
     *oscout* score-path "sss" "set" "gmnstream"
     final-score)
    ;; (osc:message *oscout* score-path "si" "tempo" 60)
    ;; (osc:message *oscout* score-path "si" "date" 60)
    ;;COLOR WHITE
    (osc:message *oscout* score-path "siii" "color" 240 240 240)))

(defun inscore-write
    (score &optional (window-name *window-name*) (layer-name *layer-name*))
  "stream score writer"
  (let ((score-path
         (concatenate 'string "/ITL/" window-name "/" layer-name "/score")))
    (osc:message
     *oscout* score-path "ss" "write"
     (format nil " ~a " score))))

;; (inscore-reset)
;; (inscore-init)
;; (inscore-stream :meter "4/4" :clef "g")
;; (inscore-write (pick "_" "a" "b" "c" "d" "f" "g" "h"))
;; (inscore-write "_/32")
;; (inscore-write "a*1/4")
;; (inscore-write "a/2")

;;--------------------------------------------------

(defvar *inscore-reverse-notes*
  (alexandria:alist-hash-table
   '((0  . "c")   (1 . "c#")
     (2  . "d")   (3 . "e&")
     (4  . "e")   (5 . "f")
     (6  . "f#")  (7 . "f#")
     (8  . "a&")  (9 . "a")
     (10 . "b&") (11 . "b"))))

(defun inscore-reverse-notes (n)
  (format nil "~a~d"
          (gethash (mod n 12) *inscore-reverse-notes*)
          (+ -3 (cm:octave-number n))))

;; a*1/4
;; Denominators: 1*1.5 1 2 4  8  16 32 64
;; Seconds:      6     4 2 1 .5 .25
;; 2^:                 0 1 2  3   4  5  6
;; HACKS!!... I need some math here instead...oh well
(defun inscore-rhythm (n)
  (cond ((>= n 8) (format nil "*~d/1" (round (/ n 4))))
        ((<= 4 n 8) "/1")
        ((<= 2 n 4) "/2")
        ((<= 1 n 2) "/4")
        ((<= .5  n  1) "/8")
        ((<= .25 n .4) "/16")
        ((<= .125 n) "/32")
        ((<= .0625 n) "/64")
        (t "/1")))

