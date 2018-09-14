(in-package :shiny)

;; NOTE: all this is kind of flaky. As it works mostly with one voice
;; per score. And that won't change until all notes go to a common place
;; where they then can be converted into chords (events from different
;; sources happening at the same time).

;; https://resolume.com/download/Manual/OSC/OSC%20list.txt
(defvar *oscout* nil)
(defvar *bar-length* 0)

(defun make-inscore ()
  (setf *oscout*
        (osc:open :host "127.0.0.1"
                  :port 7000
                  :direction :output)))

(defun inscore-init ()
  (osc:message *oscout* "/ITL/scene/l" "ss" "set" "layer")
  (osc:message *oscout* "/ITL/scene/l/background" "ssff" "set" "rect" 10f0 10f0)
  (osc:message *oscout* "/ITL/scene/l/background" "siii" "color" 0 0 0))


(defun inscore-score
    (score &key (meter "4/4") (clef "g") (key 0))
  "static score"
  (osc:message
   *oscout* "/ITL/scene/score" "sss" "set" "gmn"
   (format nil "[ \\meter<\"~a\"> \\clef<\"~a\"> \\key<~a> ~a ]"
           meter clef key score)))

(defun inscore-reset
    (&optional (root "scene"))
  (let ((s (format nil "/ITL/~a/*" root)))
    (osc:message *oscout* s "s" "del")))

;; GMNSTREAM
;; write: add the gmn code to the current gmn stream
};; clear: reinitialize the stream

;; EXAMPLE
;; Writing a score in 3 steps:

;; /ITL/scene/myScore set gmnstream "[ c";
;; /ITL/scene/myScore write " d e";
;; /ITL/scene/myScore write " f]";

(defun inscore-stream
    (&key (meter "4/4" meter-p) (clef "g" clef-p) (key 0 key-p))
  "stream score"
  (let ((final-score "[ "))
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
     *oscout* "/ITL/scene/l/score" "sss" "set" "gmnstream"
     final-score)
    ;;COLOR WHITE
    (osc:message *oscout*
                 "/ITL/scene/l/score"
                 "siii"
                 "color" 240 240 240)))

(defun inscore-write (score)
  "stream score write"
  (osc:message
   *oscout* "/ITL/scene/l/score" "ss" "write"
   (format nil " ~a " score)))

(osc:message
 *oscout* "/ITL/scene/grid" "sf"
 "xborder" .1)

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

;; (inscore-reset)
;; (inscore-init)
;; (inscore-stream :meter "4/4" :clef "g")
;; (inscore-write (pick "_" "a" "b" "c" "d" "f" "g" "h"))
;; (inscore-write "_/32")
;; (inscore-write "a*1/4")
;; (inscore-write "a/2")


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

;;----------------------------------------
;; FLUIDSYNTH helpers
(defmethod p :before
    ((time double-float) (pitch integer)
     (velocity integer) (duration number)
     (channel integer) &key pan)
  "single note helper"
    (let ((keynum)
          (rhythm))
      (if (= pitch 0)
          (setf keynum "_")
          (progn
            (setf keynum (inscore-reverse-notes pitch))
            (setf rhythm (inscore-rhythm duration))))
      (when (>= *bar-length* 4)
        (inscore-stream :meter "4/4" :clef "g")
        (setf *bar-length* 0))
      (at time #'inscore-write
          (format nil "~a~a" keynum rhythm))
      (incf *bar-length* (read-from-string
                          (format nil "1~d" rhythm)))))

(defmethod p :before
    ((time double-float) (pitch list)
     (velocity integer) (duration number)
     (channel integer) &key pan)
  "chord helper"
  ;; reset state
  (when (>= *bar-length* 4)
    (inscore-stream :meter "4/4" :clef "g")
    (setf *bar-length* 0))
  (let ((rhythm (inscore-rhythm duration))
        (keynums))
    ;; regardless of being a chord we only provide one length
    (incf *bar-length* (read-from-string (format nil "1~d" rhythm)))
    (setf keynums
          (mapcar
           (lambda (pitch)
             (format nil "~a~a"
                     (if (= pitch 0)
                         "_"
                         (inscore-reverse-notes pitch))
                     rhythm))
           pitch))
    (at time #'inscore-write        
        (format nil "{~{~a~^,~}}" keynums))))
