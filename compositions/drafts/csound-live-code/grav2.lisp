(in-package :shiny)

;; https://www.youtube.com/watch?v=0Vq1Ue8SrJY
(let ((tempo 30))
  (setf (bpm *tempo*) (* 4 tempo))
  (update-1beat)
  (csound-set-tempo (round (/ tempo 4))))

(csound-socket-send "stop \"S2\"")
(csound-socket-send "start \"ReverbMixer\"")
(csound-socket-send "start \"Mono\"")
(csound-socket-send "stop  \"Mono\"")
;;
(csound-chnset .2  "Mono.rvb")
(csound-chnset  4  "Mono.Q")
(csound-chnset .5  "Mono.pan")
(csound-chnset 500 "Mono.cut")

(progn
  (let ((note (make-cycle '(60 63 65 63
                            60 63 65 67
                            60 63 65 63
                            60 63 65 70)))
        (dur  (make-cycle (rhythm
                           '(s e s s
                             q e s q)))))
    (defun p1 (time)
      (let ((d (next dur)))
        (clc 29 (next note) 50 d)
        (aat (+ time #[d b]) #'p1 it))))
  ;;
  (let ((note (make-cycle '(53 55 55 58
                            53 55 55 58
                            53 51 53 55
                            51 48 53 55))))
    (defun p2 (time)
      (clc 90 (next note) 50 .6)
      (aat (+ time #[1 b]) #'p2 it)))
  ;; ;;
  (let ((note (make-cycle '(36 36 34 36
                            36 36 34 36
                            34 36 32 34
                            29 31 32 34))))
    (defun p3 (time)
      (clc 22 (next note) (rcosr 10 5 3) .5)
      (aat (+ time #[.5 b]) #'p3 it)))
  ;;
  (let ((notes (make-weighting
                (cm:transpose
                 (alexandria:map-product
                  #'+
                  '(-12 0 12)
                  (scale 0 'minor))
                 60))))
    (defun iharmony (time)
      (let ((n (pick 3 4)))
        (clc 27 (next notes n)
             (rsinr 4 3 3) 3))
      (incandescent::do-shot (rtg-math:v! 0 0 0))
      (aat (+ time #[2 b]) #'iharmony it))))

(defmethod clc :before (i keynum velocity duration)
  (typecase i
    (string (when (string= "Sub1" i)
              (incandescent::place-t1)))
    (fixnum (when (= 22 i)
              (incandescent::place-t1)))))

(let ((time (tempo-sync #[1 b])))
  ;;(aat time #'p1 it)
  ;;(aat time #'p2 it)
  ;;(aat time #'p3 it)
  (aat time #'iharmony it)
  )

(defun iharmony ())
(defun p1 ())
(defun p2 ())
(defun p3 ())

#||
List of strings
each string is 
||#

(defmethod clc :before (i (pitch fixnum) velocity duration)
  "single note helper"
  (declare (ignore i))
  (when (> velocity 0)
    (push (concatenate 'string
                       (inscore-reverse-notes pitch)
                       (inscore-rhythm duration))
          *ins*)))

;; NOTE: still writes directly as i dunno how to put a chord inside a chord
(defmethod clc :before (i (pitch list) velocity duration)
  "chord"
  (declare (ignore i))
  (when-let* ((velocity-p     (> velocity 0))
              (duration-p     (> duration 0))
              (inscore-rhythm (inscore-rhythm duration))
              (fstring        (str:concat "{~{~A" inscore-rhythm "~^,~}}"))
              (inscore-notes  (mapcar #'inscore-reverse-notes pitch)))
    (inscore-write (format nil fstring inscore-notes))))

(defparameter *ins* '())
(let ((changed nil))
  (defun update-inscore (time)
    (when changed (setf changed nil))
    (when (= 31 (mod (round (/ time *1beat*)) 32))
      (setf changed t)
      (inscore-roll-stream))
    ;; new beat
    (when *ins*
      (inscore-write (str:concat "{" (str:join "," *ins*) "}"))
      (setf *ins* '()))
    (aat (+ time #[.1 b]) #'update-inscore it)))

(flush-pending)
(defun update-inscore ())
(aat (tempo-sync #[1 b]) #'update-inscore it)

(clc 26 60 10 1)

(progn (inscore-reset)
       (inscore-init)
       (inscore-roll-stream))

(inscore-reverse-notes 60)
(inscore-rhythm 1)
(inscore-write " c2/4 ]")
(inscore-write " { c2/4, c3/4 } d2/4 ") ;; [
(inscore-write " { [c2/4], [c3/4] } ") ;; {
(inscore-write "  [ d3/4 ] ")
(inscore-write " , [ _/4 ] ")
(inscore-write " , [ c3/4 ] ")
(inscore-write " } ")

