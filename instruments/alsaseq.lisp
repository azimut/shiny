(in-package :shiny)

(midihelper:start-midihelper)
;; connect on qjackctl

(let ((d (new cycle :of '(2 2 4))))
  (defun f (time)
    (let ((c (make-chord 50 70 (random-elt #(3 4)) *phrygian*))
          (dd (next d)))
      (play-midi (car c) 50 dd 2)
      (mapcar 
       (lambda (x) (play-midi x 60 (+ -.1 dd) 1))
       c)
;;      (callback (+ time dd) #'f (+ time dd))
      )))

(f (quant 4))

(midihelper:send-event (midihelper:ev-pgmchange 1 5))
(midihelper:send-event (midihelper:ev-pgmchange 1 52))

(midihelper:send-event (midihelper:ev-pgmchange 2 101))

;; stolen from cl-patterns
(defun play-midi (note velocity duration channel)
  (let* ((time (local-time:timestamp+ (local-time:now)
                                      1000000000
                                      :nsec)))
    (bt:make-thread
     (lambda ()
       ;; (sleep (local-time:timestamp-difference time (local-time:now)))
       (midihelper:send-event (midihelper:ev-noteon channel note velocity))
       (sleep duration)
       (midihelper:send-event (midihelper:ev-noteoff channel note velocity)))
     :name "cl-patterns temporary midi note thread")))

