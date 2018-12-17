(in-package :shiny)

;; (ql:quickload :cl-alsaseq)
;; (midihelper:start-midihelper)

;; (midihelper:send-event (midihelper:ev-pgmchange 1 5))
;; (midihelper:send-event (midihelper:ev-pgmchange 1 52))
;; (midihelper:send-event (midihelper:ev-pgmchange 2 101))

;; With Incudine scheduler
;; FIX #[] usage
(defgeneric play-midi (time note velocity duration channel))
(defmethod play-midi (time note velocity duration channel))
(defmethod play-midi ((time double-float)
                      (note fixnum)
                      (velocity fixnum)
                      (duration number)
                      (channel fixnum))
  (at time #'midihelper:send-event (midihelper:ev-noteon channel note velocity))
  (at (+ time #[duration b])
      #'midihelper:send-event
      (midihelper:ev-noteoff channel note velocity)))

(defmethod play-midi ((time double-float)
                      (note list)
                      (velocity fixnum)
                      (duration number)
                      (channel fixnum))
  (mapcar (lambda (x) (play-midi time x 1 duration channel))
          note))
;;--------------------------------------------------
(defgeneric play-midi-arp (time note velocity duration channel offset))
(defmethod play-midi-arp  (time note velocity duration channel offset))
(defmethod play-midi-arp  ((time double-float)
                           (note fixnum)
                           (velocity fixnum)
                           (duration number)
                           (channel fixnum)
                           (offset number))
  (play-midi time note velocity duration channel))

(defmethod play-midi-arp  ((time double-float)
                           (note list)
                           (velocity fixnum)
                           (duration number)
                           (channel fixnum)
                           (offset number))
  (let* ((lnotes  (length note))
         (offsets (loop :for i :from 0 :by offset :collect i :repeat lnotes)))
    (mapcar
     (lambda (n o) (play-midi (+ time (* *sample-rate* (* (sample o) (spb *tempo*))))
                         n velocity duration channel))
            note
            offsets)))

(defmethod play-midi-arp  ((time double-float)
                           (note list)
                           (velocity fixnum)
                           (duration list)
                           (channel fixnum)
                           (offset list))
  (mapcar
   (lambda (n d o) (play-midi (+ time (* *sample-rate* (* (sample o) (spb *tempo*))))
                         n velocity d channel))
   note
   duration
   offset))
