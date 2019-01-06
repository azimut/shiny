(in-package :shiny)

;; (ql:quickload :cl-alsaseq)
;; (midihelper:start-midihelper)

;; (midihelper:send-event (midihelper:ev-pgmchange 1 5))
;; (midihelper:send-event (midihelper:ev-pgmchange 1 52))
;; (midihelper:send-event (midihelper:ev-pgmchange 2 101))
;; (midihelper:send-event (midihelper:ev-cc 0 10 100))

(defgeneric play-midi (time note velocity duration channel))
(defmethod play-midi (time note velocity duration channel))
(defmethod play-midi ((time double-float)
                      (note fixnum)
                      (velocity fixnum)
                      (duration number)
                      (channel fixnum))
  "play note"
  (when (> note 0)
    (at time #'midihelper:send-event (midihelper:ev-noteon channel note velocity))
    (at (+ time (* *sample-rate* (* (sample duration) (spb *tempo*))))
        #'midihelper:send-event
        (midihelper:ev-noteoff channel note velocity))))

(defmethod play-midi ((time double-float)
                      (note list)
                      (velocity fixnum)
                      (duration number)
                      (channel fixnum))
  "play chord"
  (mapcar (lambda (n) (play-midi time n velocity duration channel))
          note))

(defmethod play-midi ((time double-float)
                      (note list)
                      (velocity fixnum)
                      (duration list)
                      (channel fixnum))
  "chord, with different durations"
  (mapcar (lambda (n d) (play-midi time n velocity d channel))
          note
          duration))
;;--------------------------------------------------
(defgeneric play-midi-arp (time note velocity duration channel offset))
(defmethod play-midi-arp  (time note velocity duration channel offset))
(defmethod play-midi-arp  ((time double-float)
                           (note fixnum)
                           (velocity fixnum)
                           (duration number)
                           (channel fixnum)
                           (offset number))
  "fallback, takes a single note"
  (play-midi time note velocity duration channel))

(defmethod play-midi-arp  ((time double-float)
                           (note list)
                           (velocity fixnum)
                           (duration number)
                           (channel fixnum)
                           (offset number))
  "play arpeggio, at uniform times"
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
  "play arpeggio, with provided durations and offsets"
  (mapcar
   (lambda (n d o) (play-midi (+ time (* *sample-rate* (* (sample o) (spb *tempo*))))
                         n velocity d channel))
   note
   duration
   offset))
