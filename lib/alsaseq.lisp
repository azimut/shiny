(in-package #:shiny)


;; (midihelper:start-midihelper)

;; (p)
;; remove pan
;; replace method name
;; macro replaced noteon & noteoff

(defgeneric play-midi-note (time pitch velocity duration channel))

;; Stop reusing p here. WHY?
(defmethod play-midi-note ((time double-float) (pitch list) (velocity fixnum) (duration number) (channel fixnum))
  "Play chord of notes"
  (map nil (lambda (p)
             (declare (fixnum p))
             (when (> duration 0)
               (at time #'midihelper:send-event (midihelper:ev-noteon channel p velocity))
               (at (+ time (calc-beats duration)) #'midihelper:send-event (midihelper:ev-noteoff channel p velocity))))
       pitch))
(defmethod play-midi-note ((time double-float) (pitch list) (velocity fixnum) (duration number) (channel list))
  "Play chord of notes, on provided channels"
  (map nil (lambda (p c)
             (declare (fixnum p c))
             (play-midi-note time p velocity duration c))
       pitch
       channel))
(defmethod play-midi-note ((time double-float) (pitch fixnum) (velocity fixnum) (duration number) (channel fixnum))
  "Play given pitch"
  (when (and (< 0 pitch 127)
             (> duration 0))
    (at time #'midihelper:send-event (midihelper:ev-noteon channel pitch velocity))
    (at (+ time (calc-beats duration)) #'midihelper:send-event (midihelper:ev-noteoff channel pitch velocity))
    pitch))
(defmethod play-midi-note ((time double-float) (pitch fixnum) (velocity fixnum) (duration symbol) (channel fixnum))
  "Play given pitch, at CM rythm"
  (let ((d (cm:rhythm duration)))
    (when (> d 0)
      (at time #'midihelper:send-event (midihelper:ev-noteon channel pitch velocity))
      (at (+ time (calc-beats d)) #'midihelper:send-event (midihelper:ev-noteoff channel pitch velocity))
      pitch)))
(defmethod play-midi-note ((time double-float) (pitch symbol) (velocity fixnum) (duration symbol) (channel fixnum))
  "Play given note on american notation, at CM rhythm"
  (unless (and (eql :_ pitch) (eql 'cm::r pitch) (eql NIL pitch))
    (let ((n (if (keywordp pitch) (note pitch) (cm:keynum pitch)))
          (d (cm:rhythm duration)))
      (declare (fixnum n))
      (when (> d 0)
        (at time #'midihelper:send-event (midihelper:ev-noteon channel n velocity))
        (at (+ time (calc-beats d)) #'midihelper:send-event (midihelper:ev-noteoff channel n velocity))
        n))))
(defmethod play-midi-note ((time double-float) (pitch symbol) (velocity fixnum) (duration number) (channel fixnum))
  "Play given note on american notation"
  (when (and (> duration 0)
             (not (eql NIL pitch))
             (not (eql :_ pitch))
             (not (eql 'cm::r pitch)))
    (let ((n (if (keywordp pitch) (note pitch) (cm:keynum pitch))))
      (declare (fixnum n))
      (at time #'midihelper:send-event (midihelper:ev-noteon channel n velocity))
      (at (+ time (calc-beats duration)) #'midihelper:send-event (midihelper:ev-noteoff channel n velocity))
      n)))

;;--------------------------------------------------

(defgeneric play-midi-arp (time notes velocity duration channel offset)
  (:documentation "Play the given notes as an arpeggio"))

;; Single note, single all
(defmethod play-midi-arp ((time double-float) (notes number) (velocity fixnum) (duration number) (channel fixnum) (offset number))
  (when (>= offset 0)
    (play-midi-note (+ time (calc-beats offset)) notes velocity duration channel)))

;; Single note, multiple other things
(defmethod play-midi-arp ((time double-float) (notes number) (velocity list) (duration number) (channel fixnum) (offset number))
  (let* ((lnotes  (length velocity))
         (offsets (loop :for i :from 0 :by offset :collect i :repeat lnotes)))
    (map nil (lambda (v o)
               (declare (fixnum v))
               (play-midi-note (+ time (calc-beats o)) notes v duration channel))
         velocity
         offsets)))
(defmethod play-midi-arp ((time double-float) (notes number) (velocity fixnum) (duration list) (channel fixnum) (offset number))
  (let* ((lnotes  (length duration))
         (offsets (loop :for i :from 0 :by offset :collect i :repeat lnotes)))
    (map nil (lambda (d o)
               (play-midi-note (+ time (calc-beats o)) notes velocity d channel))
         duration
         offsets)))
(defmethod play-midi-arp ((time double-float) (notes number) (velocity fixnum) (duration number) (channel fixnum) (offset list))
  (map nil (lambda (o)
             (when (>= o 0)
               (play-midi-note (+ time (calc-beats o)) notes velocity duration channel)))
       offset))
(defmethod play-midi-arp ((time double-float) (notes number) (velocity list) (duration number) (channel fixnum) (offset list))
  (map nil (lambda (v o)
             (declare (fixnum v))
             (when (>= o 0)
               (play-midi-note (+ time (calc-beats o)) notes v duration channel)))
       velocity
       offset))
(defmethod play-midi-arp ((time double-float) (notes number) (velocity fixnum) (duration list) (channel fixnum) (offset list))
  (map nil (lambda (d o)
             (when (>= o 0)
               (play-midi-note (+ time (calc-beats o)) notes velocity d channel)))
       duration
       offset))

;; Multiple notes
(defmethod play-midi-arp ((time double-float) (notes list) (velocity fixnum) (duration number) (channel fixnum) (offset number))
  (let* ((lnotes  (length notes))
         (offsets (loop :for i :from 0 :by offset :collect i :repeat lnotes)))
    (map nil (lambda (n o)
               ;;               (declare (fixnum n) (number o))
               (play-midi-note (+ time (calc-beats o)) n velocity duration channel))
         notes
         offsets)))
(defmethod play-midi-arp ((time double-float) (notes list) (velocity fixnum) (duration number) (channel list) (offset number))
  (let* ((lnotes  (length notes))
         (offsets (loop :for i :from 0 :by offset :collect i :repeat lnotes)))
    (map nil (lambda (n o c)
               (declare (fixnum c))
               (play-midi-note (+ time (calc-beats o)) n velocity duration c))
         notes
         offsets
         channel)))
(defmethod play-midi-arp ((time double-float) (notes list) (velocity list) (duration number) (channel fixnum) (offset number))
  (let* ((lnotes  (length notes))
         (offsets (loop :for i :from 0 :by offset :collect i :repeat lnotes)))
    (map nil (lambda (n o v)
               (declare (fixnum v))
               (play-midi-note (+ time (calc-beats o)) n v duration channel))
         notes
         offsets
         velocity)))
(defmethod play-midi-arp ((time double-float) (notes list) (velocity list) (duration number) (channel list) (offset number))
  (let* ((lnotes  (length notes))
         (offsets (loop :for i :from 0 :by offset :collect i :repeat lnotes)))
    (map nil (lambda (n o v c)
               (declare (fixnum v c))
               (play-midi-note (+ time (calc-beats o)) n v duration c))
         notes
         offsets
         velocity
         channel)))
(defmethod play-midi-arp ((time double-float) (notes list) (velocity fixnum) (duration list) (channel fixnum) (offset number))
  (let* ((lnotes  (length notes))
         (offsets (loop :for i :from 0 :by offset :collect i :repeat lnotes)))
    (map nil (lambda (n o d)
               ;;(declare (fixnum n))
               (play-midi-note (+ time (calc-beats o)) n velocity d channel))
         notes
         offsets
         duration)))
(defmethod play-midi-arp ((time double-float) (notes list) (velocity fixnum) (duration list) (channel list) (offset number))
  (let* ((lnotes  (length notes))
         (offsets (loop :for i :from 0 :by offset :collect i :repeat lnotes)))
    (map nil (lambda (n o c d)
               (declare (fixnum c))
               (play-midi-note (+ time (calc-beats o)) n velocity d c))
         notes
         offsets
         channel
         duration)))
(defmethod play-midi-arp ((time double-float) (notes list) (velocity list) (duration list) (channel fixnum) (offset number))
  (let* ((lnotes  (length notes))
         (offsets (loop :for i :from 0 :by offset :collect i :repeat lnotes)))
    (map nil (lambda (n o v d)
               (declare (fixnum v))
               (play-midi-note (+ time (calc-beats o)) n v d channel))
         notes
         offsets
         velocity
         duration)))
(defmethod play-midi-arp ((time double-float) (notes list) (velocity list) (duration list) (channel list) (offset number))
  (let* ((lnotes  (length notes))
         (offsets (loop :for i :from 0 :by offset :collect i :repeat lnotes)))
    (map nil (lambda (n o v c d)
               (declare (fixnum v c))
               (play-midi-note (+ time (calc-beats o)) n v d c))
         notes
         offsets
         velocity
         channel
         duration)))
(defmethod play-midi-arp ((time double-float) (notes list) (velocity fixnum) (duration number) (channel fixnum) (offset list))
  (map nil (lambda (n o)
             ;;(declare (fixnum n))
             (play-midi-note (+ time (calc-beats o)) n velocity duration channel))
       notes
       offset))
(defmethod play-midi-arp ((time double-float) (notes list) (velocity fixnum) (duration number) (channel list) (offset list))
  (map nil (lambda (n o c)
             (declare (fixnum c))
             (play-midi-note (+ time (calc-beats o)) n velocity duration c))
       notes
       offset
       channel))
(defmethod play-midi-arp ((time double-float) (notes list) (velocity list) (duration number) (channel fixnum) (offset list))
  (map nil (lambda (n o v)
             (declare (fixnum v))
             (play-midi-note (+ time (calc-beats o)) n v duration channel))
       notes
       offset
       velocity))
(defmethod play-midi-arp ((time double-float) (notes list) (velocity list) (duration number) (channel list) (offset list))
  (map nil (lambda (n o v c)
             (declare (fixnum v c))
             (play-midi-note (+ time (calc-beats o)) n v duration c))
       notes
       offset
       velocity
       channel))
(defmethod play-midi-arp ((time double-float) (notes list) (velocity fixnum) (duration list) (channel fixnum) (offset list))
  (map nil (lambda (n o d)
             ;;(declare (fixnum n))
             (play-midi-note (+ time (calc-beats o)) n velocity d channel))
       notes
       offset
       duration))
(defmethod play-midi-arp ((time double-float) (notes list) (velocity fixnum) (duration list) (channel list) (offset list))
  (map nil (lambda (n o c d)
             (declare (fixnum c))
             (play-midi-note (+ time (calc-beats o)) n velocity d c))
       notes
       offset
       channel
       duration))
(defmethod play-midi-arp ((time double-float) (notes list) (velocity list) (duration list) (channel fixnum) (offset list))
  (map nil (lambda (n o v d)
             (declare (fixnum v))
             (play-midi-note (+ time (calc-beats o)) n v d channel))
       notes
       offset
       velocity
       duration))
(defmethod play-midi-arp ((time double-float) (notes list) (velocity list) (duration list) (channel list) (offset list))
  (map nil (lambda (n o v c d)
             (declare (fixnum v c))
             (play-midi-note (+ time (calc-beats o)) n v d c))
       notes
       offset
       velocity
       channel
       duration))
