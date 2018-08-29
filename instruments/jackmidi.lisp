(in-package :shiny)

(defvar *midiout* (jackmidi:open :direction :output))
;; Connect with Carla/qjackctl

;; TODO: I only have 1 "channel", so might be i need to send the *midiout* as a var in place of the channel
;; Although...that will make the code not work with other compositions...
;; ... unless I have some mapping (?

;; http://computermusicresource.com/MIDI.Commands.html
(defun to-channel (code)
  (+ code 143))

(defun off-with-the-notes ()
  (loop :for c :in (range 20)
     :do (mapcar (lambda (x) (jackmidi:write-short *midiout* (jackmidi:message (+ c 143) x 0) 3))
                              (range 120))))

(defun play-midi-note (time pitch velocity dur &optional (c 1))
  (at time #'jackmidi:write-short *midiout*
      (jackmidi:message c pitch velocity) 3)
  (at (+ time #[dur b]) #'jackmidi:write-short *midiout*
      (jackmidi:message c pitch 0) 3))

(defun play-midi-note-loop (time pitch velocity dur &optional (c 1))
  (at time #'jackmidi:write-short *midiout*
      (jackmidi:message c pitch velocity) 3)
  (at (+ time #[dur b]) #'jackmidi:write-short *midiout*
      (jackmidi:message c pitch 0) 3))

(defgeneric p (time pitch velocity duration channel)
  (:method (time (pitch integer) velocity (duration number) channel)
    (at time #'jackmidi:write-short *midiout*
        (jackmidi:message 144 pitch velocity) 3)
    (at (+ time #[duration b]) #'jackmidi:write-short *midiout*
        (jackmidi:message 128 pitch 0) 3))
  (:method (time (pitch integer) velocity (duration symbol) channel)
    (let ((d (cm:rhythm duration)))
      (at time #'jackmidi:write-short *midiout*
          (jackmidi:message 144 pitch velocity) 3)
      (at (+ time #[d b]) #'jackmidi:write-short *midiout*
          (jackmidi:message 128 pitch 0) 3)))
  (:method (time (pitch symbol) velocity (duration symbol) channel)
    (unless (eql :_ pitch)
      (let ((n (note pitch))
            (d (cm:rhythm duration)))
        (at time #'jackmidi:write-short *midiout*
            (jackmidi:message 144 n velocity) 3)
        (at (+ time #[d b]) #'jackmidi:write-short *midiout*
            (jackmidi:message 128 n 0) 3))))
  (:method (time (pitch symbol) velocity duration channel)
    (unless (eql :_ pitch)
      (let ((n (note pitch)))
        (at time #'jackmidi:write-short *midiout*
            (jackmidi:message 144 n velocity) 3)
        (at (+ time #[duration b]) #'jackmidi:write-short *midiout*
            (jackmidi:message 128 n 0) 3)))))

(defun pc (time notes velocity duration channel)
  (mapcar (lambda (x) (p time x velocity duration channel))
          notes))

(defgeneric pa (time notes offset velocity &key channel dur)
  (:method (time notes (offset number) (velocity integer)
            &key (channel 1) (dur offset))
    "Play arpeggio, with a constant offset and velocity"
    (let* ((lnotes  (length notes))
           (offsets (loop :for i :from 0 :by offset :collect i :repeat lnotes)))
      (mapcar (lambda (x y) (p (+ time #[y b]) x velocity dur channel))
              notes
              offsets)))
  (:method (time notes (offset number) (velocity list)
            &key (channel 1) (dur offset))
    "Play arpeggio, with a constant offset and provided velocities"
    (let* ((lnotes  (length notes))
           (offsets (loop :for i :from 0 :by offset :collect i :repeat lnotes)))
      (mapcar (lambda (x y z) (p (+ time #[y b]) x z dur channel))
              notes
              offsets
              velocity)))
  (:method (time notes (offset list) (velocity integer) &key (channel 1) (dur 1))
    "Play arpeggio, with provided offsets and constant velocity"
    (mapcar (lambda (x y) (p (+ time #[y b]) x velocity dur channel))
            notes
            offset))
  (:method (time notes (offset list) (velocity list) &key (channel 1) (dur 1))
    "Play arpeggio, with provided offsets and velocities"
    (mapcar (lambda (x y z) (p (+ time #[y b]) x z dur channel))
            notes
            offset
            velocity)))

(defun loop-rhythm (time notes rhythms
                    &optional (hownotes 'cdr) (howrhythms 'cdr))
  (let ((note   (if (eql hownotes 'next) (cm:next notes) (car notes)))
        (rhythm (if (eql howrhythms 'next) (cm:next rhythms) (car rhythms))))
    (p time note 60 rhythm 1)
    (aat (+ time #[rhythm b]) #'loop-rhythm
         it 
        (case hownotes
           (cdr     (cdr notes))
           (rotate  (rotate notes  1))
           (rrotate (rotate notes -1))
           (next    notes))
         (case howrhythms
           (cdr     (cdr rhythms))
           (rotate  (rotate rhythms  1))
           (rrotate (rotate rhythms -1))
           (next    rhythms))
         hownotes howrhythms)))

