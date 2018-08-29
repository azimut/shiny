(in-package :shiny)

(defgeneric p (time pitch velocity duration channel)
  (:method ((time double-float) (pitch list) velocity duration channel)
    "Play chord of notes"
    (mapcar (lambda (x) (p time x velocity duration channel))
            pitch))
  (:method ((time double-float) (pitch list) velocity duration (channel list))
    "Play chord of notes, on provided channels"
    (mapcar (lambda (x y) (p time x velocity duration y))
            pitch
            channel))
  (:method ((time double-float) (pitch integer) (velocity integer) (duration number) (channel integer))
    "Play given numerical pitch"
    (at time #'fluidsynth:noteon *synth* channel pitch velocity)
    (at (+ time #[duration b]) #'fluidsynth:noteoff *synth* channel pitch))
  (:method ((time double-float) (pitch integer) (velocity integer) (duration symbol) (channel integer))
    "Play given numerial pitch, at CM rythm"
    (let ((d (cm:rhythm duration)))
      (at time #'fluidsynth:noteon *synth* channel pitch velocity)
      (at (+ time #[d b]) #'fluidsynth:noteoff *synth* channel pitch)))
  (:method ((time double-float) (pitch symbol) (velocity integer) (duration symbol) (channel integer))
    "Play given note, at CM rhythm"
    (unless (eql :_ pitch)
      (let ((n (note pitch))
            (d (cm:rhythm duration)))
        (at time #'fluidsynth:noteon
            *synth* channel n velocity)
        (at (+ time #[d b]) #'fluidsynth:noteoff
            *synth* channel n))))
  (:method ((time double-float) (pitch symbol) (velocity integer) (duration number) (channel integer))
    "Play given note"
    (unless (eql :_ pitch)
      (let ((n (note pitch)))
        (at time #'fluidsynth:noteon
            *synth* channel n velocity)
        (at (+ time #[duration b]) #'fluidsynth:noteoff
            *synth* channel n)))))

(defun pc (time notes velocity duration channel)
  (mapcar (lambda (x) (p time x velocity duration channel))
          notes))

(defgeneric pa (time notes offset velocity channel &key dur)
  (:method ((time double-float) (notes list) (offset number) (velocity integer) (channel integer)
            &key (dur offset))
    "Play arpeggio, with a constant offset and velocity"
    (let* ((lnotes  (length notes))
           (offsets (loop :for i :from 0 :by offset :collect i :repeat lnotes)))
      (with-schedule
        (mapcar (lambda (x y) (p (+ time #[y b]) x velocity dur channel))
               notes
               offsets))))
  (:method ((time double-float) (notes list) (offset number) (velocity integer) (channel list)
            &key (dur offset))
    "Play arpeggio, with a constant offset and velocity"
    (let* ((lnotes  (length notes))
           (offsets (loop :for i :from 0 :by offset :collect i :repeat lnotes)))
      (mapcar (lambda (x y z) (p (+ time #[y b]) x velocity dur z))
              notes
              offsets
              channel)))
  (:method ((time double-float) (notes list) (offset number) (velocity list) (channel integer)
            &key (dur offset))
    "Play arpeggio, with a constant offset and provided velocities"
    (let* ((lnotes  (length notes))
           (offsets (loop :for i :from 0 :by offset :collect i :repeat lnotes)))
      (mapcar (lambda (x y z) (p (+ time #[y b]) x z dur channel))
              notes
              offsets
              velocity)))
  (:method ((time double-float) (notes list) (offset number) (velocity list) (channel list)
            &key (dur offset))
    "Play arpeggio, with a constant offset and provided velocities"
    (let* ((lnotes  (length notes))
           (offsets (loop :for i :from 0 :by offset :collect i :repeat lnotes)))
      (mapcar (lambda (x y z a) (p (+ time #[y b]) x z dur a))
              notes
              offsets
              velocity
              channel)))
  (:method ((time double-float) (notes list) (offset list) (velocity integer) (channel integer)
            &key (dur 1))
    "Play arpeggio, with provided offsets and constant velocity"
    (mapcar (lambda (x y) (p (+ time #[y b]) x velocity dur channel))
            notes
            offset))
  (:method ((time double-float) (notes list) (offset list) (velocity integer) (channel list)
            &key (dur 1))
    "Play arpeggio, with provided offsets and constant velocity"
    (mapcar (lambda (x y z) (p (+ time #[y b]) x velocity dur z))
            notes
            offset
            channel))
  (:method ((time double-float) (notes list) (offset list) (velocity list) (channel integer)
            &key (dur 1))
    "Play arpeggio, with provided offsets and velocities"
    (mapcar (lambda (x y z) (p (+ time #[y b]) x z dur channel))
            notes
            offset
            velocity))
  (:method ((time double-float) (notes list) (offset list) (velocity list) (channel list)
            &key (dur 1))
    "Play arpeggio, with provided offsets and velocities"
    (mapcar (lambda (x y z a) (p (+ time #[y b]) x z dur a))
            notes
            offset
            velocity
            channel)))

(defun play-midi-note (time pitch velocity dur c)
  (when (and pitch (> pitch 2))
    (at time #'fluidsynth:noteon *synth* c pitch velocity)
    (at (+ time #[dur b]) #'fluidsynth:noteoff *synth* c pitch)))

(defun loop-rhythm (time notes rhythms velocity channel
                    &optional (hownotes 'cdr) (howrhythms 'cdr))
  (let ((note   (if (eql hownotes 'next) (cm:next notes) (car notes)))
        (rhythm (if (eql howrhythms 'next) (cm:next rhythms) (car rhythms))))
    (p time note velocity rhythm channel)
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
         velocity
         channel
         hownotes howrhythms)))
