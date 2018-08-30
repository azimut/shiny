(in-package :shiny)

;; TODO:
;; - THere are ways to tell cffi to the allocation/conversion automatically
;; - Make an instrument library abstraction, so I can push an ORC with a custom
;;   set of instruments
;; - Aaaaaaand test ORCAsync

;; Copy csound's interfaces/csound.{lisp,asd} into
;; ~/.quicklisp/local-projects/csound/ then
;; I also re-defined :string instead of :pointer for csound:csoundreadscore

(defvar *c* nil)
(defvar *orc*
  "sr          =           44100
ksmps       =           64 ; 100
nchnls      =           2
;--------------------------------------------------------
;Instrument 1 : plucked strings chorused left/right and
;       pitch-shifted and delayed taps thru exponential
;       functions, and delayed.
;--------------------------------------------------------

            instr       1
ishift      =           .00666667               ;shift it 8/1200.
ipch        =           cpspch(p5)              ;convert parameter 5 to cps.
ioct        =           octpch(p5)              ;convert parameter 5 to oct.
kvib        oscil       1/120, ipch/50, 1       ;vibrato
ag          pluck       2000, cpsoct(ioct+kvib), 1000, 1, 1
agleft      pluck       2000, cpsoct(ioct+ishift), 1000, 1, 1
agright     pluck       2000, cpsoct(ioct-ishift), 1000, 1, 1
kf1         expon       .1, p3, 1.0             ;exponential from 0.1 to 1.0
kf2         expon       1.0, p3, .1             ;exponential from 1.0 to 0.1
adump       delayr      2.0                     ;set delay line of 2.0 sec
atap1       deltapi     kf1                     ;tap delay line with kf1 func.
atap2       deltapi     kf2                     ;tap delay line with kf2 func.
ad1         deltap      2.0                     ;delay 2 sec.
ad2         deltap      1.1                     ;delay 1.1 sec.
            delayw      ag                      ;put ag signal into delay line.
            outs        agleft+atap1+ad1, agright+atap2+ad2
            endin
;-------------------------------------------------------------
;Instrument 2 : plucked strings chorused left/right and
;       pitch-shifted with fixed delayed taps.
;------------------------------------------------------------

            instr       2
ishift      =           .00666667               ;shift it 8/1200.
ipch        =           cpspch(p5)              ;convert parameter 5 to cps.
ioct        =           octpch(p5)              ;convert parameter 5 to oct.
kvib        oscil       1/120, ipch/50, 1       ;vibrato
ag          pluck       1000, cpsoct(ioct+kvib), 1000, 1, 1
agleft      pluck       1000, cpsoct(ioct+ishift), 1000, 1, 1
agright     pluck       1000, cpsoct(ioct-ishift), 1000, 1, 1
adump       delayr      0.3                     ;set delay line of 0.3 sec
ad1         deltap      0.1                     ;delay 100 msec.
ad2         deltap      0.2                     ;delay 200 msec.
            delayw      ag                      ;put ag sign into del line.
            outs        agleft+ad1, agright+ad2
            endin
;-----------------------------------------------------------
;Instrument 3 : New FM algorithm, modified to produce large timbre
;               shifts using modulation of I and r. Detuned chorusing employed.
;-----------------------------------------------------------
            instr       3
ishift      =           .00666667               ;shift it 8/1200.
ipch        =           cpspch(p5)              ;convert parameter 5 to cps.
ioct        =           octpch(p5)              ;convert parameter 5 to oct.
kadsr       linseg      0, p3/3, 1.0, p3/3, 1.0, p3/3, 0 ;ADSR envelope
kmodi       linseg      0, p3/3, 5, p3/3, 3, p3/3, 0 ;ADSR envelope for I
kmodr       linseg      p6, p3, p7              ;r moves from p6->p7 in p3 sec.
a1          =           kmodi*(kmodr-1/kmodr)/2
a1ndx       =           abs(a1*2/20)            ;a1*2 is normalized from 0-1.
a2          =           kmodi*(kmodr+1/kmodr)/2
a3          tablei      a1ndx, 3, 1             ;lookup tbl in f3, normal index
ao1         oscil       a1, ipch, 2             ;cosine
a4          =           exp(-0.5*a3+ao1)
ao2         oscil       a2*ipch, ipch, 2        ;cosine
aoutl       oscil       1000*kadsr*a4, ao2+cpsoct(ioct+ishift), 1 ;fnl outleft
aoutr       oscil       1000*kadsr*a4, ao2+cpsoct(ioct-ishift), 1 ;fnl outright
            outs        aoutl, aoutr
            endin
")

;; (set-csound-options '("-odac" "--nchnls=2" "-+rtaudio=jack" "-M0" "-b128" "-B1048" "-+rtmidi=null"))
(defun set-csound-options (&optional (options '("-odac" "--nchnls=2")))
  (declare (list options))
  (loop :for option :in options :when (stringp option) :do
     (cffi:with-foreign-string (coption option)
       (csound:csoundsetoption *c* coption))))

;; TODO: eww
(defun start-csound ()
  (unless *c*
    ;; disable handling of signals and stop
    (csound:csoundinitialize 3)
    (setf *c* (csound:csoundcreate (cffi:null-pointer)))
    (set-csound-options '("-odac" "--nchnls=2" "-M0" "-+rtmidi=null"))
    (cffi:with-foreign-string (corc *orc*)
      (csound:csoundcompileorc *c* corc))
    (csound:csoundstart *c*)))

(start-csound)

(bt:make-thread
 (lambda ()
   (loop (let ((frame (csound:csoundperformksmps *c*)))
           (when (not (= frame 0))
             (return))))))

;; ?
(csound:csoundreadscore *c* "f1 0 8192 10 1")
(csound:csoundreadscore *c* "f2 0 8192 11 1")
(csound:csoundreadscore *c* "f3 0 8192 -12 20.0")

;; TODO: add default args as a list for "extra" instead of a number
(defclass cinstrument ()
  ((iname :initarg :iname)
   (extra :initarg :extra :initform 0)))

(defgeneric playcsound (instrument keynum duration &rest rest))
(defmethod playcsound ((i cinstrument) (keynum integer) duration &rest rest)
  "plays a single note with INSTRUMENT
  (playcsound *ins* 60 1 2.0 .02)"
  (let ((pch (incudine:keynum->pch keynum))
        (start-at 0)
        (padding 0))
    (with-slots (iname extra) i
      (when (= (length rest) extra)
        (csound:csoundreadscore
         *c*
         (concatenate
          'string
          iname
          (format nil "~{ ~A~}"
                  (append (list start-at duration padding pch)
                          rest))))))))
(defmethod playcsound ((i cinstrument) (keynum list) duration &rest rest)
  "plays chord of KEYNUM list
   (playcsound *ins* '(60 62 64) 1 2.0 .02)"
  (let ((pchs (mapcar (lambda (x) (incudine:keynum->pch x)) keynum))
        (start-at 0)
        (padding 0))
    (with-slots (iname extra) i
      (when (= (length rest) extra)
        (mapcar
         (lambda (x)
           (csound:csoundreadscore
            *c*
            (concatenate
             'string
             iname
             (format nil "~{ ~A~}"
                     (append (list start-at duration padding x)
                             rest)))))
         pchs)))))

(defparameter *inst1*
  (make-instance 'cinstrument :iname "i1"))
(defparameter *inst2*
  (make-instance 'cinstrument :iname "i2"))
(defparameter *inst3*
  (make-instance 'cinstrument :iname "i3" :extra 2))

;;(csound:csounddestroy *c*)

(defparameter *expand*
  (loop :for n :from 1 :to 3 :by .1 :collect
     (mapcar #'round
             (cm:expwarp (make-chord-fixed 60 3 (scale 0 'ryukyu)) n))))

(defparameter *spectrum*
  (mapcar
   (lambda (x)
     (mapcar #'round
             (cm:scale-spectrum-low (make-chord-fixed 60 3 (scale 0 'ryukyu)) x)))
   (cm:placereg (cm:heapvec 12) 3)))

(defpattern k ((gethash 'getup *patterns*) .5)
  (playcsound *inst3* *gm-kick* d 4 .9)
  (playcsound *inst3* *gm-snare* d 2 .2)
  (playcsound *inst3* *gm-closed-hi-hat* d 4 .1))

(defun k ())
(k (now))
(defun f ())
(let ((chord (make-cycle (make-chord-fixed 60 3 (scale 0 'ryukyu))))
      (here  (make-cycle '(t nil)))
      (lead  (make-cycle (make-chord-fixed 80 5 (scale 0 'ryukyu)))))
  (defun f (time)
    (let ((n (next chord)))
      (playcsound *inst3* n 2 2 .2)
      ;; (if (odds .5)
      ;;     (p time 60 60 1 0)
      ;;     (progn
      ;;       (p time 60 60 .5 2 :pan 0)
      ;;       (p (+ time #[.5 b]) 60 60 .5 3 :pan 127)))
      ;; (pa time (list (pickl '(79 83 84 88 89)) 0)
      ;;     4 50 4 4)
      (when (next here)
        (playcsound *inst2* (next lead) 4)))
    (aat (+ time #[2 b])
         #'f it)))

(fp 4 52)
(f (tempo-sync #[1 b]))

(let ((chord (make-cycle (make-chord-fixed 60 3 (scale 0 'ryukyu))))
      (here  (make-cycle '(t nil)))
      ;;(lead  (make-cycle (make-chord-fixed 80 5 (scale 0 'ryukyu))))
      (lead  (make-cycle *spectrum*)))
  (defun f (time)
    (let ((n (next chord)))
      ;;(playcsound *inst3* n 2 2 .2)
      ;; (if (odds .5)
      ;;     (p time 60 60 1 0)
      ;;     (progn
      ;;       (p time 60 60 .5 2 :pan 0)
      ;;       (p (+ time #[.5 b]) 60 60 .5 3 :pan 127)))
      (when (next here)
        (playcsound *inst1* (next lead) 4)))
    (aat (+ time #[2 b])
         #'f it)))

(defpattern k ((gethash 'wm *patterns*) .25)
  (bbplay "kick_OH_F_9.wav" :attenuation .2 :rate 1)
  (bbplay "snare_OH_FF_9.wav" :attenuation .2 :rate .2)
  (bbplay "hihatClosed_OH_F_20.wav" :attenuation .05 :rate 2)
  ;;(bbplay "hihatOpen_OH_FF_6.wav" :attenuation .05 :rate -2)
  )

(k (tempo-sync #[1 b]))
(f (tempo-sync #[1 b]))

(defun f ())
(defun k ())

(let ((chord (make-cycle (make-chord-fixed 60 3 (scale 0 'ryukyu))))
      (here  (make-cycle '(t nil)))
      ;;(lead  (make-cycle (make-chord-fixed 80 5 (scale 0 'ryukyu))))
      (lead  (make-line *expand*)))
  (defun f (time)
    (let ((n (next chord)))
      (playcsound *inst3* (+ 12 n) 2 2 .2)
      ;; (if (odds .5)
      ;;     (p time 60 60 1 0)
      ;;     (progn
      ;;       (p time 60 60 .5 2 :pan 0)
      ;;       (p (+ time #[.5 b]) 60 60 .5 3 :pan 127)))
      (when (next here)
        (playcsound *inst3* (cm:transp (next lead) 0) 2 3 0.4)))
    (aat (+ time #[1 b])
         #'f it)))

