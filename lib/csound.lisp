(in-package :shiny)

;; TODO:
;; - THere are ways to tell cffi to the allocation/conversion automatically
;; - Make an instrument library abstraction, so I can push an ORC with a custom
;;   set of instruments...nvm, tables are to thightly related to instruments
;;   combine or compose them won't be that easy...
;; - Make a parser for orcs, initially to get the signature of the insts, like
;;  if they need extra arguments or not. And create the classes accordingly
;; - Synths bring to the table the fact that I don't do much over time changes
;; - Then parse the instrument part too might be to get good default values
;; - But like baggers said "when you get a string in lisp is like it insults you"
;; - Aaaaaaand test ORCAsync

;; Copy csound's interfaces/csound.{lisp,asd} into
;; ~/.quicklisp/local-projects/csound/ then
;; I also re-defined :string instead of :pointer for csound:csoundreadscore and :orcproc

(ql:quickload :csound)

(defvar *c* nil)
(defvar *orcs* (make-hash-table))

;; TODO: add default args as a list for "extra" instead of a number
(defclass cinstrument ()
  ((iname :initarg :iname)
   (extra :initarg :extra :initform 0 :reader extra)))

;; FIXME: meh, needs to be a macro
(defmethod print-object ((obj cinstrument) out)
  (print-unreadable-object (obj out :type t)
    (format out "~s" (extra obj))))

;; HACKS!! ... p4 is kind of optional
(defgeneric playcsound (instrument keynum duration p4 &rest rest))
(defmethod playcsound
    ((i cinstrument) (keynum integer) (duration number) (p4 number) &rest rest)
  "plays a single note with INSTRUMENT
  (playcsound *ins* 60 1 2.0 .02)"
  (when (and (> keynum 0) (> duration 0))
    (with-slots (extra iname) i
      (let ((pch (incudine:keynum->pch keynum))
            (start-at 0)
            (lrest  (length rest))
            (lextra (length extra)))
        ;; Fill arguments if not provided or partially provided
        (cond ((= lrest 0)
               (setf rest extra))
              ((< lrest lextra)
               (setf rest (append rest (loop :for n :in (subseq extra lrest) :collect n)))))
        (csound:csoundreadscore
         *c*
         (concatenate
          'string
          iname
          (format nil "~{ ~A~}"
                  (append (list start-at duration p4 pch)
                          rest))))))))

(defmethod playcsound
    ((i cinstrument) (keynum list) (duration number) (p4 number) &rest rest)
  "plays chord of KEYNUM list
   (playcsound *ins* '(60 62 64) 1 2.0 .02)"
  (with-slots (extra iname) i
    (let ((pchs (mapcar (lambda (x) (incudine:keynum->pch x)) keynum))
          (start-at 0)
          (lrest (length rest))
          (lextra (length extra)))
      ;; TODO: multiple values?
      ;; Fill arguments if not provided or partially provided
      (cond ((= lrest 0)
             (setf rest extra))
            ((< lrest lextra)
             (setf rest (append rest (loop :for n :in (subseq extra lrest) :collect n)))))
      (mapcar
       (lambda (x)
         (csound:csoundreadscore
          *c*
          (concatenate
           'string
           iname
           (format nil "~{ ~A~}"
                   (append (list start-at duration p4 x)
                           rest)))))
       pchs))))

;; FIX: redundant loops..
(defmacro make-play (name i p4 &rest rest)
  (let ((fname (intern (format nil "~A-~A" 'play name))))
    `(let ((c (make-instance 'cinstrument :iname ,i :extra (list ,@rest))))
       (defun ,fname (keynum duration &optional (p4 ,p4) ,@(loop :for i :from 1 :for n :in rest :collect (list (intern (format nil "~a~d" 'arg i)) n)))
         (playcsound c keynum duration p4 ,@(loop :for i :from 1 :for n :in rest :collect (intern (format nil "~a~d" 'arg i))))))))

(defclass orc ()
  ((orc :initarg :orc)
   (table :initarg :table)))

(defun make-orc (name &key orc table)
  (declare (symbol name) (string orc table))
  (let ((o (make-instance 'orc :orc orc :table table)))
    (setf (gethash name *orcs*) o)))


;; (set-csound-options '("-odac" "--nchnls=2" "-+rtaudio=jack" "-M0" "-b128" "-B1048" "-+rtmidi=null"))
(defun set-csound-options (&optional (options '("-odac" "--nchnls=2")))
  (declare (list options))
  (loop :for option :in options :when (stringp option) :do
     (cffi:with-foreign-string (coption option)
       (csound:csoundsetoption *c* coption))))

;; TODO: ew
(defun start-csound (orchestra)
  (declare (type orc orchestra))
  (unless *c*
    (with-slots (orc table) orchestra
      (csound:csoundinitialize 3)
      (setf *c* (csound:csoundcreate (cffi:null-pointer)))
      (set-csound-options '("-odac" "--nchnls=2" "-M0" "-+rtmidi=null"))
      (csound:csoundcompileorc *c* orc)
      (csound:csoundstart *c*)
      (csound:csoundreadscore *c* table))))

(make-orc
 :xanadu
 :table "f1 0 8192 10 1
         f2 0 8192 11 1
         f3 0 8192 -12 20.0"
 :orc "
sr          =           44100
ksmps       =           100
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

(make-orc
 :trapped
 :table "
f1   0  8192  10   1
f2   0  512   10   10  8   0   6   0   4   0   1
f3   0  512   10   10  0   5   5   0   4   3   0   1
f4   0  2048  10   10  0   9   0   0   8   0   7   0  4  0  2  0  1
f5   0  2048  10   5   3   2   1   0
f6   0  2048  10   8   10  7   4   3   1
f7   0  2048  10   7   9   11  4   2   0   1   1
f8   0  2048  10   0   0   0   0   7   0   0   0   0  2  0  0  0  1  1
f9   0  2048  10   10  9   8   7   6   5   4   3   2  1
f10  0  2048  10   10  0   9   0   8   0   7   0   6  0  5
f11  0  2048  10   10  10  9   0   0   0   3   2   0  0  1
f12  0  2048  10   10  0   0   0   5   0   0   0   0  0  3
f13  0  2048  10   10  0   0   0   0   3   1
f14  0  512   9    1   3   0   3   1   0   9  .333   180
f15  0  8192  9    1   1   90
f16  0  2048  9    1   3   0   3   1   0   6   1   0
f17  0  9     5   .1   8   1
f18  0  17    5   .1   10  1   6  .4
f19  0  16    2    1   7   10  7   6   5   4   2   1   1  1  1  1  1  1  1
f20  0  16   -2    0   30  40  45  50  40  30  20  10  5  4  3  2  1  0  0  0
f21  0  16   -2    0   20  15  10  9   8   7   6   5   4  3  2  1  0  0
f22  0  9    -2   .001 .004 .007 .003 .002 .005 .009 .006"
 :orc "
sr     =        44100  
ksmps  =        100
nchnls =        2
;============================================================================;
;=============================== INITIALIZATION =============================;
;============================================================================;
garvb  init     0
gadel  init     0
;============================================================================;
;==================================== IVORY =================================;
;============================================================================;
       instr    1                            ; p6 = amp
ifreq  =        cpspch(p5)                   ; p7 = vib rate
                                             ; p8 = glis. del time (default < 1)
aglis  expseg   1, p8, 1, p3 - p8, p9        ; p9 = freq drop factor

k1     line     0, p3, 5
k2     oscil    k1, p7, 1
k3     linseg   0, p3 * .7, p6, p3 * .3, 0
a1     oscil    k3, (ifreq + k2) * aglis, 1

k4     linseg   0, p3 * .6, 6, p3 * .4, 0
k5     oscil    k4, p7 * .9, 1, 1.4
k6     linseg   0, p3 * .9, p6, p3 * .1, 0
a3     oscil    k6, ((ifreq + .009) + k5) * aglis, 9, .2

k7     linseg   9, p3 * .7, 1, p3 * .3, 1
k8     oscil    k7, p7 * 1.2, 1, .7
k9     linen    p6, p3 * .5, p3, p3 * .333
a5     oscil    k9, ((ifreq + .007) + k8) * aglis, 10, .3

k10    expseg   1, p3 * .99, 3.1, p3 * .01, 3.1
k11    oscil    k10, p7 * .97, 1, .6
k12    expseg   .001, p3 * .8, p6, p3 * .2, .001
a7     oscil    k12,((ifreq + .005) + k11) * aglis, 11, .5

k13    expseg   1, p3 * .4, 3, p3 * .6, .02
k14    oscil    k13, p7 * .99, 1, .4
k15    expseg   .001, p3 *.5, p6, p3 *.1, p6 *.6, p3 *.2, p6 *.97, p3 *.2, .001
a9     oscil    k15, ((ifreq + .003) + k14) * aglis, 12, .8

k16    expseg   4, p3 * .91, 1, p3 * .09, 1
k17    oscil    k16, p7 * 1.4, 1, .2
k18    expseg   .001, p3 *.6, p6, p3 *.2, p6 *.8, p3 *.1, p6 *.98, p3 *.1, .001
a11    oscil    k18, ((ifreq + .001) + k17) * aglis, 13, 1.3

       outs     a1 + a3 + a5, a7 + a9 + a11
       endin
;============================================================================;
;==================================== BLUE ==================================;
;============================================================================;
       instr 2                               ; p6 = amp
ifreq  =        cpspch(p5)                   ; p7 = reverb send factor
                                             ; p8 = lfo freq
k1     randi    1, 30                        ; p9 = number of harmonic
k2     linseg   0, p3 * .5, 1, p3 * .5, 0    ; p10 = sweep rate
k3     linseg   .005, p3 * .71, .015, p3 * .29, .01
k4     oscil    k2, p8, 1,.2
k5     =        k4 + 2

ksweep linseg   p9, p3 * p10, 1, p3 * (p3 - (p3 * p10)), 1

kenv   expseg   .001, p3 * .01, p6, p3 * .99, .001
asig   gbuzz    kenv, ifreq + k3, k5, ksweep, k1, 15

       outs     asig, asig
garvb  +=       (asig * p7)
       endin
;============================================================================;
;================================== VIOLET ==================================;
;============================================================================;
       instr   3                             ; p6 = amp
ifreq  =       cpspch(p5)                    ; p7 = reverb send factor
                                             ; p8 = rand freq
k3     expseg  1, p3 * .5, 30 ,p3 * .5, 2
k4     expseg  10, p3 *.7, p8, p3 *.3, 6
k8     linen   p6, p3 * .333, p3, p3 * .333
k13    line    0, p3, -1

k14    randh   k3, k4, .5
a1     oscil   k8, ifreq + (p5 * .05) + k14 + k13, 1, .1

k1     expseg  1, p3 * .8, 6, p3 *.2, 1
k6     linseg  .4, p3 * .9, p8 * .96, p3 * .1, 0
k7     linseg  8, p3 * .2, 10, p3 * .76, 2

kenv2  expseg  .001, p3 * .4, p6 * .99, p3 * .6, .0001
k15    randh   k6, k7
a2     buzz    kenv2, ifreq + (p5 * .009) + k15 + k13, k1, 1, .2

kenv1  linen   p6, p3 * .25, p3, p3 * .4
k16    randh   k4 * 1.4, k7 * 2.1, .2
a3     oscil   kenv1, ifreq + (p5 * .1) + k16 + k13, 16, .3

amix   =       a1 + a2 + a3
       outs    a1 + a3, a2 + a3
garvb  +=      (amix * p7)
       endin
;============================================================================;
;==================================== BLACK =================================;
;============================================================================;
       instr   4                             ; p6 = amp
ifreq  =       cpspch(p5)                    ; p7 = filtersweep strtfreq
                                             ; p8 = filtersweep endfreq
k1     expon   p7, p3, p8                    ; p9 = bandwidth
anoise rand    8000                          ; p10 = reverb send factor
a1     reson   anoise, k1, k1 / p9, 1
k2     oscil   .6, 11.3, 1, .1
k3     expseg  .001,p3 * .001, p6, p3 * .999, .001
a2     oscil   k3, ifreq + k2, 15

       outs   (a1 * .8) + a2, (a1 * .6) + (a2 * .7)
garvb  +=      (a2 * p10)
       endin
;============================================================================;
;==================================== GREEN =================================;
;============================================================================;
        instr  5                             ; p6 = amp
ifreq   =      cpspch(p5)                    ; p7 = reverb send factor
                                             ; p8 = pan direction
k1     line    p9, p3, 1                     ; ... (1.0 = L -> R, 0.1 = R -> L)
k2     line    1, p3, p10                    ; p9 = carrier freq
k4     expon   2, p3, p12                    ; p10 = modulator freq
k5     linseg  0, p3 * .8, 8, p3 * .2, 8     ; p11 = modulation index
k7     randh   p11, k4                       ; p12 = rand freq
k6     oscil   k4, k5, 1, .3

kenv1  linen   p6, .03, p3, .2
a1     foscil  kenv1, ifreq + k6, k1, k2, k7, 1

kenv2  linen   p6, .1, p3, .1
a2     oscil   kenv2, ifreq * 1.001, 1

amix   =       a1 + a2
kpan   linseg  int(p8), p3 * .7, frac(p8), p3 * .3, int(p8)
       outs    amix * kpan, amix * (1 - kpan)
garvb  +=       (amix * p7)
       endin
;============================================================================;
;================================== COPPER ==================================;
;============================================================================;
        instr  6                             ; p5 = FilterSweep StartFreq
ifuncl  =      8                             ; p6 = FilterSweep EndFreq
                                             ; p7 = bandwidth
k1      phasor p4                            ; p8 = reverb send factor
k2      table  k1 * ifuncl, 19               ; p9 = amp
anoise  rand   8000
k3      expon  p5, p3, p6
a1      reson  anoise, k3 * k2, k3 / p7, 1

kenv    linen  p9, .01, p3, .05
asig    =      a1 * kenv

        outs   asig, asig
garvb   +=  (asig * p8)
        endin
;============================================================================;
;==================================== PEWTER ================================;
;============================================================================;
       instr   7                             ; p4 = amp
ifuncl =       512                           ; p5 = freq
ifreq  =       cpspch(p5)                    ; p6 = begin phase point
                                             ; p7 = end phase point
a1     oscil   1, ifreq, p10                 ; p8 = ctrl osc amp (.1 -> 1)
k1     linseg  p6, p3 * .5, p7, p3 * .5, p6  ; p9 = ctrl osc func
a3     oscili  p8, ifreq + k1, p9            ; p10 = main osc func (f2 or f3)
a4     phasor  ifreq                         ; ...(function length must be 512!)
a5     table   (a4 + a3) * ifuncl, p10       ; p11 = reverb send factor

kenv   linen   p4, p3 * .4, p3, p3 * .5
asig   =       kenv * ((a1 + a5) * .2)

       outs    asig, asig
garvb  +=       (asig * p11)
       endin
;============================================================================;
;==================================== RED ===================================;
;============================================================================;
       instr   8                             ; p4 = amp
ifuncl =       16                            ; p5 = FilterSweep StartFreq
                                             ; p6 = FilterSweep EndFreq
k1     expon   p5, p3, p6                    ; p7 = bandwidth
k2     line    p8, p3, p8 * .93              ; p8 = cps of rand1
k3     phasor  k2                            ; p9 = cps of rand2
k4     table   k3 * ifuncl, 20               ; p10 = reverb send factor
anoise rand    8000
aflt1  reson   anoise, k1, 20 + (k4 * k1 / p7), 1

k5     linseg  p6 * .9, p3 * .8, p5 * 1.4, p3 * .2, p5 * 1.4
k6     expon   p9 * .97, p3, p9
k7     phasor  k6
k8     tablei  k7 * ifuncl, 21
aflt2  reson   anoise, k5, 30 + (k8 * k5 / p7 * .9), 1

abal   oscil   1000, 1000, 1
a3     balance aflt1, abal
a5     balance aflt2, abal

k11    linen   p4, .15, p3, .5
a3     =       a3 * k11
a5     =       a5 * k11

k9     randh   1, k2
aleft  =       ((a3 * k9) * .7) + ((a5 * k9) * .3)
k10    randh   1, k6
aright =       ((a3 * k10) * .3)+((a5 * k10) * .7)
       outs    aleft, aright
garvb  +=       (a3 * p10)
endin
;============================================================================;
;==================================== SAND ==================================;
;============================================================================;
        instr  9                             ; p4 = delay send factor
ifreq   =      cpspch(p5)                    ; p5 = freq
                                             ; p6 = amp
k2      randh  p8, p9, .1                    ; p7 = reverb send factor
k3      randh  p8 * .98, p9 * .91, .2        ; p8 = rand amp
k4      randh  p8 * 1.2, p9 * .96, .3        ; p9 = rand freq
k5      randh  p8 * .9, p9 * 1.3

kenv    linen  p6, p3 *.1, p3, p3 * .8

a1      oscil  kenv, ifreq + k2, 1, .2
a2      oscil  kenv * .91, (ifreq + .004) + k3, 2, .3
a3      oscil  kenv * .85, (ifreq + .006) + k4, 3, .5
a4      oscil  kenv * .95, (ifreq + .009) + k5, 4, .8

amix    =      a1 + a2 + a3 + a4

        outs   a1 + a3, a2 + a4
garvb   +=      (amix * p7)
gadel   +=      (amix * p4)
        endin
;============================================================================;
;==================================== TAUPE =================================;
;============================================================================;
        instr  10
ifreq   =      cpspch(p5)                    ; p5 = freq
                                             ; p6 = amp
k2      randh  p8, p9, .1                    ; p7 = reverb send factor
k3      randh  p8 * .98, p9 * .91, .2        ; p8 = rand amp
k4      randh  p8 * 1.2, p9 * .96, .3        ; p9 = rand freq
k5      randh  p8 * .9, p9 * 1.3

kenv    linen  p6, p3 *.1, p3, p3 * .8

a1      oscil  kenv, ifreq + k2, 1, .2
a2      oscil  kenv * .91, (ifreq + .004) + k3, 2, .3
a3      oscil  kenv * .85, (ifreq + .006) + k4, 3, .5
a4      oscil  kenv * .95, (ifreq + .009) + k5, 4, .8

amix    =      a1 + a2 + a3 + a4

        outs   a1 + a3, a2 + a4
garvb   +=      (amix * p7)
        endin
;============================================================================;
;==================================== RUST ==================================;
;============================================================================;
       instr   11                            ; p4 = delay send factor
ifreq  =       cpspch(p5)                    ; p5 = freq
                                             ; p6 = amp
k1     expseg  1, p3 * .5, 40, p3 * .5, 2    ; p7 = reverb send factor
k2     expseg  10, p3 * .72, 35, p3 * .28, 6
k3     linen   p6, p3* .333, p3, p3 * .333
k4     randh   k1, k2, .5
a4     oscil   k3, ifreq + (p5 * .05) + k4, 1, .1

k5     linseg  .4, p3 * .9, 26, p3 * .1, 0
k6     linseg  8, p3 * .24, 20, p3 * .76, 2
k7     linen   p6, p3 * .5, p3, p3 * .46
k8     randh   k5, k6, .4
a3     oscil   k7, ifreq + (p5 * .03) + k8, 14, .3

k9     expseg  1, p3 * .7, 50, p3 * .3, 2
k10    expseg  10, p3 * .3, 45, p3 * .7, 6
k11    linen   p6, p3 * .25, p3, p3 * .25
k12    randh   k9, k10, .5
a2     oscil   k11, ifreq + (p5 * .02) + k12, 1, .1

k13    linseg  .4, p3 * .6, 46, p3 * .4, 0
k14    linseg  18, p3 * .1, 50, p3 * .9, 2
k15    linen   p6, p3 * .2, p3, p3 * .3
k16    randh   k13, k14, .8
a1     oscil   k15, ifreq + (p5 * .01) + k16, 14, .3

amix   =       a1 + a2 + a3 + a4
       outs    a1 + a3, a2 + a4
garvb  +=       (amix * p7)
gadel  +=       (amix * p4)
       endin
;============================================================================;
;==================================== TEAL ==================================;
;============================================================================;
       instr   12                            ; p6 = amp
ifreq  =       octpch(p5)                    ; p7 = FilterSweep StartFreq
ifuncl =       8                             ; p8 = FilterSweep PeakFreq
                                             ; p9 = bandwdth
k1     linseg  0, p3 * .8, 9, p3 * .2, 1     ; p10 = reverb send factor
k2     phasor  k1
k3     table   k2 * ifuncl, 22
k4     expseg  p7, p3 * .7, p8, p3 * .3, p7 * .9

anoise rand    8000

aflt   reson   anoise, k4, k4 / p9, 1
kenv1  expseg  .001, p3 *.1, p6, p3 *.1, p6 *.5, p3 *.3, p6 *.8, p3 *.5,.001
a3     oscil   kenv1, cpsoct(ifreq + k3) + aflt * .8, 1

       outs    a3,(a3 * .98) + (aflt * .3)
garvb  +=  (anoise * p10)
       endin
;============================================================================;
;==================================== FOAM ==================================;
;============================================================================;
       instr   13                            ; p6 = amp
ifreq  =       octpch(p5)                    ; p7 = vibrato rate
                                             ; p8 = glis. factor
k1     line    0, p3, p8
k2     oscil   k1, p7, 1
k3     linseg  0, p3 * .7, p6, p3 * .3, 1
a1     oscil   k3, cpsoct(ifreq + k2), 1

k4     linseg  0, p3 * .6, p8 * .995, p3 * .4, 0
k5     oscil   k4, p7 * .9, 1, .1
k6     linseg  0, p3 * .9, p6, p3 * .1, 3
a2     oscil   k6, cpsoct((ifreq + .009) + k5), 4, .2

k7     linseg  p8 * .985, p3 * .7, 0, p3 * .3, 0
k8     oscil   k7, p7 * 1.2, 1, .7
k9     linen   p6, p3 * .5, p3, p3 * .333
a3     oscil   k6, cpsoct((ifreq + .007) + k8), 5, .5

k10    expseg  1, p3 * .8, p8, p3 * .2, 4
k11    oscil   k10, p7 * .97, 1, .6
k12    expseg  .001, p3 * .99, p6 * .97, p3 * .01, p6 * .97
a4     oscil   k12, cpsoct((ifreq + .005) + k11), 6, .8

k13    expseg  .002, p3 * .91, p8 * .99, p3 * .09, p8 * .99
k14    oscil   k13, p7 * .99, 1, .4
k15    expseg  .001, p3 *.5, p6, p3 *.1, p6 *.6, p3 *.2, p6 *.97, p3 *.2, .001
a5     oscil   k15, cpsoct((ifreq + .003) + k14), 7, .9

k16    expseg  p8 * .98, p3 * .81, .003, p3 * .19, .003
k17    oscil   k16, p7 * 1.4, 1, .2
k18    expseg  .001, p3 *.6, p6, p3 *.2, p6 *.8, p3 *.1, p6 *.98, p3 *.1, .001
a6     oscil   k18, cpsoct((ifreq + .001) + k17), 8, .1

       outs    a1 + a3 + a5, a2 + a4 + a6
       endin
;============================================================================;
;==================================== SMEAR =================================;
;============================================================================;
        instr  98
asig    delay  gadel, .08
        outs   asig, asig
gadel   =      0
        endin
;============================================================================;
;==================================== SWIRL =================================;
;============================================================================;
       instr   99                            ; p4 = panrate
k1     oscil   .5, p4, 1
k2     =       .5 + k1
k3     =       k2 - 1
asig   reverb  garvb, 2.1
       outs    asig * k2, asig * k3
garvb  =       0
       endin")
