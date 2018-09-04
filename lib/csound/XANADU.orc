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