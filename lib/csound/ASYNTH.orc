; Steven Cook
instr    1 ; Synth With 2 VCO's, 2 LFO's, PWM, Glide And Ringmod.
 
 ilevl    = p4 * 32767 ; Level
 inote    = cpspch(p5) ; Pitch
 iport    = .0625      ; Glide Time
 ivco2    = p6         ; VCO2 Offset
 idet     = p7         ; VCO2 Detune
 ivcf1    = p8         ; VCF Freq Start
 ivcf2    = p9         ; VCF Freq End
 irez     = p10        ; VCF Resonance
 iwav1    = p11        ; VCO1 Waveform
 iwav2    = p12        ; VCO2 Waveform
 iwave    = p13        ; LFO Waveform
 irate1   = p14        ; LFO1 Rate
 irate2   = p15        ; LFO2 Rate
 iring    = p16        ; Ringmod Level.
 imaxd    = 1/inote
 
 tigoto   tieinit
 ibegp    = inote
 iprev    = inote
 goto     cont
 
 tieinit:
 ibegp    = iprev
 iprev    = inote
 cont:
 
 k1       linseg  ibegp, iport, inote, abs(p3), inote
 k3       expseg  ivcf1, abs(p3), ivcf2
 k4       linseg  0, .01, ilevl, abs(p3)-.02, ilevl, .01, 0
 
 kpwm1    oscil  .45, irate1, iwave, -1
 kpw1     = kpwm1 + .5
 kpwm2    oscil  .45, irate2, iwave, -1
 kpw2     = kpwm2 + .5
 
 avco1    vco  k4, k1, iwav1, kpw1, 1, imaxd
 avco2    vco  k4, k1*ivco2 + idet, iwav2, kpw2, 1, imaxd
 aring    = avco1*avco2/32767
 
 amix     = (avco1 + avco2)*(1-iring) + aring*iring
 
 avcf     rezzy  amix, k3, irez
 
 out      avcf
 
 endin