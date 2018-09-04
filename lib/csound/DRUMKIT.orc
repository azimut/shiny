gaDL	init	0
gaDR	init	0

instr 1

;Bass drum

idur		=	p3
iamp		=	p4
iwave		=	p5
ipulse	=	p6
ienv		=	p7
irhytable	=	p8
icps		=	cpspch (p9)
igliss	=	p10
isend		=	p11

;=====RHYTHMS============
krhythm	phasor	ipulse/ftlen(irhytable)
krhythm	table		krhythm*ftlen(irhytable), irhytable

;=====ENVELOPE========
kenvindex	phasor	ipulse
kenv		tablei	kenvindex*ftlen(ienv), ienv

;======ADD GLISS, makes it sound more like a bass drum?========

kgliss	phasor	ipulse
kgliss	tablei	kgliss * ftlen (igliss), igliss


;=====SOUND=======

athump	oscili	1,icps*kgliss*.5, iwave

;=====MIX signal ========

athump	=		athump * kenv * krhythm * iamp

kpan		=	.5

aL		=		athump*kpan
aR		=		athump*(1-kpan)

		outs		aL, aR

gaDL		=		aL*isend + gaDL
gaDR		=		aR*isend + gaDR

endin

;=============================================================================
;=============================================================================
;=============================================================================

instr 2

;hihat

idur 		=	p3
iamp		=	p4
ipulse	=	p5
irhytable	=	p6
ienv		=	p7
ienv2		=	p8
ipan		=	p9
isend		=	p10

;=====RHYTHM========
krhythm	phasor	ipulse/(ftlen (irhytable))
krhythm	table		krhythm*(ftlen(irhytable)), irhytable

;open (longer envelope)

kenvindex		phasor	ipulse
kenv			tablei	kenvindex*ftlen(ienv), ienv

; closed (shorter envelope)

kenv2			tablei	kenvindex*ftlen(ienv2), ienv2

;randomly chose envelope

kwhichenv		randh		5, ipulse, 1
kwhichenv		=		abs (kwhichenv)
kwhichenv		=	(kwhichenv < 2 ? 0 : 1 )

; mix envelopes
kenv		=		kenv*kwhichenv + kenv2 * (1- kwhichenv)


;====create sound ========

asound	rand		1
asound	=		asound*kenv

; filter sound

asound	butterbp	asound, cpspch (5.08+8), cpspch (5.08+8) * .5
asound	moogvcf	asound, cpspch (5.08+8), .7
a1		comb		asound, .5, 1/cpspch(8)
asound		=		a1*.05 + asound*.93


; mix
asound	=		asound*iamp*krhythm

; pan

ipan		=		.7

aL		=		asound*ipan
aR		=		asound*(1-ipan)

		outs		aL, aR

gaDL		=		aL*isend + gaDL
gaDR		=		aR*isend + gaDR

endin

;=============================================================================
;=============================================================================
;=============================================================================


instr 3

;snare

idur		=	p3
iamp		=	p4
ipulse	=	p5
ienv		=	p6
ienv2		=	p7
irhytable	=	p8
ipan		=	p9
isend		=	p10

;=====NOISE=======

asound	rand		1

;=====filternoise========

as1		butterbp	asound, 210, 55


;===one envelope (usually sharp for noise) ========

kenv2		phasor	ipulse
kenv2		table		kenv2*ftlen(ienv2), ienv2

;=== mix signals ========

asound	=		as1*.9 + asound*.8*kenv2

;=== another envelope =======

kenv		phasor	ipulse
kenv		tablei	kenv*ftlen(ienv), ienv

;====GET RHYTHMS======

krhythm	phasor	ipulse/ftlen(irhytable)
krhythm	table		krhythm*ftlen(irhytable), irhytable

;====MIX SIGNAL=======

asound	=		asound*kenv*krhythm*iamp

;==== PANNING ========


aL		=		asound*ipan
aR		=		asound*(1-ipan)

		outs		aL, aR

gaDL		=		aL*isend + gaDL
gaDR		=		aR*isend + gaDR

endin

;=============================================================================
;=============================================================================
;=============================================================================


instr 4

;crash

idur 		=	p3
iamp		=	p4
ipulse	=	p5
irhytable	=	p6
ienv		=	p7
ienv2		=	p8
ipan		=	p9
isend		=	p10
icps		=	p11

;=====RHYTHM========
krhythm	phasor	ipulse/(ftlen (irhytable))
krhythm	table		krhythm*(ftlen(irhytable)), irhytable

;open (longer envelope)

kenvindex		phasor	ipulse
kenv			tablei	kenvindex*ftlen(ienv), ienv

kenv2			tablei	kenvindex*ftlen(ienv2), ienv2

;====create sound ========

asound	rand		1
asound	=		asound

; filter sound

icps		=		cpspch (icps)
kenv11	oscili	.333, ipulse, 111
kenv12	oscili	.333, ipulse, 112
kenv13	oscili	.333, ipulse, 113
kenv14	oscili	.333, ipulse, 114
kenv15	oscili	.333, ipulse, 115
kenv16	oscili	.333, ipulse, 116
kenv17	oscili	.333, ipulse, 117
kenv18	oscili	1, ipulse, 118




a11		oscili	kenv11,	icps, 1
a111		oscili	kenv11,	icps*.89, 1
a112		oscili	kenv11,	icps*1.12341, 1
a11		=		a11+a111+a112
a12		oscili	kenv12, 	icps*2.24, 1
a121		oscili	kenv12, 	icps*2.4*.98, 1
a122		oscili	kenv12, 	icps*2.14*1.02, 1
a12		=		a12+a121+a122
a13		oscili	kenv13, 	icps*3.3312, 1
a131		oscili	kenv13, 	icps*3.513312*.89, 1
a132		oscili	kenv13, 	icps*3.123312*1.11, 1
a13		=		a13+a131+a132


a14		oscili	kenv14, 	icps*4.89, 1
a141		oscili	kenv14, 	icps*5.89*.89, 1
a142		oscili	kenv14, 	icps*6.89*1.03, 1
a14		=		a14+a141+a142

a15 		oscili	kenv15, 	icps*5.12, 1
a151		oscili	kenv15, 	icps*5.5612*1.02, 1
a152 		oscili	kenv15, 	icps*5.7312*.998, 1
a15		=		a15+a151+a152

a16		oscili	kenv16, 	icps*6.97, 1
a161		oscili	kenv16, 	icps*6.97*1.02, 1
a162		oscili	kenv16, 	icps*6.97*.98, 1
a16		=		a16+a161+a162

a17		oscili	kenv17, 	icps*7.89, 1
a171		oscili	kenv17, 	icps*7.89*1.02, 1
a172		oscili	kenv17, 	icps*7.89*.98, 1
a17		=		a17+a171+a172

a18		oscili	kenv13, 	icps*1.25, 1
a181		oscili	kenv13, 	icps*1.125*1.004, 1
a182		oscili	kenv13, 	icps*1.134*.996, 1
a18		=		a18+a181+a182


aall		=		(a11*.2+ a12*.2+ a13*.2+ a14*.2 + a15*.2 + a16*.2 + a17*.2+a18*.1) ; + a18*.2
asound	=	aall + asound * kenv2
a2		butterbp	asound, icps, icps * .5
a3		butterbp	asound, icps*2.1,icps * .5
a4		butterbp	asound,icps*2.8, icps*.5
asound	=		a2+a3+a4


a1		comb		asound, .5, 1/icps*.5
asound	=		aall*.5  +asound*.7 + a1*.6 *kenv18

; mix
asound	=		asound*iamp*krhythm

; pan



aL		=		asound*ipan
aR		=		asound*(1-ipan)

		outs		aL, aR

gaDL		=		aL*isend + gaDL
gaDR		=		aR*isend + gaDR

endin


instr 199

itime		=		1/p4
kenv		linen		1, 0, p3, 2
aDL		=		gaDL
aDR		=		gaDR

aFL		init		0
aFR		init		0

aDL		multitap	aDL+aFL, itime*3, .7, itime*5, .5, itime*7, .3, itime*9, .2
aDR		multitap	aDR+aFR, itime*4, .7, itime*6, .5, itime*8, .3, itime*2, .4

aFL		=		aDL*.1+aDR*.2
aFR		=		aDR*.1+aDL*.2

		outs		aDL*kenv, aDR*kenv

gaDL		=		0
gaDR		=		0

endin
