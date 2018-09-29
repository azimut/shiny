(in-package :shiny)

(make-orc :synth
          :filename "synth"
          :filepath "/home/sendai/projects/csound-instruments/cook/")

(start-csound (get-orchestra :synth))
(load-csound (get-orchestra :synth))
(start-thread)
;; ;                       -----VCOs------ ------VCF------ ---VCOs-- ------LFOs------
;; ;     Strt Leng Levl    Pitch Semi Fine Vcf1  Vcf2  Rez Wav1 Wav2 Wave Rate1 Rate2 Ring
;; i1.1 0.00 -2     0.70   06.00 1.00 1.00 1000  200   10   02  02   02   0.25  0.30  0.75
(make-play synth "i1"
           :amp .7 :keynum 60 :semi 1f0 :fine 1f0
           :vcf1 1000 :vcf2 200 :rez 10 :wav1 2 :wav2 2
           :wave 2 :rate1 .25 :rate2 .3 :ring .75 :left 1f0 :right 1f0)

(play-synth (make-chord 60 90 3 (scale 0 'minor)) 3 :amp .1 :vcf1 2000 :left .6)

(play-synth-arp '(60 54) .5 1 :amp .1)
;;--------------------------------------------------

(make-orc :phaser
          :filename "stringphaser"
          :filepath "/home/sendai/projects/csound-instruments/costello/")

(start-csound (gethash :phaser *orcs*))
(start-thread)
(load-csound (get-orchestra :phaser))

;; i19 0  5      3000   6.07
; Global instrument for ensemble/phaser effect. p4 sets amount
; of pitch change in chorusing.
;; i22 0  20     .9  
; Global instrument for reverb.
;; i99 0  24     .93    1.2    1      7000 1
(make-play phaser "i19" :amp 3000 :keynum 60)
(make-play phaser-effect "i22" :change .9)
(make-play phaser-reverb "i99" :p1 .93 :p2 1.2 :p3 1 :p4 7000 :p5 1)

(play-phaser-reverb 24)
(play-phaser-effect 20 :change .1)
(play-phaser 70 1)
(play-phaser (make-chord 60 70 3 (scale 0 'minor)) 2 :amp 100)

;;--------------------------------------------------

(make-orc :wavmodfm
          :filename "wavmodfm"
          :filepath "/home/sendai/projects/csound-instruments/dodge/")

(start-csound (get-orchestra :wavmodfm))
(start-thread)

(load-csound (get-orchestra :wavmodfm))

(make-play wavmodfm "i1" :amp 3000 :freq 440)
(play-wavmodfm (make-chord 40 60 3 (scale 0 'minor)) 2 :amp (pick 2000 3000))

;;--------------------------------------------------

;; Inception-esque gong
(make-orc :gong
          :filename "gong"
          :filepath "/home/sendai/projects/csound-instruments/costello/")

(start-csound (get-orchestra :gong))
(start-thread)
(load-csound (get-orchestra :gong))

;; i99 1  25     200    5000   .999   0      100    200    10000  1.2
;; i99 6  20     150    5000   .999   0      100    300    10000  .8
;; i99 11 15     400    5000   .999   0      200    300    20000  1

(make-play gong "i99"
           :freq 200 :amp 5000 :p4 .999
           :p5 0 :p6 100 :p7 200
           :p8 10000 :p9 1.2)

(play-gong 60 20 :amp 500)

;;--------------------------------------------------
;; A set of cool eerie stuff...
(make-orc :otis
          :filename "otis"
          :filepath "/home/sendai/projects/csound-instruments/costello/")

(start-csound (get-orchestra :otis))
(load-csound (get-orchestra :otis))
(start-thread)

(make-play otis15 "i15" :freq 443.713 :p6 200 :p7 .401)
(make-play drums "i7" :amp .799 :ampfunc 3 :freq 203 :ratio 1.416 :p8 4.99 :p9 .511)
(make-play drone "i8"  :freq 60 :amp .399 :p6 9.999 :p7 3)
(make-play bell "i5" :freq 60 :amp .032 :p6 4 :p7 70.006 :p8 169.437)
(make-play timpani "i14" :freq 48.695 :amp .046 :p6 3 :p7 7 :p8 90)

;; 5 bell
;; 7 mouse drums - fm
(play-timpani (pickl (make-chord-fixed 60 3 (scale 0 'minor))) .3)
(play-timpani (pickl (ov-scale :c6 :minor)) 4 :amp .01)
(play-drone 58 10 :amp .1  :p7 1)
(play-bell-arp '(60 62 65) 2 2 :p8 20)
(play-drums 200 .05)
(play-otis15 50 8 :p7 .9)
;;--------------------------------------------------

(make-orc :physmodl
          :filename "physmodl"
          :filepath "/home/sendai/projects/csound-instruments/mikelson/")

(load-csound (get-orchestra :physmodl))
(start-csound (get-orchestra :physmodl))
(start-thread)
;; i2  128       4      1400   6.00   .25
(make-play bass "i2" :amp 1400 :keynum 60 :pluckdur .25 :left 1f0 :right 1f0)
;;   Start  Dur    Amp      Pitch   Press  Filter     Embouchure  Reed Table
;;               (20000) (8.00-9.00) (0-2) (500-1200)   (0-1)      
;;i4  32 16         6000   8.00      1.5    1000        .2     1
(make-play clarinet "i4" :amp 6000 :keynum 60 :press 1.5 :filter 1000 :emb .2 :reed 1
           :left 1f0 :right 1f0)
; Slide Flute
;  Start  Dur  Amplitude Pitch  Pressure  Breath  Feedbk1  Feedbk2
i3  80    16     6000    8.00   .9        .036   .4     .4
i3  .      4      .      8.05   .99       .      .      .    

(make-play flute "i3"
           :amp 6000 :keynum 60 :press .9 :breath .036 :fbk1 .4 :fbk2 .4
           :left 1f0 :right 1f0)

; Drumstick
;  Start  Dur  Amp  Pitch  PreFilter
i8  0     4    200    7.05   100
(make-play drumstick "i8" :amp 200 :keynum 50 :prefilter 100)
(play-drumstick 60 1)

; Drum 1
;  Start  Dur    Amp    Pitch  PreFilter  TubeLength  Feedback1  Feedback2
;;i9  0    128   160    5.11   100              4      4.4    4.3
(make-play drum "i9" :amp 160 :keynum 40 :prefilter 100 :tubelength 4
           :feedback1 4.4 :feedback 4.3 :left .4 :right .4)

(play-flute 62 1 :left .2 :press .9 :fbk1 .5)
(play-drum 60 10)
(play-clarinet 70 1 :left .2)
(play-bass 30 1 :pluckdur .3 :right .02  :amp 200)

;;--------------------------------------------------

(make-orc :hallown
          :filename "hallown"
          :filepath "/home/sendai/projects/csound-instruments/mikelson/")
(start-csound (get-orchestra :hallown))
(load-csound (get-orchestra :hallown))
(start-thread)

; Ghosties
;  Start  Dur  Amp  Frqc 
;;i11 44 6      5      7.00 
;;i12 48 3      7      8.00 
(make-play ghost1 "i11" :amp 5 :keynum 60)
(make-play ghost2 "i12" :amp 5 :keynum 60)
(play-ghost2 100 3)

; Electronic Zap from the Mandelbrot set.
;    Sta  Dur  Amp  Pitch XCorner  YCorner  XStep   YStep  MaxLoop  Pan
;; i10 70 1.0    60     7.00   -0.6   -0.8   .0002  .002   400    1.0
;; (make-play zap "i10" :amp 60 :keynum 60 :xcorner -.6 :ycorner -.8
;;            :xtep .0002 :ystep .002 :maxloop 400 :pan 1f0)
;; (play-zap 80 1)

; Wolf Howl
;    Sta  Dur  Amp  FqcTab  FrmTab  Pan  Fco
;; i2  12 4      10     6      20     1      1000 
;; i2  13 2      25     8      20     .2     1500
;; (make-play wolf "i2" :amp 10 :fqctab 6 :frmtab 20 :pan 1f0 :fco 1000)
;; (play-wolf 2 :fqctab 6 :pan .2)

; Thunder
;    Sta   Dur  Amp   Fqc  Fco  Atk  Pan
;; i3  14 4      6000   100    100    40     .5  
;; i3  17 4      6800   80     200    10     .7
;; (make-play thunder "i3" :amp 600 :fqc 100 :fco 100 :atk 40 :pan .5)
;; (play-thunder 2)

; Church Bell
;    Sta  Dur  Amp    Fqc   Pan  OutCh1  OutCh2
i6  42 2.0    1000   8.02   1.0    3      4    

(make-play bell "i6" :amp 1000 :keynum 60 :pan 1f0 :outch1 3 :outch2 4)
(play-bell 60 1 :pan .8 :amp 3000)

; Organ Intro to Bach's Toccata & Fugue in Dminor
;   Sta   Dur   Amp    Fqc   Pan  OutCh1  OutCh2
i4  26 .12    200    7.09   .9     1      2    
i4  +  .1     300    7.07   .8     1      2    

(make-play organ "i4" :amp 200 :keynum 60 :pan .5 :outch1 1 :outch2 2)
(play-organ 55 1)

;;--------------------------------------------------

(make-orc :bass-drum
          :filename "bass-drum"
          :filepath "/home/sendai/projects/csound-instruments/varios/")
(load-csound (get-orchestra :bass-drum))
(make-play bass-drum "i1" :amp 3000)
(start-csound (get-orchestra :bass-drum))
(start-thread)
(play-bass-drum .25 :amp 1000)

;;--------------------------------------------------

(make-orc :ourvoice
          :filename "ourvoice"
          :filepath "/home/sendai/projects/csound-instruments/pinkston/")

(load-csound (get-orchestra :ourvoice))

(make-play voice-a "i1"
           :amp 10000 :keynum 60 :fmt1 609 :db1 0 :fmt2 1000 :db2 -6
           :fmt3 2450 :db3 -12 :fmt4 2700 :db4 -11 :fmt5 3240 :db5 -24
           :left 1f0 :right 1f0)
(make-play voice-e "i1"
           :amp 10000 :keynum 60 :fmt1 400 :db1 0 :fmt2 1700 :db2 -9
           :fmt3 2300 :db3 -8 :fmt4 2900 :db4 -11 :fmt5 3400 :db5 -19
           :left 1f0 :right 1f0)
(make-play voice-iy "i1"
           :amp 10000 :keynum 60 :fmt1 238 :db1 0 :fmt2 1741 :db2 -20
           :fmt3 2450 :db3 -16 :fmt4 2900 :db4 -20 :fmt5 4000 :db5 -32
           :left 1f0 :right 1f0)
(make-play voice-o "i1"
           :amp 10000 :keynum 60 :fmt1 325 :db1 0 :fmt2 700 :db2 -12
           :fmt3 2550 :db3 -26 :fmt4 2850 :db4 -22 :fmt5 3100 :db5 -28
           :left 1f0 :right 1f0)
(make-play voice-oo "i1"
           :amp 10000 :keynum 60 :fmt1 360 :db1 0 :fmt2 750 :db2 -12
           :fmt3 2400 :db3 -29 :fmt4 2675 :db4 -26 :fmt5 2950 :db5 -35
           :left 1f0 :right 1f0)


