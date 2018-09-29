(in-package :shiny)
(start-csound (gethash :xanadu *orcs*))

(bt:make-thread
 (lambda ()
   (loop
      :for frame = (csound:csoundperformksmps *c*)
      :while (= frame 0))))

;; dodgefig327.orc - meh - drop of things, weird electic drums, no keynum
;; winkler/pwm2
;; varios/bass-drum (blu1e-monday like)
;; costello/stringphaser (synth strings intro like)
;; cook/808hithat ()
;; cook/synth (old electronic music, demo crack style)
;; cook/sequencer
;; cook/buzz

(make-orc :string
          :filename "string"
          :filepath "/home/sendai/projects/csound-instruments/dodge/")

(make-orc :dodsoprano
          :filename "dodsoprano"
          :filepath "/home/sendai/projects/csound-instruments/dodge/")

(make-orc :soprano
          :filename "soprano"
          :filepath "/home/sendai/projects/csound-instruments/cook/")

(make-orc :soprano
          :filename "soprano"
          :filepath "/home/sendai/projects/csound-instruments/pinkston/")

(make-orc :fmdrum
          :filename "fmdrum"
          :filepath "/home/sendai/projects/csound-instruments/pinkston/")

(csound:csoundcompileorc *c* (get-orc :fmdrum))
(csound:csoundreadscore  *c* (get-sco :fmdrum))

; play some notes
; p4 = amp; p5 = fund; p6 = 0 < ampfac < 1; p7 = fmt env func
; p8 = port func; p9 = fmt hz func; p10 = fmt amp func; p11, 12 = i1,i2 fns 
i01 0  1      20000  7.07   .5     2      3      4      5      6      7

(make-play dod "i1" :amp 20000 :keynum 60 :p6 .5 :p7 2 :p8 3 :p9 4 :p10 5 :p11  6 :p12  7)
(play-dod (pickl (ov-scale :C4 :minor)) 1 :p10 3)

;     Strt  Leng  Levl  Pitch Vowl1 Vowl2 Delay Glide
i01 0.00      -0.50  1.00   09.00  2.00   1.00   2.00   0.25
(csound:csoundreadscore *c* "i01 0.00      -0.50  1.00   09.00  2.00   1.00   2.00   0.25")
(make-play soprano "i1" :lvl 1f0 :keynum 60 :vowl1 2f0 :vowl2 1f0 :delay 2f0 :glide .25)
(play-soprano 60 1)

; play some notes
; p4 = amp; p5 = fund; p6 = 0 < ampfac < 1; p7 = fmt env func
; p8 = port func; p9 = fmt hz func; p10 = fmt amp func; p11, 12 = i1,i2 fns
i01 0  1      30000  7.07   .7     2      3      4      5      6      7    
i01 1  .      .      8.04 

(make-play soprano "i1" :amp 20000 :keynum 60 :ampfac .7
           :env 2 :port 3 :fmthz 4 :fmtamp 5 :i1 6 :i2 7)

(play-soprano (pickl (ov-scale :C4 :minor)) 2  :fmthz 1 :i1 3)


;nn st	dur	igain	irvt	ilpt-or-pch	iscale
i01 0  10      .7     .2     5.00   .4

(make-play drum "i1" :igain .7 :irvt .2 :keynum 60 :iscale 4)
(play-drum 50 1)
;parms: st      dur     amp     pch     rise    decay   lfohz   lfofn   oscfn
i1  0  3      16000             13.00  1      2      15     2      1    
i1  +  .      .      .      .      .      20   
i1  +  .      .      .      .      .      25   
f0  10
s
;parms:	st	dur	amp	pch	rise	decay	lfohz	amfn	oscfn
i2  0  3      16000  13.00  1      2      15     2      1    
;	lfohzfn	panfn
                        3       4     
i2  +  .      .      .      .      .      20   
i2  +  .      .      .      .      .      25   
f0  10
s
;parms: st      dur     amp     pch     rise    decay   lfohz   amfn    oscfn
i3  0  3      16000  12.06  1      2      15     2      1    
;	lfohzfn	panfn	ibend	bendfn
                        3       4       .25     4   
i3  +  .      .      .      .      .      20   
i3  +  .      .      .      .      .      25   

(make-play tick1 "i1" :amp 10000 :keynum 60 :rise 1 :Decay 2 :lfohz 15 :lfofn 2 :oscfn 2)
(play-tick1 60 1)
;Test a range of pitches, bend amounts, transient durations, and indexes.
;   st	dur  ipkdb	ipch	carfac	modfac	    maxbnd	trdur	endbnd	indx1	indx2	isusdb
i01 0  .75    90        7.00   1        .31791         1         .2     0        7      .005   82

(make-play drum "i1" :ipkdb 90 :keynum 60 :carfac 1 :modfac .31791 :maxbnd 1 :trdur .2 :endbnd 0 :indx1 7 :indx2 .005 :isusdb 82)
(play-drum (pickl (ov-scale :C3 :minor)) 1 :trdur 2 :modfac .8 :carfac .2)

;;--------------------------------------------------

(load-csound
 (merge-orcs
  (get-orchestra :phaser)
  (get-orchestra :physmodl '(0 1))))

(make-play phaser "i1" :amp 3000 :keynum 60)
(make-play phaser-effect "i2" :change .9)
(make-play phaser-reverb "i3" :p1 .93 :p2 1.2 :p3 1 :p4 7000 :p5 1)
(make-play bass "i4" :amp 1400 :keynum 60 :pluckdur .25 :left 1f0 :right 1f0)
(make-play flute "i5"
           :amp 6000 :keynum 60 :press .9 :breath .036 :fbk1 .4 :fbk2 .4
           :left 1f0 :right 1f0)

(start-thread)
(play-phaser-effect 10)
(play-phaser 60 1)
(play-flute 60 1)
(play-bass-arp (make-chord 30 40 3 (scale 0 'minor)) .5 .5)
