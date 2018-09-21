(in-package :shiny)

(load-csound (get-orchestra :bass))

(make-play bass "i1"
           :fco1 .1  :fco2 .3
           :res1 .2  :res2 .2
           :env1 .1  :env2 .4
           :dec1 .05 :dec2 .8
           :acc1 0   :acc2 0
           :bars 120
           :amp .2
           :keynum 60)

(play-bass 50 1 :bars 120 :amp .1 :res1 .9 :res2 .2)
(play-bass 60 1 :bars 1)

;;--------------------------------------------------

(load-csound (merge-orcs (get-orchestra :bass) (get-orchestra :asynth)))
;; https://github.com/AbletonTriCitiesUserGroup/Gallery/
;;                    -----VCOs------ ------VCF------ ---VCOs-- ------LFOs------
;;     Strt Leng Levl Pitch Semi Fine Vcf1  Vcf2  Rez Wav1 Wav2 Wave Rate1 Rate2 Ring
;;i1.1  0    -2   .7   06.00 1    1    1000  200   10  2    2    2   .25   .3     .75
(make-play asynth "i2" :amp .7 :keynum 60 :semi 1 :fine 1 :vcf1 1000 :vcf2 200 :rez 10 :wav1 2 :wav2 2 :wave 7 :rate1 .25 :rate2 .3 :ring .75)
(play-asynth 60 .5 :amp .2)
(let ((note (make-cycle (make-chord-fixed 66 3 (scale 0 'ryukyu)))))
  (defun f (time)
    (if (odds .8) (play-hihat 1) (play-snare 1))
    (if (odds .8)
        (play-bass (next note) 1 :amp .2 :bars 300)
        (play-bass (+ 12 (next note)) 2 :amp .2 :bars 1))
    (aat (+ time #[1 b]) #'f it)))
(f (now))
(defun f ())

;;--------------------------------------------------
(start-csound (get-orchestra :drumkit))
(start-thread)

(make-play bass "i1"
           :amp 10000  :wave 1
           :pulse 8 :env 10 :rhyth 30 :keynum 60 :gliss 20 :p1 .1)

;;in star dur amp   pulse rhythm env1 env2 pan
;;i2   0   16  20000 8     32     12   11   .7 .2

(make-play hihat "i2"
           :amp 10000 :pulse 8 :rhy 32
           :env1 12 :env2 11 :pan1 .7 :pan2 .2)

;;SNARE
;; ;in star dur amp   pulse env1 env2 rhythm pan
;; i3  0    16	30000 8     13   14   33     .3 .2
(make-play snare "i3"
           :amp 10000 :pulse 8
           :env1 13 :env2 14 :rhy 33 :pan1 .3 :pan2 .2)
;;CRASH
;;in star dur amp  pulse rhythm env1 env2 pan
;;i4 0    32  2500 .25   34     12   15   .2 .2 13.03
(make-play crash "i4" :amp 2500 :pulse .25 :rhy 34 :env1 12 :env2 15
           :pan1 .2 :pan2 .2 :keynum 80)

(play-hihat 1)
(play-bass 60 1)
(play-snare 2)
(play-crash 60 2 :pan1 .9)
