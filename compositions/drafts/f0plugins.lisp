(in-package :sc)

(setf *s* (sc:make-external-server "localhost" :port 4444))
(sc:server-boot *s*)

(sc:server-quit *s*)

;; NOTE: different sounds have different tunnings
;; bass=pitfall square=~lead saw!=.*
(defparameter atari-sounds
  '((saw . 1) (engine . 3) (square . 4) (bass . 6)
    (pitfall . 7) (noise . 8) (lead . 12) (buzz . 15)))

;; kick  - buzz(15)  - 30
;; hat   - noise(8) - 0
;; snare - noise(8) - 8
;;  "    - buzz (15) - 6

;; https://www.youtube.com/watch?v=q59427J96wU
;; http://hiphoptranscriptions.com/page/2
;; "512" = 2 channel - 16 timbres - 32 notes
;;       que en realidad son 
;; audvX - volume register - 4bit
;; audcX - control register - 4bit - timbre
;; audfX - frequency divider - 5bit - 32 freqs per timbre
;; AUDC4/5 - AUDC12/13

(defugen (atari2600 "Atari2600")
    (&optional (audc0 1) (audc1 2)
               (audf0 3) (audf1 4)
               (audv0 5) (audv1 5) (rate 1))
  ((:ar (multinew new 'pure-ugen audc0 audc1 audf0 audf1 audv0 audv1 rate))))

(defsynth atari ((out 0) (gate 1)
                 (tone0 5) (tone1 8)
                 (freq0 10) (freq1 20)
                 (vol0 1) (vol1 0)
                 (rate 1)
                 (amp 1) (pan 0)
                 (dur 5))
  (let* ((e (env-gen.kr (linen .002 .2 dur) :gate gate :act :free))
         (z (* amp (atari2600.ar tone0 tone1 freq0 freq1 vol0 vol1 rate))))
    (out.ar out (pan2.ar (* z e) pan))))

(in-package :shiny)

(defparameter *synth* (synth 'atari :tone0 15 :freq0 30 :vol1 0 :vol0 1))
(free *synth*)

(defun ff ())
(defun ff (time)
  (synth 'atari :tone0 15 :freq0 6
         :vol1 0 :vol0 1 :dur .5)
  (callback (+ time 1) #'ff (+ time 1)))

(ff (quant 4))

(defpattern pat1 ((get-pattern 'gogo) .25)
  (synth 'atari :dur (/ d 50) :tone0 15 :freq0 30 :rate 2 :amp .2)
  (synth 'atari :dur (/ d 50) :tone0 8 :freq0 8 :rate 1 :amp .2)
  (synth 'atari :dur (/ d 55) :tone0 8 :freq0 0 :rate 20 :amp .1))

(defun pat1 ())

(defbeat kick .3 (bjorklund-s 5 13)
  (synth 'atari :dur (/ d 2) :tone0 15 :freq0 30 :rate 2))

(defbeat kick2 .3 (bjorklund-s 4 4)
  (synth 'atari :dur (/ d 1) :tone0 15 :freq0 30 :rate 1))

(defbeat kick3 .3 (bjorklund-s 3 8)
  (synth 'atari :dur (/ d 4) :tone0 15 :freq0 30 :rate 1))

;;--------------------------------------------------

(in-package :sc)

(defugen (nes2 "Nes2")
    (&optional (trig 0.)
               (a0 0.) (a1 0.) (a2 0.) (a3 0.)
               (b0 0.) (b1 0.) (b2 0.) (b3 0.)
               (c0 0.)         (c2 0.) (c3 0.)
               (d0 0.)         (d2 0.) (d3 0.)
               (e0 0.) (e1 0.) (e2 0.) (e3 0.)
               (smask 0.))
  ((:ar (multinew new 'pure-ugen trig a0 a1 a2 a3 b0 b1 b2 b3 c0 c2 c3 d0 d2 d3 e0 e1 e2 e3 smask))))

(defsynth nes2-square ((trig 0) (dutycycle 0) (loopenv 0) (envdecay 0)
                      (vol 10) (sweep 0) (sweeplen 0) (sweepdir 0)
                      (sweepshi 0) (freq 100) (vbl 0))
  (let* ((a0 (* (clip.kr (round dutycycle 1) 0 3) 64))
         (a0 (logior a0 (* (clip.kr (round loopenv 1) 0 1) 32)))
         (a0 (logior a0 (* (clip.kr (round envdecay 1) 0 1) 16)))
         (a0 (logior a0 (clip.kr (round vol 1) 0 15)))
         (a1 (* (clip.kr (round sweep 1) 0 1) 128))
         (a1 (logior a1 (* (clip.kr (round sweeplen 1) 0 7) 16)))
         (a1 (logior a1 (* (clip.kr (round sweepdir 1) 0 1) 8)))
         (a1 (logior a1 (clip.kr (round sweepshi 1) 0 7)))
         (a2 (mod (round (max 0 freq) 1) 256))
         (a3 (clip.kr (floor (/ freq 256)) 0 7))
         (a3 (logior a3 (* (clip.kr (round vbl 1) 0 31) 8))))
    (out.ar 0 (nes2.ar trig a0 a1 a2 a3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1))))

(in-package :shiny)

