(in-package :shiny)

;; http://earslap.com/page/otomata.html
;; https://pastebin.com/dPWLZDBs
;;
;; Each alive cell has 4 states: Up, right, down, left. at each cycle,
;; the cells move themselves in the direction of their internal
;; states. If any cell encounters a wall, it triggers a pitched sound
;; whose frequency is determined by the xy position of collision, and
;; the cell reverses its direction. If a cell encounters another cell
;; on its way, it turns itself clockwise.
;;
;; - 9x9 cells
;; - Tempo
;; - Scale: akebono, yue-diao?, bayati, dorian, harmonic minor, hijaz, hijaz kar, huzam, ionian, kokin choshi, kourd-atar, lydian, yu diao, neveseri, niavent, nirz rast, gong-diao, zhi-diao, purvi, pygmy, rast, rumanikos, sabah, segiah, sho, blues, goonkali, iwato, kumoi, locrian, magen abot, melog, mixolydian, noh, phrygian, pyeong jo, shang-diao, zokuso,
;;
;; NOTES: It does NOT MATTER if hits up or down on a column, same note is play.


;;--------------------------------------------------
;; Tonematrix
;; https://github.com/Babkock/ToneMatrix
;; Is a 16-step drum machine. The Y (vertical) axis
;; represents eight different sounds, and more than one can be
;; played at a time. The X (horizontal) axis represents one
;; measure in sixteenth-notes.

;;(fluidsynth:sfload *synth* "/home/sendai/Downloads/sf2/CTK-230_SoundFont.sf2" 1)
(bbuffer-load "/home/sendai/Downloads/Silent Hill/Ambience/Silent Hill Homecoming/Dreams of Leaving/Train Cars.wav")
(freverb-toggle 1)
(fluidsynth:set-reverb *synth* .6d0 .1d0 .9d0 25d0)
(all-piano 22)

(all-piano 33)

(defpattern k (("--------x-------"
                "----x-----------"
                "----------x-----"
                "---x---x--------"
                "-------------x--"
                "----------------"
                "-----------x----"
                "------x--------x")
               .5)
  (progn
    (let ((*clef* "g")
          (*window-name* "lead"))
      (p time (pickl (ov-scale :C4 :minor)) 50 4 11))
    (p time (+ -12 60) (rcosr 60 5 5) (sinr 2 1 4) (random 9)))
  (progn
    (let ((*clef* "g")
          (*window-name* "lead2"))
      (p time (pickl (ov-scale :C5 :minor)) 50 (pick 3 4) 12))
    (p time (+ -12 62) (rcosr 60 5 4) (sinr 2 1 5) (random 9)))
  (progn
    (bbplay "Train Cars.wav" :id 2 :amp .2 :rate .8 :downsamp 8 :left (random 1f0) :right (random 1f0))
    (p time (+ -12 63) (rcosr 60 5 3) (sinr 2 1 3) (random 9)))
  (p time (+ -12 65) (rcosr 60 5 6) (sinr 2 1 2) (random 9))
  (p time (+ -12 67) (rcosr 60 5 7) (sinr 2 1 5) (random 9))
  (p time (+ -12 68) (rcosr 60 5 1) (sinr 2 1 6) (random 9))
  (p time (+ -12 70) (rcosr 60 5 5) (sinr 2 1 8) (random 9))
  (p time (+ -12 72) (rcosr 60 5 3) (sinr 2 1 3) (random 9)))

(let ((level (make-palindrome (iota 127))))
  (defun panover (time)
    (dotimes (channel 10)
      (fluidsynth:cc *synth* channel 10 (next level)))
    (aat (+ time #[.5 b]) #'panover it)))


(defun panover ())
(panover (tempo-sync #[(* .5 16) b]))

(inscore-reset)
(inscore-init)
(inscore-reset "lead" t)

(k (tempo-sync #[.5 b]))
(defun k ())

