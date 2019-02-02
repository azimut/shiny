(in-package :shiny)
(fg 2f0)

(bbuffer-load "/home/sendai/projects/FoxDot/FoxDot/snd/x/lower/0_kick_drum.wav" (alexandria:symbolicate "x"))
(bbuffer-load "/home/sendai/projects/FoxDot/FoxDot/snd/_/hyphen/0_hihat_closed.wav" (alexandria:symbolicate "-"))
(bbuffer-load "/home/sendai/projects/FoxDot/FoxDot/snd/d/lower/0_wood.wav" (alexandria:symbolicate "d"))
(bbuffer-load "/home/sendai/projects/FoxDot/FoxDot/snd/v/lower/0_low_bass.wav" (alexandria:symbolicate "v"))
(clean-buffers)

(setf (bpm *tempo*) 160d0)

(let ((pat (fx-pat "(x-)-{-x}{x-}d---")))
  (defun f (time)
    (bbplay (next pat) :amp .5)
    (aat (+ time #[1 b]) #'f it)))

(aat (tempo-sync #[4 b]) #'f it)
(defun f ())
