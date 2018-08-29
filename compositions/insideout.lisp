(in-package :somecepl)

(at (tempo-sync #[8 b])
    #'eval
    (let ((s (make-cycle '(.2 1 1 1))))
      (defpattern pat1 ('("-x-x-" "x-x-" "xxxx----" "---x--------") .4)
        (bbplay "kick_OH_F_9.wav" :rate 2)
        (bbplay "snare_OH_FF_9.wav" :rate 1 :left (next s))
        (bbplay "hihatClosed_OH_F_20.wav" :rate 2)
        (bbplay "hihatOpen_OH_FF_6.wav" :rate (pick 1 2)))))

(defun pat1 ())
(pat1 (tempo-sync #[4 b]))
