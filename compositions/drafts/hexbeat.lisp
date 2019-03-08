(in-package :shiny)

(fp 2 40)
(fp 0 20)
(fg 2f0)

(at (tempo-sync #[(* .2 16) b])
    #'eval
    (defpattern h (((parse1 (hexbeat "9108"))
                    (parse1 (hexbeat "4884"))
                    (parse1 (hexbeat "2442"))
                    (parse1 (hexbeat "1221")))
                   .5)
      (p time 40 40 .1 0)
      (p time 52 40 .1 0)
      (p time 60 40 .1 0)
      (p time (pc-quantize (drunk 70 5) (scale 0 'minor)) 40 .1 0)))

(aat (tempo-sync #[ (* 16 .2) b]) #'h it)
(defun h ())

(defun f (time)
  (when (hbeat time "9108")
    (p time 40 30 .1 0))
  (when (hbeat time "4884")
    (p time 52 40 .1 0))
  (when (hbeat time "2442") (p time 60 40 .1 0))
  (when (hbeat time "1221")
    (p time (pc-quantize (drunk 70 5) (scale 0 'minor)) 50 .1 0))
  (aat (+ time #[1 b]) #'f it))

(setf (bpm *tempo*) 200)
(aat (tempo-sync #[1 b]) #'f it)
(defun f ())
