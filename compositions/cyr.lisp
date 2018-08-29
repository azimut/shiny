(in-package :shiny)

;; https://github.com/efairbanks/Cybin/blob/1a482bb824910ca2d16c94e4b8b16b8a5d9f2125/main.lua#L277
;; https://musical-artifacts.com/artifacts/415

(all-piano 22)
(fp 0 7 8)
(freverb-toggle 1)
(freverb-preset 6)
(fp 0 40)

(defun ff ())
(let ((notes '(7 0 3 0)))
  (defun ff (time)
    (let ((offset (mod (* 7 (floor (/ (get-universal-time) 8))) 12))
          (note   (elt notes (mod (get-universal-time) 4))))
      (p time (+ note 60 offset) 35 1.1 0)
      (when (or (zmodt 4 0)
                (zmodt 4 2))
        (p time (+ note offset 53) 60 1 1)
;;        (p time (+ note offset 55) 60 1 1)
        )
      (when (or (zmodt 8 6)
                (zmodt 8 0))
        (p time (+ offset 36) 60 1 2)
        (p time (+ offset 48) 60 1 3)))
    (aat (+ time 1) #'ff it)))

(ff (quant 4))
