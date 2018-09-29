(in-package :shiny)

;;;
;; arpeggios.rb
;;;

;; frequency = 293.665 # Middle D
;; twelfth = 2 ** (1.0/12)
;;
;; next = frequency * twelfth
;;      = 311.127
;;
;; (293.665 311.12723 329.6278 349.22852 369.99472 391.99576 415.30502 440.00034 466.16412 493.8837 523.2515 554.3657 587.33)
;;
;; http://subsynth.sourceforge.net/midinote2freq.html
;; 62 = 293.665
;; 63 = 311.127
;; 64 = 329.627

(defun make-arpeggio (note mode)
  "makes a very peculiar kind of arpeggio
  > (make-arpeggio :D4 :major) => (62 69 74 67 69)
  Basically it create the scale midi notes and then picks
  elements from it based on the PC of the mode selected"
  (let* ((n     (ov-scale note mode))
         (nn    (append n n))
         (steps (cdr (assoc mode +scale+)))
         (sum-steps (loop :for (x y) :on steps
                       :by #'cddr
                       :collect (apply #'+ (remove nil (list x y))))))
    (loop :for x :in (append '(0) sum-steps)
       :with z = 0
       :collect (nth (incf z x) nn))))

#|
> y scale.scale_notes.map(&:frequency)
- 293.665
- 329.6278174167721
- 369.994715117378
- 391.9957457748432
- 440.00034773099077
- 493.8836915709643
- 554.3657000673392
- 587.3300000000002
(62 64 66 67 69 71 73 74)
   2  2  1  2  2  2  1

> (ov-scale :D4 :major)
(62 64 66 67 69 71 73 74)
> (scale 0 'ionian)
(0 2 4 5 7 9 11)

> y scale.i.chord.third.all_notes.map(&:frequency)
- 293.665
- 440.00034773099077
- 587.3300000000002
- 391.9957457748432
- 440.00034773099077
(62 69 74 67 69)
> y scale.ii.chord.third.all_notes.map(&:frequency)
- 329.6278174167721
- 493.8836915709643
- 659.2556348335443
- 440.00034773099077
- 493.8836915709643
(64 71 76 69 71)
|#
