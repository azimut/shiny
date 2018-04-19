(in-package :somecepl)

;;;
;; arpeggios.rb
;;;

;; frequency = 293.665 # Middle D
;; twelfth = 2 ** (1.0/12)
;;
;; next = frequency * twelfth
;;      = 311.127

;; http://subsynth.sourceforge.net/midinote2freq.html
;; 62 = 293.665
;; 63 = 311.127
;; 64 = 329.627
(defun f (time notes)
  (p time (car notes) 60 1 1)
  (aat (+ time #[1 b]) #'f it (cdr notes)))
(f (now) '(62 63 64 65 66 67 68 69 70 71 72 73 74)) 

;; (293.665 311.12723 329.6278 349.22852 369.99472 391.99576 415.30502 440.00034 466.16412 493.8837 523.2515 554.3657 587.33)
