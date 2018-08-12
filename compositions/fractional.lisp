(in-package :somecepl)

;; https://github.com/laurasirco/The-Algorithmic-Composer-MAC/blob/master/src/FractionalNoiseComposer.cpp

;; From what I see the algorithm works by:
;; - Creating a list/array with all the notes from a scale across
;;   several octaves.
;; - Elements from that array then are pick by a our random function using
;;   the array index value.
;; The random function has a concept of a "seed" that is self-feed across
;; calls. Making it less random. I guess.

;; compose()
(defparameter max-pitch 96)
(defparameter min-pitch 36)

(defparameter *seed* nil)

(defparameter *pitches-noises* nil)

;; set-pitches-based-on-scale-and-octaves()
;;(defparameter *scale* 1)
(defparameter *pitches* nil)
(defparameter *pitches-array* nil)

;; (defvar min-octave 1)
;; (defvar max-octave 6)

;; (defparameter *scale-chromatic*
;;   (make-array 12 :initial-contents (scale 0 'chromatic)))
;; (defparameter *scale-pentatonic*
;;   (make-array 5 :initial-contents (scale 0 'pentatonic)))

;; (defparameter *myscales*
;;   (make-array 2 :initial-contents
;;               (list *scale-chromatic*
;;                     *scale-pentatonic*)))

;; ------------------------
;; Set pitches from scale
;; ------------------------

(defun set-pitches-based-on-scale-and-octaves (pc min-octave max-octave)
  (let ((max (length pc)))
    (setf *pitches* nil
          *pitches-array* nil)
    (loop :for octave :from min-octave :upto max-octave :do
       (loop :for tone :below max :do
          (let ((pitch (+ (* 10 (+ octave 2))
                          (+ 4  (* octave 2))
                          (nth tone pc))))
            (push pitch *pitches*))))
    (setf *pitches-array*
          (make-array (length *pitches*) :initial-contents *pitches*))))

;; (defun set-pitches-based-on-scale-and-octaves ()
;;   (let ((max 7))
;;     (cond ((equal *scale* 0) (setf max 12))
;;           ((equal *scale* 1) (setf max 5)))
;;     (setf *pitches* nil
;;           *pitches-array* nil)
;;     (loop :for octave :from min-octave :upto max-octave :do
;;        (loop :for tone :below max :do
;;           (let ((pitch (+ (* 10 (+ octave 2))
;;                           (+ 4  (* octave 2))
;;                           (aref (aref *myscales* *scale*) tone))))
;;             (push pitch *pitches*))))
;;     (setf *pitches-array*
;;           (make-array (length *pitches*) :initial-contents *pitches*))))

;; ----------------------
;; Mapping functions
;; ----------------------

(defun map-interpolate (x in-min in-max out-min out-max)
  (let ((returnvalue (+ out-min
                        (/ (* (- x in-min)
                              (- out-max out-min))
                           (- in-max in-min)))))
    returnvalue))

(defun map-value (value min max)
  (let ((returnvalue (round (+ min (* value (- max min))))))
    returnvalue))

(defparameter *dtype* #(d-whole whole d-half half
                        d-quarter quarter d-eighth eighth
                        d-sixteenth sixteenth
                        d-thirty-second thirdty-second
                        sixty-fourth))

(defun type-to-duration (tt)
  (let ((d 0))
    (case tt
      ((d-whole)   (setf d (+ 1.0 0.5 )))
      ((whole)     (setf d 1.0))
      ((d-half)    (setf d (+ 0.5 (/ 0.5 2))))
      ((half)      (setf d 0.5))
      ((d-quarter) (setf d (+ 0.25 (/ 0.25 2))))
      ((quarter)   (setf d 0.25))
      ((d-eighth)  (setf d (+ 0.125 (/ 0.125 2))))
      ((eighth)    (setf d 0.125))
      ((d-sixteenth) (setf d (+ 0.0625 (/ 0.0625 2))))
      ((sixteenth)   (setf d 0.0625))
      ((d-thirty-second) (setf d (+ 0.03125 (/ 0.03125 2))))
      ((thirty-second) (setf d 0.03125))
      ((sixty-fourth)  (setf d 0.015625)))
    (* d 4)))


;; ----------------------
;; Random functions
;; ----------------------

(defun r8-uniform-01 ()
  "returns a unit pseudorandom R8."
  (let* ((i4-huge 2147483647)
         (seed    *seed*)
         (k       (round (/ seed 127773)))
         (seed    (- (* 16807 (- seed (* k 127773)))
                     (* k 2836)))
         (seed    (if (< seed 0) (+ seed i4-huge) seed))
         (r       (* seed 4.656612875 (expt 10 -10))))
    (setf *seed* seed)
    r))

;; Simulating a "constant" variable
(let ((used 0)
      (seed1 0) (seed2 0) (seed3 0)
      (v2 0.0))
  (defun r8-normal-01 ()
    "returns a unit pseudonormal R8

     The standard normal probability distribution function (PDF) has
     mean 0 and standard deviation 1."
    (let ((v1 0)
          (r1 0)
          (r2 0))
      ;; If USED is odd but the input SEED does not match
      ;; the output SEED on the previous call, then the user has changed
      ;; the seed. Wipe out internal memory.
      (when (and (equal (mod used 2) 1)
                 (not (equal *seed* seed2)))
        (setf used 0
              seed1 0 seed2 0 seed3 0
              v2 0.0))
      (if (equal (mod used 2) 0)
          (progn
            (setf seed1 *seed*
                  r1    (r8-uniform-01)
                  seed2 *seed*
                  r2    (r8-uniform-01)
                  seed3 *seed*
                  *seed* seed2
                  v1 (* (sqrt (* -2.0 (log r1)))
                        (cos (* 2.0 pi r2)))
                  v2 (* (sqrt (* -2.0 (log r1)))
                        (sin (* 2.0 pi r2)))))
          (progn
            (setf v1 v2
                  *seed* seed3)))
      (incf used)
      v1)))

(defun r8vec-sftb (n azero a b)
  "computes a \"slow\" backward Fourier transform of real data"
  (let ((r  (make-array n :initial-element azero)))
    (loop :for i :from 0 :below n :do
       (loop :for k :from 0 :below (/ n 2) :do
          (let ((theta (/ (* (* (1+ k) i 2) pi)
                          n)))
            (incf (aref r i)
                  (+ (* (aref a k) (cos theta))
                     (* (aref b k) (sin theta)))))))
    r))

(defun r8vec-sftf (n r)
  "computes a \"slow\" forward Fourier transform of real data."
  (let ((azero 0.0)
        (a (make-array
            (/ n 2) :initial-element 0.0))
        (b (make-array
            (/ n 2) :initial-element 0.0)))
    (loop :for i :below n :do
       (incf azero (aref r i)))
    (divf azero n)
    (loop :for i :below (/ n 2) :do
       (loop :for j :below n :do
          (let ((theta (/ (* (* 2 j (1+ i)) pi)
                          n)))
            (incf (aref a i)
                  (* (aref r j) (cos theta)))
            (incf (aref b i)
                  (* (aref r j) (sin theta)))))
       (divf (aref a i) n)
       (divf (aref b i) n)
       (when (not (equal i (1- (/ n 2))))
         (mulf (aref a i) 2.0)
         (mulf (aref b i) 2.0)))
    (values azero a b)))

(defun f-alpha (n q-d alpha)
  "generates a 1/F^ALPHA noise sequence."
  (let ((q-d  (sqrt q-d))
        (hfa  (make-array (* 2 n) :initial-element 0.0))
        (wfa  (make-array (* 2 n) :initial-element 0.0))
        (h-a) (h-b) (w-a) (w-b) (h-azero) (w-azero)
        (wr) (wi)
        (x1 (make-array n)) (x2 (make-array n))
        (ii (1- n)))
    ;; Generate the coefficients Hk
    (setf (aref hfa 0) 1.0)
    (loop :for i :from 1 :below n :do
       (setf (aref hfa i) (/ (* (aref hfa (1- i))
                                (+ (* 0.5 alpha)
                                   (1- i)))
                             i)))
    ;; Fill Wk with white noise
    (loop :for i :from 0 :below n :do
       (setf (aref wfa i)
             (* q-d (r8-normal-01))))
    ;; Perform the discrete FT of Hk and Wk
    (multiple-value-bind (azero a b) (r8vec-sftf (* 2 n) hfa)
      (setf h-azero azero h-a a h-b b))
    (multiple-value-bind (azero a b) (r8vec-sftf (* 2 n) wfa)
      (setf w-azero azero w-a a w-b b))
    ;; Multiple the two complex vectors
    (mulf w-azero h-azero)
    (loop :for i :from 0 :below n :do
       (setf wr (aref w-a i)
             wi (aref w-b i))
       (setf (aref w-a i) (- (* wr (aref h-a i))
                             (* wi (aref h-b i)))
             (aref w-b i) (+ (* wi (aref h-a i))
                             (* wr (aref h-b i)))))
    ;; This scaling is introduced only to match the behavior
    ;; of the Numerical recipes code...
    (mulf w-azero (* 2 n))
    (loop :for i :from 0 :below (1- n) :do
       (mulf (aref w-a i) n)
       (mulf (aref w-b i) n))
    (mulf (aref w-a ii) (* 2 n))
    (mulf (aref w-b ii) (* 2 n))
    ;; Take the inverse FT of the result
    (setf x2 (r8vec-sftb n w-azero w-a w-b))
    (loop :for i :from 0 :below n :do
       (setf (aref x1 i) (aref x2 i)))
    x1))

;; ----------------------
;; Use random functions
;; ----------------------

(defun get-fractional-noise-sequence (size)
  (setf *seed* (get-universal-time))
  (let* ((x    (f-alpha size 1.0 1.0))
         (output (make-array size))
         (max 0.0)
         (min 0.0))
    (loop :for i :from 0 :below size :do
       (when (< (aref x i) min)
         (setf min (aref x i)))
       (when (> (aref x i) max)
         (setf max (aref x i))))
    ;; map the values
    (loop :for i :from 0 :below size :do
       (setf (aref output i)
             (map-interpolate (aref x i) min max 0.0 1.0 )))
    output))

;; (defun compose ()
;;   (let ((l-pitches (length *pitches*))
;;         (position 0)
;;         (pitch 0)
;;         (mypitches '()))
;;     (set-pitches-based-on-scale-and-octaves)
;;     (setf *pitches-noises* (get-fractional-noise-sequence 300))
;;     (loop :for i :from 0 :below l-pitches :do
;;        (setf position (map-value (aref *pitches-noises* i)
;;                                  0 (1- l-pitches))
;;              pitch    (aref *pitches-array* position))
;;        (when (< pitch min-pitch)
;;          (setf pitch min-pitch))
;;        (when (> pitch max-pitch)
;;          (setf pitch max-pitch))
;;        (push pitch mypitches))
;;     mypitches))

(defun compose (pc min-octave max-octave)
  (let ((l-pitches (length *pitches*))
        (position 0)
        (pitch 0)
        (duration 0)
        (mytype)
        (mypitches '())
        (mydurations '()))
    (set-pitches-based-on-scale-and-octaves pc min-octave max-octave)
    (setf *pitches-noises* (get-fractional-noise-sequence 300))
    (loop :for i :from 0 :below l-pitches :do
       (setf position (map-value (aref *pitches-noises* i)
                                 0 (1- l-pitches))
             pitch    (aref *pitches-array* position)
             mytype   (aref *dtype*
                            (map-value (aref *pitches-noises* i)
                                       0 12))
             duration (type-to-duration mytype))
       (when (< pitch min-pitch)
         (setf pitch min-pitch))
       (when (> pitch max-pitch)
         (setf pitch max-pitch))
       (push pitch mypitches)
       (push duration mydurations))
    (values mypitches mydurations)))

#|
(fluidsynth:program-change *synth* 1 60)
(setf (fluidsynth:setting *fluid-settings* "synth.gain") .5)

;; 46 harp
(all-piano 46)

(defun playthis (time notes)
  (unless notes
    (setf notes (compose)))
  (p time (car notes) 50 1 (random 4))
;;  (aat (+ time .5) #'playthis it (cdr notes))
  )

(playthis (now) (compose))

(pa (quant 4) '(76 62 64 55 62 62 69 50 36 48 57 60 50 38 62 62 52 60 69) .5 50 1 .5)

(defun playthis (time notes dur)
  (p time (car notes) 60 (car dur) 1)
  (aat (+ time (car dur))
       #'playthis it (cdr notes) (cdr dur)))

(multiple-value-bind (notes duration)
    (compose) (playthis (now) notes duration))

(flush-pending)

|#
