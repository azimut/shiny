(in-package :shiny)

;; Adding filters to output from fluidsynth.
;;----------------------------------------
;; Due the output is single-float I cannot apply a filter directly
;; So i dump the channels to buses. Then use a second dsp to filter.
;; Trying to implement:
;; https://sourceforge.net/p/incudine/mailman/message/36266377/

(define-vug input-bus ((channel fixnum))
  (bus
   (the fixnum
        (+ (the fixnum
                (* current-frame
                   2))
               channel))))

(dsp! fluid-in ((synth fluidsynth:synth))
  (with ((len   (block-size))
         (left  (make-f32-array len))
         (right (make-f32-array len)))
    (fluidsynth:write-float synth len left 0 1 right 0 1)
    (foreach-frame
      (dochannels (current-channel 2)
        (setf (input-bus current-channel) (f32-ref left current-frame)
              (input-bus current-channel) (f32-ref right current-frame))))))

(dsp! fluid-out (lpf hpf)
  (:defaults 0 0)
  (foreach-frame
    (foreach-channel
      (with-samples ((c (input-bus current-channel)))
        (maybe-expand c)
        (unless (= 0d0 lpf)
          (setf c (lpf c lpf 2)))
        (unless (= 0d0 hpf)
          (setf c (hpf c hpf 2)))
        (cout c)))))

(fluidsynth:sfload *synth* "/home/sendai/Downloads/Nice-Keys-Suite-V1.0.sf2" 1)

(defvar *synth2* (fluidsynth:new *settings*))
(fluidsynth:sfload *synth2* "/home/sendai/Downloads/sf2/CTK-230_SoundFont.sf2" 1)

(defvar *synth3* (fluidsynth:new *settings*))
(fluidsynth:sfload *synth3* "/home/sendai/Downloads/KBH_Real_and_Swell_Choir.sf2" 1)

(defun in-times (l f)
  (declare (function f) (list l))
  (mapcar (lambda (x) (at (+ (now) #[x b])
                     f))
          l))

(defun dall (class-name)
  (setf *actors*
        (delete-if
         (lambda (x) (string= class-name (class-name (class-of x))))
         *actors*)))
(defun ds (name)
  (declare (keyword actor-name))
  (setf *actors*
        (delete-if (lambda (x) (eq name (slot-value x 'name)))
                   *actors*)))
(defun cs (name &optional (buf (box)) (pos (v! 0 2 0)) (instance 'cement) (rot (q:identity)) )
  (or (position-if (lambda (x) (equal name (slot-value x 'name))) *actors*)
      (push (make-instance instance :buf buf :name name :pos pos :rot rot) *actors*)))

(defparameter *fog* (make-line (alexandria:iota 23 :start 10 :step 3)))
(let* ((s (ov-scale :C5 :bartok))
      (c (make-heap s))
      (d (make-cycle '(nil t)))
      (f (make-cycle '(.1 .2 .3 .5 .6 1f0 .2)))
      (n (make-cycle '(5 10 5 10))))
  (defun f (time)
    (let ((*synth* *synth3*))
      (p time (pickl s) 50 1 0))
    ;;(pa time (next c (pick 2 3 1)) .333 (+ -30 (next *fog*)) 2 .333)
    ;;(pa time '(60 60 61) .333 70 0 .333)
    ;; (let ((*synth* *synth2*))
    ;;   (p time (- 60 24) (next *fog*) .5 1))
    (aat (+ time #[1 b])
         #'f it)))

(incudine:free (node 0))
(fluid-in *synth3* :id 9)
(fluid-in *synth*  :id 8)
(fluid-out :id 10 :after 9)
(set-controls (node 10) :lpf 300 :hpf 600)
(let ((*synth* *synth3*))
  (p (now) 50 50 1 1))

(defun f ())
(f (now))
