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
        (+ (the fixnum (* current-frame 2))
           channel
           2))))

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

;;--------------------------------------------------
(dsp! fluid-in-down ((synth fluidsynth:synth))
  (with ((len   (block-size))
         (left  (make-f32-array len))
         (right (make-f32-array len)))
    (fluidsynth:write-float synth len left 0 1 right 0 1)
    (foreach-frame
      (dochannels (current-channel 2)
        (setf (bus 0) (f32-ref left current-frame)
              (bus 1) (f32-ref right current-frame))))))

(dsp! fluid-out-down ()
  (foreach-frame
    (foreach-channel
      (with-samples ((c (bus current-channel)))
        (cout c)))))
;;--------------------------------------------------

(bbuffer-load "/home/sendai/glenn.wav")
(incudine:free (node 0))
(bplay (gethash "glenn.wav" *buffers*) :rate 128 :amp .2)

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
  (declare (keyword name))
  (setf *actors*
        (delete-if (lambda (x) (eq name (slot-value x 'name)))
                   *actors*)))
(defun cs (name &optional (buf (box)) (pos (v! 0 2 0)) (scale 1f0)
                  (instance 'cement) (rot (q:identity)) )
  (or (and (position-if (lambda (x) (equal name (slot-value x 'name))) *actors*) (ds name))
      (push (make-instance instance :scale scale
                           :buf buf :name name :pos pos :rot rot) *actors*)))


(f (now))

(defparameter *fog*
  (make-line (alexandria:iota 23 :start 11 :step 3)))
(defparameter *fog*
  (make-line (alexandria:iota 23 :start 76 :step -3)))
(fpan 0 0)
(fpan 1 0)
(fp 3 2)
(fp 4 32)

(defun f ())
(f (now))
(defun hand (time)
  (if (odds .5)
      (p time (pc-random 70 90 (scale 0 'bartok))
         (rcosr 30 6 5) 1 4))
  (p time (pc-random 70 90 (scale 0 'bartok))
     (rcosr 30 6 5) (cosr .5 .3 5) 4))

(dall "CEMENT")
(let ((*synth* *synth2*))
  (fpan 0 30)
  (fp 2 49)
  (fp 3 11))
(let* ((s (make-heap (ov-scale :C5 :bartok)))
       (c (make-heap s))
       (flip (make-cycle '(t nil))))
  (defun f (time)
    (let (;;(*synth* *synth2*)
          (fog (next *fog*)))
      (if (and (> fog 40)
               (odds .9))
          (let ((n (pick 3 2 1)))
            (in-times
             (loop :for i :by .3333 :repeat n :collect i)
             (lambda () (cs (gensym) (sphere) (v! (1- (random 2f0)) 0 (1- (random 2f0))) (between .2 .6))))
            (pa time (next c n)
                (rcosr (+ -40 fog) 5 5)
                .333 2 .333))))
    (cs :d1 (box) (v! 0 0 0))
    (when (zmodt 4) (dall "CEMENT"))
    (let ((fog (next *fog*)))
      (when (and (= 10 fog) (zmodt 3))
        (setf *cam-vector* (v! (cync time) (sync time) (random 1f0)))
        (let ((*synth* *synth3*)) (p time 60 (+ (- (next *fog*)) (rcosr 30 5 5)) 1 3))))
    (pa time '(60 60 61) '(55 50 50) '(.333 .1 .1) 0 .333)
    (unless (zmodt 6)
      (let ((*synth* *synth2*)) (p time 30 30 .3 0)))
    (aat (+ time #[1 b])
         #'f it)))

(set-controls (node 10) :lpf 900 :hpf 400)
(set-controls (node 10) :lpf 50 :hpf 200)
(defun f ())
(fp 1 5)
(fg 1.5f0)
(let ((*synth* *synth2*)) (fg 1.5f0))
(f (now))
(let ((*synth* *synth3*))
  (fp 0 4 0))
(fluid-test *synth3*)
(fp 0 0 0)
(fg 2f0)
(p (now) 50 50 1 0)
(incudine:free (node 0))

(fluid-in *synth* :id 9)
(fluid-out :id 10 :after 9)

(fluid-test *synth2*)

(fluid-in-down *synth2* :id 11)
(fluid-out-down :id 12 :after 11)



(let ((*synth* *synth2*))
  (p (now) 50 50 1 1))

(defun f ())
(f (now))


(dsp! buffer-play-test ((buf buffer))
  (with ((o (make-buffer (block-size))))
    (declare (buffer o))
    (foreach-frame
      (cout (buffer-play buf)))))

(incudine:free (node 0))
(buffer-play-test (gethash "glenn.wav" *buffers*))
