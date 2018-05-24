(in-package :sc)

(setf *s* (sc:make-external-server "localhost" :port 4444))
(sc:server-boot *s*)
(sc:server-quit *s*)

(in-package :somecepl)

(defparameter *synth* (play (sin-osc.ar 300 0 .2)))

(defsynth rhodey ((freq 440) (out 0) (gate 1) (pan 0) (amp 0.1)
                  (vel .8) (mod-index .2) (mix .2)
                  (lfo-speed .4) (lfo-depth .1))
  (let* ((lfo-speed (* 12 lfo-speed))
         (freq (* 2 freq))
         (env1 (env-gen.ar (adsr .001 1.25 .0 .04 1.0)))
         (env2 (env-gen.ar (adsr .001 1.00 .0 .04 1.0)))
         (env3 (env-gen.ar (adsr .001 1.50 .0 .04 1.0)))
         (env4 (env-gen.ar (adsr .001 1.50 .0 .04 1.0)))
         (osc4 (* TWOPI 2 0.535887 mod-index env4 vel (sin-osc.ar (* freq .5))))
         (osc3 (* env3 vel (sin-osc.ar freq osc4)))
         (osc2 (* TWOPI 0.108819 env2 vel (sin-osc.ar (* 15 freq))))
         (osc1 (* env1 vel (sin-osc.ar freq osc2)))
         (snd (mix (+ (* osc1 mix) (* osc3 (- 1 mix)))))
         (snd (* snd (+ 1 (* lfo-depth (sin-osc.ar lfo-speed)))))
         (snd (* snd (env-gen.ar (asr 0 1 .1) :gate gate :act :free)))
         (snd (pan2.ar snd pan amp)))
    (out.ar out snd)))

(synth 'rhodey :freq (midicps 62) :amp .5 :gate 0)

(defmacro pa (time notes offset)
  `(let* ((other (cdr ,notes))
          (offsets (loop :for o :from ,offset :by ,offset
                      :collect o :repeat (length other))))
     (at ,time (synth 'rhodey :freq (midicps (first ,notes))))
     (loop
        :for p :in other
        :for d :in offsets :do
        (at (+ ,time d) (synth 'rhodey :freq (midicps p))))))

(pa (quant 4) '(90 80 60) 1)

(defun f (time)
  (let ((next-time (+ time 4))
        (mychord   (make-chord 60 70 3 *phrygian*)))
    (at (+ 2 time)
      (synth 'rhodey
             :freq (midicps (+ -12 (first mychord)))
             :mix .4))
    (pa time mychord .5)
;;    (callback next-time #'f next-time)
    ))

(f (quant 4))

(defparameter *m* (synth 'rhodey))

(proxy :foo (sin-osc.ar 440 0 .4))
(proxy :foo (saw.ar 300 .2))
(proxy :foo (saw.ar (sin-osc.kr .5 0 150 300) .2))
(proxy :foo nil)

(defsynth drum2 ((freq 3000))
  (let* ((env (env-gen.kr (perc .001 .1) :act :free))
         (sig (lpf.ar (white-noise.ar) (* freq env))))
    (out.ar 0 (pan2.ar sig 0 .2))))

(drum 3000)


(defsynth saw-synth ((note 60) (dur 4.0))
  (let* ((env (env-gen.kr (env [0 .2 0] [(* dur .2) (* dur .8)]) :act :free))
         (freq (midicps note))
    	 (sig (lpf.ar (saw.ar freq env) (* freq 2))))
    (out.ar 0 [sig sig])))

(defun make-melody (time n &optional (offset 0))
  (when (> n 0)
    (at time (synth 'saw-synth :note (+ offset (alexandria:random-elt '(62 65 69 72)))))
      (let ((next-time (+ time (alexandria:random-elt '(0 1 2 1.5)))))
        (callback next-time #'make-melody next-time (- n 1) offset))))

(make-melody (quant 4) 16)


(defsynth saw-synth ((note 60) (dur 4.0))
  (let* ((env (env-gen.kr (env [0 .2 0] [(* dur .2) (* dur .8)]) :act :free))
         (freq (midicps note))
    	 (sig (lpf.ar (saw.ar freq env) (* freq 2))))
	(out.ar 0 [sig sig])))


(in-package :sc-user)

(defsynth saw-synth ((note 60) (dur 4.0))
  (let* ((env (env-gen.kr (env [0 .2 0] [(* dur .2) (* dur .8)]) :act :free))
         (freq (midicps note))
    	 (sig (lpf.ar (saw.ar freq env) (* freq 2))))
	(out.ar 0 [sig sig])))

