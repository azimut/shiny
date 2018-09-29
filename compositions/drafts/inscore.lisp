;; (inscore-reset)
;; (inscore-init)
;;(inscore-stream :meter "4/4")
;; (inscore-write (pick "_" "a" "b" "c" "d" "f" "g" "h"))
;; (inscore-write "_/32")
;; (inscore-write "a*1/4")
;; (inscore-write "a/2")


;; file:///home/sendai/projects/inscore/doc/OSCMsg/OSCMsgch7.html

(osc:message *oscout* "/ITL/scene/l/v1" "sss" "set" "video" "/home/sendai/bunny.mp4")
;; OR JUST THE 4th color param
;; the less the more transparent
(osc:message *oscout* "/ITL/scene/l/v1" "si" "alpha" 100)

;; move Y = -.5 .5
;; move X = -1.0 1.0
;; (osc:message *oscout* "/ITL/scene/l/v1" "sf" "x" 0f0)
;; (osc:message *oscout* "/ITL/scene/l/v1" "sf" "y" 0f0)

;; play 1, does not loop...does nothing if already playing
;; play 0, pause
;;(osc:message *oscout* "/ITL/scene/l/v1" "si" "play" 1)

;; volume max 1f0
;;(osc:message *oscout* "/ITL/scene/l/v1" "si" "volume" .1f0)
;; scale video
;;(osc:message *oscout* "/ITL/scene/l/v1" "sf" "scale" 2.4f0)
;; del
;;(osc:message *oscout* "/ITL/scene/l/v1" "s" "del")w

;; rate - changing the rate on the flay makes all janky...
;;(osc:message *oscout* "/ITL/scene/l/v1" "sf" "rate" .5f0)

(osc:message *oscout* "/ITL/scene/l/v1" "si" "vdate" 20000)

(osc:message *oscout* "/ITL/scene/l/v1" "sf" "rotatez" 0f0)

(osc:message *oscout* "/ITL/scene/l/v1" "ssi" "effect" "blur" 9)
;; strength, tint
(osc:message *oscout* "/ITL/scene/l/v1" "ssfiii" "effect" "colorize"
             0.5 0 255 10)

(osc:message *oscout* "/ITL/scene" "si" "alpha" 255)
(osc:message *oscout* "/ITL/scene/l/background" "si" "alpha" 255)

;; FIXME: this yells object! use CLOS!
(defun inscore-video
    (video dur
     &key (window-name *window-name*) (layer-name *layer-name*)
       (video-name *video-name*) (volume .1) (scale 1f0 scale-p)
       (rate 1f0 rate-p) (x 1f0 x-p) (y 1f0 y-p) (alpha 100 alpha-p)
       (rotatex 0f0 rotatex-p) (rotatey 0f0 rotatey-p) (rotatez 0f0 rotatez-p))
  (let ((osc-path
         (format nil "/ITL/~a/~a/~a" window-name layer-name video-name)))
    (assert (uiop:file-exists-p video))
    ;;(inscore-init)
    (osc:message *oscout* osc-path "sss" "set" "video" video)
    (osc:message *oscout* osc-path "sf" "volume" volume)
    (osc:message *oscout* osc-path "si" "play" 1)
    (when alpha-p
      (osc:message *oscout* osc-path "si" "alpha" alpha))
    (when rotatex-p
      (osc:message *oscout* osc-path "sf" "rotatex" rotatex))
    (when rotatey-p
      (osc:message *oscout* osc-path "sf" "rotatey" rotatey))
    (when rotatez-p
      (osc:message *oscout* osc-path "sf" "rotatez" rotatez))
    (when x-p
      (osc:message *oscout* "/ITL/scene/l/v1" "sf" "x" x))
    (when y-p
      (osc:message *oscout* "/ITL/scene/l/v1" "sf" "y" y))
    (when rate-p
      (osc:message *oscout* osc-path "sf" "rate" rate))
    (when scale-p
      (osc:message *oscout* osc-path "sf" "scale" scale))    
    (at (+ (now) #[dur b])
        #'(lambda () (osc:message *oscout* osc-path "s" "del")))))
