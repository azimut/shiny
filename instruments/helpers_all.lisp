(in-package :shiny)


;;(fluidsynth:sfload *synth* "/usr/share/sounds/sf2/FluidR3_GM.sf2" 1)
;;(fluidsynth:sfload *synth* "/home/sendai/Downloads/Nice-Keys-Suite-V1.0.sf2" 1)
;;(fluidsynth:sfload *synth* "/home/sendai/Downloads/sf2/CTK-230_SoundFont.sf2" 1)
;;(fluidsynth:sfload *synth* "/home/sendai/Downloads/sf2/The_Ultimate Megadrive_Soundfont.sf2" 1)
;;(fluidsynth:sfload *synth* "/home/sendai/Downloads/sf2/999/Imaginary.sf2" 1)
;;(fluidsynth:sfload *synth* "/usr/share/sounds/sf2/FluidR3_GM.sf2" 1)
;;(fluidsynth:sfload *synth* "/home/sendai/samples/Touhou.sf2" 1)
;;(fluidsynth:sfload *synth* "/home/sendai/Touhou.sf2.sf2" 1)
;;(fluidsynth:sfload *synth* "/home/sendai/Downloads/KBH_Real_and_Swell_Choir.sf2" 1)
;;(fluidsynth:sfload *synth* "/home/sendai/Downloads/Majora_s_Mask_N64_Soundfont.sf2" 1)
;;(fluidsynth:sfload *synth* "/home/sendai/Downloads/sf2/Super_Nintendo_Unofficial_update.sf2" 1)
;;(fluidsynth:sfload *synth* "/home/sendai/Downloads/Nice-Keys-Suite-V1.0.sf2" 1)
;;(fluidsynth:delete *synth*)


;; (defgeneric pa (time notes offset velocity channel duration &key pan)
;;   (:documentation "Play the given notes as an arpeggio")
;;   (:method ((time double-float) (notes number) (offset number) (velocity integer) (channel integer) (duration number) &key pan)
;;     (if pan
;;         (p time notes velocity offset channel :pan pan)
;;         (p time notes velocity offset channel)))
;;   (:method ((time double-float) (notes list) (offset number) (velocity integer) (channel integer) (duration number) &key pan)
;;     (let* ((lnotes  (length notes))
;;            (offsets (loop :for i :from 0 :by offset :collect i :repeat lnotes)))
;;       (when pan
;;         (fluidsynth:cc *synth* channel 10 pan))
;;       (mapcar (lambda (n o) (p (+ time o) n velocity duration channel))
;;               notes
;;               offsets)))
;;   (:method ((time double-float) (notes list) (offset number) (velocity integer) (channel list) (duration number) &key pan)
;;     (let* ((lnotes  (length notes))
;;            (offsets (loop :for i :from 0 :by offset :collect i :repeat lnotes)))
;;       (mapcar (lambda (n o c) (p (+ time o) n velocity duration c))
;;               notes
;;               offsets
;;               channel)))
;;   (:method ((time double-float) (notes list) (offset number) (velocity list) (channel integer) (duration number) &key pan)
;;     (let* ((lnotes  (length notes))
;;            (offsets (loop :for i :from 0 :by offset :collect i :repeat lnotes)))
;;       (mapcar (lambda (n o v) (p (+ time o) n v duration channel))
;;               notes
;;               offsets
;;               velocity)))
;;   (:method ((time double-float) (notes list) (offset number) (velocity list) (channel list) (duration number) &key pan)
;;     (let* ((lnotes  (length notes))
;;            (offsets (loop :for i :from 0 :by offset :collect i :repeat lnotes)))
;;       (mapcar (lambda (n o v c) (p (+ time o) n v duration c))
;;               notes
;;               offsets
;;               velocity
;;               channel)))  
;;   (:method ((time double-float) (notes list) (offset number) (velocity integer) (channel integer) (duration list) &key pan)
;;     (let* ((lnotes  (length notes))
;;            (offsets (loop :for i :from 0 :by offset :collect i :repeat lnotes)))
;;       (mapcar (lambda (n o d) (p (+ time o) n velocity d channel))
;;               notes
;;               offsets
;;               duration)))
;;   (:method ((time double-float) (notes list) (offset number) (velocity integer) (channel list) (duration list) &key pan)
;;     (let* ((lnotes  (length notes))
;;            (offsets (loop :for i :from 0 :by offset :collect i :repeat lnotes)))
;;       (mapcar (lambda (n o c d) (p (+ time o) n velocity d c))
;;               notes
;;               offsets
;;               channel
;;               duration)))
;;   (:method ((time double-float) (notes list) (offset number) (velocity list) (channel integer) (duration list) &key pan)
;;     (let* ((lnotes  (length notes))
;;            (offsets (loop :for i :from 0 :by offset :collect i :repeat lnotes)))
;;       (mapcar (lambda (n o v d) (p (+ time o) n v d channel))
;;               notes
;;               offsets
;;               velocity
;;               duration)))
;;   (:method ((time double-float) (notes list) (offset number) (velocity list) (channel list) (duration list) &key pan)
;;     (let* ((lnotes  (length notes))
;;            (offsets (loop :for i :from 0 :by offset :collect i :repeat lnotes)))
;;       (mapcar (lambda (n o v c d) (p (+ time o) n v d c))
;;               notes
;;               offsets
;;               velocity
;;               channel
;;               duration)))
;;   (:method ((time double-float) (notes list) (offset list) (velocity integer) (channel integer) (duration number) &key pan)
;;     (mapcar (lambda (n o) (p (+ time o) n velocity duration channel))
;;             notes
;;             offset))
;;   (:method ((time double-float) (notes list) (offset list) (velocity integer) (channel list) (duration number) &key pan)
;;     (mapcar (lambda (n o c) (p (+ time o) n velocity duration c))
;;             notes
;;             offset
;;             channel))
;;   (:method ((time double-float) (notes list) (offset list) (velocity list) (channel integer) (duration number) &key pan)
;;     (mapcar (lambda (n o v) (p (+ time o) n v duration channel))
;;             notes
;;             offset
;;             velocity))
;;   (:method ((time double-float) (notes list) (offset list) (velocity list) (channel list) (duration number) &key pan)
;;     (mapcar (lambda (n o v c) (p (+ time o) n v duration c))
;;             notes
;;             offset
;;             velocity
;;             channel))
;;   (:method ((time double-float) (notes list) (offset list) (velocity integer) (channel integer) (duration list) &key pan)
;;     (mapcar (lambda (n o d) (p (+ time o) n velocity d channel))
;;             notes
;;             offset
;;             duration))
;;   (:method ((time double-float) (notes list) (offset list) (velocity integer) (channel list) (duration list) &key pan)
;;     (mapcar (lambda (n o c d) (p (+ time o) n velocity d c))
;;             notes
;;             offset
;;             channel
;;             duration))
;;   (:method ((time double-float) (notes list) (offset list) (velocity list) (channel integer) (duration list) &key pan)
;;     (mapcar (lambda (n o v d) (p (+ time o) n v d channel))
;;             notes
;;             offset
;;             velocity
;;             duration))
;;   (:method ((time double-float) (notes list) (offset list) (velocity list) (channel list) (duration list) &key pan)
;;     (mapcar (lambda (n o v c d) (p (+ time o) n v d c))
;;             notes
;;             offset
;;             velocity
;;             channel
;;             duration)))
