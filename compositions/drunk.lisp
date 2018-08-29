(in-package :somecepl)
;; Trying CM drunk, random walk

(defun plank (time root)
  (p time root 60 1 1)
  (aat (+ time #[.5 b]) #'plank
       it
       (pc-quantize (drunk root 5 :low 50 :high 70)
                    (scale 0 'ryukyu))))


(defun plank (time root)
  (p time root 60 1 1)
  (aat (+ time #[.5 b]) #'plank
       it
       (pc-relative root (pick 1 -1)
                    (scale 0 'ryukyu))))


(plank (now) 60)

(defvar pat1
  (cm:new cm:cycle 
       :of (list (cm:new cm:chord 
                   :of '(c5 d ef f g af bf c6))
                 (cm:new cm:chord
                   :of '(c4 d ef f g af bf c5) )
                 (cm:new cm:chord
                   :of '(c3 d ef f g af bf c) ))))

(defvar pat2
  (cm:new cm:cycle 
       :of (list '(c5 d ef f g af bf c6)
                 '(c4 d ef f g af bf c5) 
                 '(c3 d ef f g af bf c) )))


(defvar pat3
  (cm:new cm:cycle 
    :of (list (cm:new cm:chord 
                :of (cm:new cm:heap :notes '(c5 d ef f g af bf c6) 
                            :for (cm:new cm:weighting :of '(4 5))))
              (cm:new cm:chord
                :of (cm:new cm:heap :notes '(c4 d ef f g af bf c5) 
                            :for (cm:new cm:weighting :of '(4 5))))
              (cm:new cm:chord
                :of (cm:new cm:heap :notes '(c3 d ef f g af bf c) 
                            :for (cm:new cm:weighting :of '(4 5)))))))
