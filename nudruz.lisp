(in-package :somecepl)

;;
;; Nudruz
;;

;; Code copied from https://github.com/gogins/gogins.github.io/tree/master/nudruz

;; EXPWARP -- 'warps' pits by expt factor
;; (above optional bass-note, or lowest note in chd)
;; ME: added (round) to return value
(defun expwarp (pits factor &optional (bassnote nil))
      (let* ((orig-hz (remove-duplicates (cm:hertz pits)))
	     (bn (if bassnote bassnote (apply #'min orig-hz)))
	     (hzdiffs (mapcar (lambda (x) (- x bn)) orig-hz)))
	(loop for n to (- (length orig-hz) 1) collect
	      (round (cm:keynum
	       (+ bn (* (nth n hzdiffs) factor))
	       :hz 't)))))
