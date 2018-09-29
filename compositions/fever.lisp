(in-package :shiny)

(gmeplay "/home/sendai/nin2/Mega Man 2.nsfe" 4 0
         :amp .005 :length 18 :voices '(2 0)
;;         :load-only t
         )

(gmeplay "/home/sendai/nin2/Mega Man 2.nsfe" 5 0
         :amp .005 :length 30 :voices '(3)
;;         :load-only t
         )

(incudine:free (node 5))

(defun f (time)
  (bbplay (gethash "/home/sendai/nin2/Mega Man 2.nsfe4" *playing*)
          :id 4 :amp .01)
  (bbplay (gethash "/home/sendai/nin2/Mega Man 2.nsfe5" *playing*)
          :id 5 :amp .01)
  (aat (+ time #[1 b]) #'f it))

(f (tempo-sync #[1 b]))
(defun f ())

(let* ((mod12 (cm::scanons 4 7 3))
       (mel1  (make-cycle (cm:transpose mod12 40)))
       (mel2  (make-cycle (alexandria:rotate (cm:transpose mod12 60))))
       (pan1  (make-palindrome (iota 127)))
       (lin2  (make-line (iota 35 :start 10)))
       (pan2  (make-line (iota 64 :start 127 :step -1))))
  (defun f (time)
    (pa time (next mel1) .25 50 0 '(.2 .2 .2 .25) :pan (next pan1))
    (pa (+ #[.1 b] time) (next mel2) .333 (next lin2) 1 .333 :pan (next pan2))
    (aat (+ time #[1 b]) #'f it)))

(let* ((mod12 (cm::scanons 4 7 3))
       (mel1  (make-cycle (cm:transpose mod12 40)))
       (mel2  (make-cycle (alexandria:rotate (cm:transpose mod12 60))))
       (pan1  (make-palindrome (iota 127))))
  (defun f (time)
    (pa time (next mel1) .25 50 0 '(.2 .2 .2 .25) :pan (next pan1))
    (pa (+ #[.1 b] time) (next mel2) .333 45 1 .333 :pan 64)
    (aat (+ time #[1 b]) #'f it)))

(fg 1f0)
(fp 1 32)
(fp 2 2)
(defun f ())
(f (now))


(cm::tintab '(60 62 64) cm::stravmode)

(let* ((mod12 (cm::scanons 4 7 3))
       (mel1  (make-cycle (cm:transpose mod12 40)))
       (mel2  (make-cycle (mapcar (lambda (x) (cm::tintab x cm::stravmode))
                                  (cm::transpose mod12 60))))
       (pan1  (make-palindrome (iota 127))))
  (defun f (time)
    (pa time (next mel1) .25 50 0 '(.2 .2 .2 .25) :pan (next pan1))
    (let ((*window-name* "sec"))
      (pa (+ #[.1 b] time) (fourth (next mel2)) .333 45 1 .333 :pan 64))
    (aat (+ time #[1 b]) #'f it)))

(fp 1 0)
(f (now))

(defun f ())

(defparameter *notes* (cm::heapvec 20 10 50))
(defparameter *strums* (cm::strums 20 2 6 4 6))
(defparameter *rhythm* (cm::ferney '(1) '(5) *strums*))

(let ((rhythm (make-cycle (cm::ferney '(1) '(4) *strums*)))
      (notes (make-cycle *notes*)))
  (defun f (time)
    (let ((r (next rhythm)))
      (p time (next notes) 50 r 0)
      (aat (+ time #[r b]) #'f it))))


(let ((rhythm (make-cycle (cm::ferney '(1) '(3) *strums*)))
      (notes (make-cycle (cm::transp *notes* 21))))
  (defun ff (time)
    (let ((r (next rhythm)))
      (p time (next notes) 50 r 0)
      (aat (+ time #[r b]) #'ff it))))

(f (now))
(ff (now))
(defun f ())
(defun ff ())

;;--------------------------------------------------

(let ((chords (make-cycle
               (cm::transp
                (append
                 (cm::generic-path
                  #'cm::tonnetz-func
                  '(0 4 7) '(3 6 10)))
                50))))
  (defun f (time)
    (play-synth-arp (next chords) .333 .333 :amp .01)
    (aat (+ time #[1 b]) #'f it)))

(defun f ())
(f (now))
