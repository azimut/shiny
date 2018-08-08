(in-package :somecepl)

(defparameter *patterns* (make-hash-table))

;; TODO: track the beat progression to skip ahead on redefinition
;;       add metro?
(defmacro defbeat (name duration beats &body body)
  "(defbeat kick .15 \"x---x-----------\"
      (synth 'atari :dur (- d .05) :tone0 15 :freq0 30))"
  (let ((i-name (intern (format nil "%~a" name)))
        (d (intern "D")))
    `(let ((l-beats (coerce ,beats 'list))
           (beats ,beats)
           (,d    ,duration))
       (defun ,i-name (time lbeats bbeats)
         (when (not (equal ,beats bbeats))
           (setf bbeats beats
                 lbeats (coerce bbeats 'list)))
         (cond ((or (eq #\1 (first lbeats))
                    (eq #\x (first lbeats)))
                ,@body)
               ((eq #\X (first lbeats))
                (let ((,d (* 2 ,d)))
                  ,@body)))
         (callback (+ time ,duration) #',i-name
                   (+ time ,duration)
                   (alexandria:rotate lbeats -1)
                   bbeats))
       (defun ,name ()
         (,i-name (quant 4) l-beats beats)))))

(defmacro defbeats (name duration beats notes &body body)
  (let ((i-name (intern (format nil "%~a" name)))
        (d (intern "D")))
    `(let* ((l-beats (coerce ,beats 'list))
            (l-beats (substitute #\x #\1 l-beats))
            (n-on-beats (loop :for b :in l-beats :count (eq #\x b)))
            (onotes  ,notes)
            (notes   ,notes)
            (notes   (subseq notes 0 (min n-on-beats (length notes))))
            (beats   ,beats)
            (,d      ,duration))
       (defun ,i-name (time lbeats bbeats lnotes lonotes)
         (when (not (equal onotes lonotes))
           (setf lnotes onotes
                 lonotes onotes))
         (when (not (equal ,beats bbeats))
           (setf bbeats beats
                 lbeats (coerce bbeats 'list)))
         (cond ((or (eq #\1 (first lbeats))
                    (eq #\x (first lbeats)))
                (let ((note (first lnotes)))
                  ,@body
                  (setf lnotes (alexandria:rotate lnotes -1))))
               ((eq #\X (first lbeats))
                (let ((,d (* 2 ,d)))
                  ,@body)))
         (callback (+ time ,duration) #',i-name
                   (+ time ,duration)
                   (alexandria:rotate lbeats -1)
                   bbeats
                   lnotes
                   lonotes))
       (defun ,name ()
         (,i-name (quant 4) l-beats beats notes onotes)))))

;;--------------------------------------------------

;; https://en.wikipedia.org/wiki/Drum_tablature
;; https://stackoverflow.com/questions/5457346/lisp-function-to-concatenate-a-list-of-strings
(defun concat (&rest list)
  "A non-recursive function that concatenates a list of strings."
  (if (listp list)
      (let ((result ""))
        (dolist (item list)
          (if (stringp item)
              (setq result (concatenate 'string result item))))
        result)))

(defclass drum-pattern ()
  ((name :initarg :name :reader pattern-name)
   (bd   :initarg :bd :reader bd)
   (ch   :initarg :ch :reader ch)
   (oh   :initarg :oh :reader oh)
   (sn   :initarg :sn :reader sn)))

(defmethod print-object ((obj drum-pattern) out)
  (print-unreadable-object (obj out :type t)
    (format out "~s" (pattern-name obj))))

(defun make-pattern (name short-name &key bd ch oh sn)
  (declare (string name) (symbol short-name))
  (flet ((f (s)
             (when (stringp s)
               (loop
                  :for c
                  :across (string-downcase s)
                  :when (or (eq c #\x) (eq c #\0)
                            (eq c #\~) (eq c #\-))
                  :collect
                  (if (or (eq c #\x)
                          (eq c #\0))
                      t
                      nil)))))
    (setf (gethash short-name *patterns*)
          (make-instance 'drum-pattern
                         :name name
                         :bd (f bd) :ch (f ch)
                         :oh (f oh) :sn (f sn)))))

(defun list-patterns ()
  (alexandria:hash-table-keys *patterns*))

(defun get-pattern (name)
  (declare (symbol name))
  (gethash name *patterns*))

(make-pattern
 "Synthetic Substitution"
 'ssub
 :bd "[0 ~ 0 ~][~ ~ ~ 0][~ 0 0 0][~ ~ ~ 0]"
 :sn "[~ ~ ~ ~][~ 0 ~ ~][~ ~ ~ ~][~ 0 ~ ~]"
 :ch (concat
      "[0 ~ 0 ~][0 ~ 0 ~][0 ~ 0 ~][0 ~ 0 ~]"
      "[0 ~ ~ ~][0 ~ 0 ~][0 ~ 0 ~][0 ~ 0 ~]")
 :oh "[~ ~ 0/2 ~][~ ~ ~ ~][~ ~ ~ ~][~ ~ ~ ~]")

(make-pattern
 "Get Up"
 'getup
 :bd "[0 ~ ~ ~][~ ~ ~ ~][~ ~ 0 ~][~ ~ 0 ~]"
 :sn "[~ ~ ~ ~][0 ~ 0 0][~ 0 ~ ~][0 ~ ~ 0]"
 :ch "[0 ~ ~ ~][0 ~ 0 0][0 ~ ~ ~][0 ~ 0 0]"
 :oh "[~ ~ 0 ~][~ ~ ~ ~][~ ~ 0 ~][~ ~ ~ ~]")

(make-pattern
 "Superstition"
 'superstition
 :bd "[0 ~ ~ ~][0 ~ ~ ~][0 ~ ~ ~][0 ~ ~ ~]"
 :sn "[~ ~ ~ ~][0 ~ ~ ~][~ ~ ~ ~][0 ~ ~ ~]"
 :ch "[0 ~ 0 ~][0 ~ 0 0][0 0 0 ~][0 ~ 0 0]")

(make-pattern
 "Rock Steady"
 'rsteady
 :bd (concat "[~ ~ 0 ~][0 ~ ~ 0][~ ~ 0 ~][0 ~ ~ ~]")
 :sn (concat "[~ 0 ~ ~][0 0 ~ 0][~ 0 ~ ~][0 0 ~ 0]")
 :ch (concat "[~ 0 ~ ~][0 ~ 0 ~][0 ~ ~ ~][0 ~ 0 ~]"
             "[0 ~ ~ ~][0 ~ 0 0][0 ~ ~ ~][0 ~ 0 0]")
 :oh (concat "[~ ~ 0 ~][~ ~ ~ 0][~ ~ 0 ~][~ ~ ~ 0]"
             "[~ 0 ~ ~][~ ~ ~ ~][~ ~ 0 ~][~ ~ ~ ~]"))

(make-pattern
 "Haitian Divorce"
 'haitian
 :bd "[~ ~ 0 ~][0 ~ ~ ~][~ ~ 0 ~][0 ~ ~ ~]"
 :sn "[~ 0 ~ ~][0 ~ 0 0][~ 0 ~ ~][0 ~ 0 0]"
 :ch "[0 0 ~ ~][0 0 0 0][0 0 ~ ~][0 0 0 0]"
 :oh "[~ ~ 0 ~][~ ~ ~ ~][~ ~ 0 ~][~ ~ ~ ~]")

(make-pattern
 "The Fez"
 'fez
 :bd (concat "[0 ~ ~ ~][~ ~ ~ ~][0 ~ ~ ~][~ ~ ~ ~]"
             "[0 ~ ~ ~][~ ~ ~ ~][0 ~ ~ 0][0 ~ ~ ~]")
 :sn (concat "[~ 0 ~ 0][0 0 ~ 0][~ 0 ~ 0][0 0 ~ 0]")
 :ch (concat "[~ ~ 0 ~][~ ~ 0 ~][~ ~ 0 ~][~ ~ 0 ~]"))

(make-pattern
 "Pop tech 2010"
 'pop
 :bd (concat "[0 ~ ~ ~][~ ~ ~ ~][~ ~ ~ ~][~ ~ ~ ~]"
             "[~ 0 ~ ~][~ ~ ~ ~][~ ~ ~ ~][~ 0 0 0]")
 :sn (concat "[~ ~ ~ ~][0 ~ ~ ~][~ ~ ~ ~][0 ~ ~ ~]")
 :ch (concat "[~ ~ ~ ~][~ ~ ~ ~][0 ~ ~ ~][~ ~ ~ ~]"
             "[0 ~ ~ ~][~ ~ ~ ~][0 ~ ~ ~][~ ~ ~ ~]")
 :oh (concat "[0 ~ ~ ~][~ ~ ~ ~][~ ~ ~ ~][~ ~ ~ ~]"
             "[~ ~ ~ ~][~ ~ ~ ~][~ ~ ~ ~][~ ~ ~ ~]"))

(make-pattern
 "Kissing My Love"
 'kml
 :bd (concat "[0 ~ ~ 0][~ ~ ~ ~][~ ~ ~ 0][~ ~ 0 ~]"
             "[0 ~ ~ ~][~ ~ ~ ~][~ ~ ~ 0][~ 0 ~ ~]")
 :sn (concat "[~ ~ ~ ~][0 ~ ~ ~][~ 0 ~ ~][0 ~ ~ ~]"
             "[~ ~ ~ ~][0 ~ ~ 0][~ 0 ~ ~][0 ~ ~ ~]")
 :ch (concat "[0 0 0 0][0 0 0 0][0 0 0 0][0 0 ~ ~]")
 :oh (concat "[~ ~ ~ ~][~ ~ ~ ~][~ ~ ~ ~][~ ~ 0 ~]"))

(make-pattern
 "Chug Chug Chug Chug A Lug"
 'chug
 :bd "[0 ~ ~ 0][~ 0 ~ 0][~ 0 ~ 0][~ ~ 0 ~]"
 :sn "[~ 0 0 ~][0 ~ ~ 0][~ 0 0 ~][0 ~ ~ ~]"
 :ch "[0 ~ 0 ~][0 0 0 ~][0 0 0 ~][0 ~ ~ ~]"
 :oh "[~ ~ ~ ~][~ ~ ~ ~][~ ~ ~ ~][~ ~ 0 ~]")

(make-pattern
 "Cold Sweat"
 'cold
 :bd (concat "[0 ~ ~ ~] [~ ~ ~ ~] [0 ~ 0 ~] [~ ~ ~ ~]")
 :sn (concat "[~ ~ ~ ~] [0 ~ ~ 0] [~ ~ ~ ~] [~ ~ 0 ~]"
             "[~ 0 ~ ~] [0 ~ ~ 0] [~ 0 ~ ~] [0 ~ ~ ~]")
 :ch (concat "[0 ~ ~ ~] [0 ~ 0 ~] [0 ~ ~ ~] [0 ~ 0 ~]")
 :oh (concat "[~ ~ 0 ~] [~ ~ ~ ~] [~ ~ 0 ~] [~ ~ ~ ~]"))

(make-pattern
 "Amen"
 'amen
 :bd (concat "[0 ~ 0 ~] [~ ~ ~ ~] [~ ~ 0 0] [~ ~ ~ ~]"
             "[0 ~ 0 ~] [~ ~ ~ ~] [~ ~ 0 0] [~ ~ ~ ~]"
             "[0 ~ 0 ~] [~ ~ ~ ~] [~ ~ 0 ~] [~ ~ ~ ~]"
             "[~ ~ 0 0] [~ ~ ~ ~] [~ ~ 0 ~] [~ ~ ~ ~]")
 :sn (concat "[~ ~ ~ ~] [0 ~ ~ 0] [~ 0 ~ ~] [0 ~ ~ 0]"
             "[~ ~ ~ ~] [0 ~ ~ 0] [~ 0 ~ ~] [0 ~ ~ 0]"
             "[~ ~ ~ ~] [0 ~ ~ 0] [~ 0 ~ ~] [~ ~ 0 ~]"
             "[~ 0 ~ ~] [0 ~ ~ 0] [~ 0 ~ ~] [~ ~ 0 ~]")
 :ch (concat "[0 ~ 0 ~] [0 ~ 0 ~] [0 ~ 0 ~] [0 ~ 0 ~]")
 :oh (concat "[~ ~ ~ ~] [~ ~ ~ ~] [~ ~ 0 ~] [~ ~ ~ ~]"))

(make-pattern
 "OOH Child"
 'oohchild
 :bd (concat "[0 ~ 0 ~][~ ~ ~ ~][0 ~ 0 0][~ ~ ~ ~]"
             "[0 ~ 0 0][~ ~ ~ ~][0 ~ 0 0][~ ~ ~ ~]")
 :sn (concat "[~ 0 ~ 0][0 0 ~ 0][~ 0 ~ 0][0 0 ~ 0]"
             "[~ 0 ~ 0][0 0 ~ 0][~ 0 ~ ~][0 ~ 0 ~]")
 :ch (concat "[0 0 ~ ~][0 0 ~ ~][0 0 ~ ~][0 0 ~ ~]")
 :oh (concat "[~ ~ 0 ~][~ ~ 0 ~][~ ~ 0 ~][~ ~ 0 ~]"))

(make-pattern
 "Use Me"
 'useme
 :bd (concat "[0 ~ 0 ~][~ 0 ~ 0][0 ~ 0 0][~ 0 ~ 0]"
             "[0 ~ 0 ~][~ 0 ~ ~][0 0 ~ 0][~ 0 ~ 0]")
 :sn (concat "[~ ~ ~ ~][0 ~ ~ 0][~ 0 ~ ~][0 ~ ~ 0]"
             "[~ ~ ~ ~][0 ~ ~ 0][~ ~ ~ ~][~ ~ ~ 0]")
 :ch (concat "[0 0 0 0][0 0 0 0][0 0 0 0][0 0 0 0]"
             "[0 0 0 0][0 0 0 0][0 ~ ~ ~][~ ~ ~ 0]")
 :oh (concat "[~ ~ ~ ~][~ ~ ~ ~][~ 0 ~ 0][~ 0 ~ ~]"))

(make-pattern
 "Use Me"
 'useme2
 :bd (concat "[0 ~ ~ ~][0 ~ ~ ~][~ ~ ~ ~][0 ~ ~ ~]"
             "[~ ~ ~ ~][0 ~ ~ 0][~ ~ 0 ~][0 ~ ~ ~]")
 :sn (concat "[~ ~ 0 ~][0 ~ 0 0][~ 0 0 ~][0 ~ 0 0]"
             "[~ 0 0 ~][0 ~ 0 0][~ 0 0 ~][0 ~ 0 0]")
 :ch (concat "[0 ~ ~ ~][0 0 0 0][0 0 ~ ~][0 0 0 0]"
             "[0 0 ~ ~][0 0 0 0][0 0 ~ ~][0 0 0 0]")
 :oh (concat "[~ ~ 0 ~][~ ~ ~ ~][~ ~ 0 ~][~ ~ ~ ~]"
             "[~ ~ 0 ~][~ ~ ~ ~][~ ~ 0 ~][~ ~ ~ ~]"))

(make-pattern
 "The Same Blood"
 'blood
 :bd "[0 0 ~ ~][~ ~ ~ ~][0 0 ~ ~][~ ~ ~ ~]"
 :sn "[~ ~ ~ 0][~ 0 0 ~][~ ~ ~ ~][0 0 0 ~]"
 :ch "[0 ~ 0 ~][0 ~ 0 0][0 ~ 0 0][0 ~ 0 0]")

(make-pattern
 "Lady"
 'lady
 :bd (concat "[0 ~ ~ ~][~ ~ ~ ~][0 ~ ~ 0][~ ~ 0 ~]"
             "[0 ~ ~ ~][~ ~ ~ ~][~ ~ ~ 0][~ ~ 0 ~]")
 :sn (concat "[~ ~ ~ ~][0 0 ~ ~][~ ~ ~ ~][~ ~ ~ ~]"
             "[~ ~ ~ ~][0 0 ~ ~][0 ~ ~ ~][~ ~ ~ ~]")
 :ch (concat "[~ ~ 0 ~][~ ~ 0 ~][~ ~ ~ ~][~ ~ ~ ~]")
 :oh (concat "[~ ~ 0 ~][~ ~ 0 ~][~ ~ ~ ~][~ ~ ~ ~]"))

(make-pattern
 "Funky President"
 'funkyp
 :bd "[0 ~ ~ 0][~ ~ ~ 0][~ 0 0 ~][~ ~ ~ ~]"
 :sn "[~ ~ ~ ~][0 ~ ~ ~][~ ~ ~ ~][0 ~ ~ ~]"
 :ch "[0 ~ 0 ~][0 ~ 0 ~][0 ~ ~ ~][0 ~ 0 ~]"
 :oh "[~ ~ ~ ~][0 ~ ~ ~][~ ~ 0 ~][~ ~ ~ ~]")

;; http://hiphoptranscriptions.com/post/159995606528
(make-pattern
 "Galactic"
 'galactic
 :bd "xx-- x-x- xx-- -x--"
 :sn "--x-"
 :ch "xxxx")
