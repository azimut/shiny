(in-package :somecepl)

(defparameter *drums* (make-hash-table))

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
            (n-on-beats (length (loop for b in l-beats when (eq #\x b)
                                   collect b)))
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
  ((kick :initform nil :initarg :kick :accessor kick-of)
   (snare :initform nil :initarg :snare :accessor snare-of)
   (chat :initform nil :initarg :chat :accessor chat-of)))

(defmethod kick-of ((pattern drum-pattern))
  (slot-value pattern 'kick))
(defmethod snare-of ((pattern drum-pattern))
  (slot-value pattern 'snare))
(defmethod chat-of ((pattern drum-pattern))
  (slot-value pattern 'chat))

(defun pattern-of (name element)
  (let ((pattern (gethash name *drums*)))
    (case element
      (:kick (kick-of pattern))
      (:snare (snare-of pattern))
      (:chat (chat-of pattern))
      (t (error "not valid element")))))

(setf (gethash :moses *drums*)
      (make-instance
       'drum-pattern
       :kick (concat  "x---" "x---" "x--x" "----")
       :snare (concat "----" "x---" "----" "x---")
       :chat (concat  "----" "---x" "----" "---x")))

(setf (gethash :amen *drums*)
      (make-instance
       'drum-pattern
       :kick (concat "x-x-" "----" "--xx" "----"
                     "x-x-" "----" "--xx" "----"
                     "x-x-" "----" "--xx" "----"
                     "--xx" "----" "--x-" "----")
       :snare (concat "----" "x--x" "-x--" "x--x"
                      "----" "x--x" "-x--" "x--x"
                      "----" "x--x" "-x--" "--x-"
                      "-x--" "x--x" "-x--" "--x-")
       :chat (concat "x-x-")))

(setf (gethash :funky *drums*)
      (make-instance
       'drum-pattern
       :kick (concat "x-x-" "--x-" "--x-" "-x--")
       :snare (concat "----" "x--x" "-x-x" "x--x")
       :chat (concat "xxxxxxx-" "xxxxx-xx")))


;; http://hiphoptranscriptions.com/post/159995606528
(setf (gethash :galactic *drums*)
      (make-instance
       'drum-pattern
       :kick  (concat "xx--" "x-x-" "xx--" "-x--")
       :snare "--x-"
       :chat  "xxxx"))
