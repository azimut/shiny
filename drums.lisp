(in-package :somecepl)

(defparameter *patterns* (make-hash-table))

;; Drum Patterns took from:
;; https://github.com/lvm/tidal-drum-patterns
;; http://hiphoptranscriptions.com/post/159995606528
;;
;; Info about pattern names:
;; https://en.wikipedia.org/wiki/Drum_tablature
;;
;; TODO:
;; - Some definition lost while translating them to t/nil
;;       For example some are 0/2.
;;
;; - Others not done to being unable to understand the pattern format yet.
;; $ grep -l -e ']]' -e ']/' -e '\[\[' *.hs
;; Breaks.hs
;; Jungle.hs

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


(defun parse-pattern (s)
  (when s
    (loop
       :for c
       :across (string-downcase s)
       :when (or (eq c #\x) (eq c #\0)
                 (eq c #\~) (eq c #\-))
       :collect
       (if (or (eq c #\x)
               (eq c #\0))
           t
           nil))))

(defun parse-patternc (s)
  (let ((p (parse-pattern s)))
    (when p
      (make-cycle p))))

(defun make-pattern (name short-name &key bd ch oh sn)
  (declare (string name) (symbol short-name))
  (setf (gethash short-name *patterns*)
        (make-instance
         'drum-pattern
         :name name
         :bd (parse-pattern bd)
         :ch (parse-pattern ch)
         :oh (parse-pattern oh)
         :sn (parse-pattern sn))))

(defun list-patters ()
  (alexandria:hash-table-keys *patterns*))

(defun get-pattern (name)
  (declare (symbol name))
  "returns a list of the pattern"
  (gethash name *patterns*))

(make-pattern
 "Mother Popcorn"
 'popcorn
 :bd (concat "[0 ~ 0 ~][~ ~ ~ ~][~ ~ 0 ~][~ ~ ~ ~]"
             "[~ ~ 0 ~][~ ~ 0 ~][~ ~ 0 ~][~ ~ 0 ~]")
 :sn (concat "[~ ~ ~ ~][0 ~ ~ 0][~ 0 ~ ~][~ ~ 0 ~]"
             "[~ 0 ~ 0][0 0 ~ 0][~ 0 ~ 0][0 0 ~ 0]")
 :ch (concat "[0 ~ ~ ~]"))

(make-pattern
 "Book Of Moses"
 'moses
 :bd "[0 ~ ~ ~][0 ~ ~ ~][0 ~ ~ 0][~ ~ ~ ~]"
 :sn "[~ ~ ~ ~][0 ~ ~ ~][~ ~ ~ ~][0 ~ ~ ~]"
 :ch "[0 ~ 0 ~]")

(make-pattern
 "Synthetic Substitution"
 'ssub
 :bd "[0 ~ 0 ~][~ ~ ~ 0][~ 0 0 0][~ ~ ~ 0]"
 :sn "[~ ~ ~ ~][~ 0 ~ ~][~ ~ ~ ~][~ 0 ~ ~]"
 :ch (concat
      "[0 ~ 0 ~][0 ~ 0 ~][0 ~ 0 ~][0 ~ 0 ~]"
      "[0 ~ ~ ~][0 ~ 0 ~][0 ~ 0 ~][0 ~ 0 ~]")
 :oh "[~ ~ 0 ~][~ ~ ~ ~][~ ~ ~ ~][~ ~ ~ ~]")

(make-pattern
 "Get Up"
 'getup
 :bd "[0 ~ ~ ~][~ ~ ~ ~][~ ~ 0 ~][~ ~ 0 ~]"
 :sn "[~ ~ ~ ~][0 ~ 0 0][~ 0 ~ ~][0 ~ ~ 0]"
 :ch "[0 ~ ~ ~][0 ~ 0 0][0 ~ ~ ~][0 ~ 0 0]"
 :oh "[~ ~ 0 ~][~ ~ ~ ~][~ ~ 0 ~][~ ~ ~ ~]")

(make-pattern
 "Express Yourself"
 'express
 :bd (concat "[0 ~ ~ 0][0 ~ ~ 0][~ ~ 0 ~][0 ~ ~ 0]"
             "[0 ~ ~ 0][~ ~ ~ ~][0 ~ ~ 0][~ ~ 0 ~]")
 :sn (concat "[~ ~ ~ ~][0 ~ ~ 0][~ 0 ~ 0][~ 0 ~ 0]"
             "[~ ~ ~ ~][0 ~ ~ 0][~ 0 ~ 0][0 ~ ~ ~]")
 :ch (concat "[0 0 0 0][0 0 0 0][0 0 0 0][0 0 0 0]"
             "[0 0 0 0][0 0 0 0][0 0 0 0][0 0 0 ~]"))

(make-pattern
 "Superstition"
 'superstition
 :bd "[0 ~ ~ ~][0 ~ ~ ~][0 ~ ~ ~][0 ~ ~ ~]"
 :sn "[~ ~ ~ ~][0 ~ ~ ~][~ ~ ~ ~][0 ~ ~ ~]"
 :ch "[0 ~ 0 ~][0 ~ 0 0][0 0 0 ~][0 ~ 0 0]")

(make-pattern
 "Respect Yourself"
 'respect
 :bd (concat "[0 ~ ~ ~][0 ~ ~ ~][0 ~ ~ ~][0 ~ ~ ~]")
 :sn (concat "[~ ~ ~ ~][0 ~ ~ ~][~ ~ 0 ~][0 ~ ~ ~]"
             "[~ ~ ~ ~][0 ~ ~ ~][0 ~ 0 ~][0 ~ ~ ~]")
 :ch (concat "[0 ~ 0 ~]"))

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
 "GrooveMe"
 'grooveme
 :bd "[0 ~ ~ 0][0 ~ ~ 0][0 0 ~ 0][~ 0 ~ 0]"
 :sn "[~ ~ ~ ~][0 ~ ~ ~][~ ~ ~ ~][0 ~ ~ ~]"
 :ch "[0 ~ 0 ~]")

(make-pattern
 "The Thrill Is Gone"
 'thrill
 :bd (concat "[~ ~ ~ ~][~ ~ ~ 0][0 ~ 0 ~][~ ~ ~ ~]")
 :sn (concat "[0 ~ ~ ~][0 ~ ~ ~][0 ~ ~ ~][0 ~ ~ ~]")
 :ch (concat "[0 ~ 0 ~]"))

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
 "Knocks Me Off My Feet"
 'knocks
 :bd "[0 ~ 0 ~][0 ~ ~ 0][0 ~ 0 ~][0 ~ ~ 0]"
 :sn "[~ ~ ~ ~][0 ~ ~ ~][~ ~ ~ ~][0 ~ ~ ~]"
 :ch "[0 ~ ~ ~][~ ~ 0 0][~ 0 ~ ~][~ ~ 0 ~]"
 :oh "[~ ~ 0 ~][~ ~ ~ ~][~ ~ 0 ~][~ ~ ~ ~]") 

(make-pattern
 "Cissy Strut"
 'cissy
 :bd "[0 ~ ~ 0][~ 0 ~ ~][0 0 ~ 0][~ 0 ~ ~]"
 :sn "[0 0 0 ~][0 ~ 0 0][~ 0 0 ~][0 ~ 0 ~]"
 :ch "[0 0 0 ~][0 ~ 0 0][~ 0 0 ~][0 ~ 0 ~]")

(make-pattern
 "Cissy Strut 2"
 'cissy2
 :bd (concat "[0 ~ ~ 0][~ 0 ~ ~][~ 0 ~ 0][0 ~ 0 ~]"
             "[0 ~ ~ 0][~ ~ ~ 0][~ 0 ~ 0][0 ~ 0 ~]")
 :sn (concat "[~ ~ ~ ~][0 ~ ~ 0][~ 0 0 ~][~ ~ ~ ~]"
             "[~ ~ 0 ~][~ 0 0 ~][0 0 ~ ~][~ ~ ~ ~]")
 :oh (concat "[~ ~ ~ ~][~ ~ ~ ~][~ ~ ~ ~][0 ~ 0 ~]"))

(make-pattern
 "StrBtsDcGogo100"
 'gogo
 :bd "[0 ~ ~ 0][~ ~ 0 ~][~ ~ 0 ~][~ ~ ~ ~]"
 :sn "[~ 0 ~ ~][0 ~ ~ 0][~ ~ ~ ~][0 ~ ~ ~]"
 :ch "[~ 0 0 ~][0 0 ~ 0][~ ~ ~ ~][~ 0 0 ~]"
 :oh "[~ ~ ~ ~][~ ~ ~ ~][0 ~ ~ ~][~ ~ ~ ~]")

(make-pattern
 "Come Dancing"
 'dancing
 :bd (concat "[0 ~ ~ ~][~ ~ ~ 0][0 ~ ~ ~][~ ~ ~ 0]"
             "[0 ~ 0 ~][~ 0 ~ 0][0 ~ ~ ~][~ ~ ~ 0]")
 :sn (concat "[~ 0 0 ~][0 0 0 ~][~ 0 0 ~][0 0 0 ~]"
             "[~ 0 ~ ~][0 0 ~ ~][~ 0 ~ ~][0 0 0 ~]")
 :ch (concat "[0 ~ 0 ~]"))

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
 "Cowd Bell"
 'cbell
 :bd (concat "[0 ~ ~ 0][~ ~ 0 0][~ ~ 0 0][~ 0 ~ 0]"
             "[0 ~ 0 0][~ ~ ~ 0][0 ~ 0 0][~ 0 ~ 0]")
 :sn (concat "[0 ~ 0 0][~ ~ ~ 0][0 ~ 0 0][~ 0 ~ 0]")
 :ch (concat "[0 ~ 0 0][0 ~ 0 0][0 ~ 0 0][0 ~ 0 0]"))

(make-pattern
 "Expensive Shit"
 'expensive
 :bd (concat "[~ ~ ~ 0][~ ~ 0 ~][~ ~ ~ ~][~ ~ 0 ~]")
 :sn (concat "[0 0 ~ 0][~ 0 ~ ~][0 0 ~ ~][0 0 ~ ~]")
 :ch (concat "[0 ~ 0 0][0 ~ 0 0][0 ~ 0 ~][0 ~ 0 0]"
             "[0 ~ 0 ~][0 ~ 0 0][0 ~ 0 0][0 ~ 0 0]")
 :oh (concat "[~ ~ ~ ~][~ ~ ~ ~][~ ~ ~ 0][~ ~ ~ ~]"
             "[~ ~ ~ 0][~ ~ ~ ~][~ ~ ~ ~][~ ~ ~ ~]"))

(make-pattern
 "Hook And Sling"
 'hook
 :bd (concat "[0 ~ 0 ~][~ ~ ~ ~][~ 0 ~ ~][~ 0 0 ~]"
             "[~ ~ ~ ~][~ ~ ~ ~][~ ~ ~ ~][~ 0 ~ ~]")
 :sn (concat "[~ ~ ~ ~][0 ~ 0 0][~ ~ 0 ~][0 ~ ~ ~]"
             "[0 ~ ~ ~][0 0 ~ 0][~ ~ 0 0][~ ~ 0 0]")
 :ch (concat "[0 ~ 0 0][~ 0 ~ ~][0 0 ~ 0][~ ~ ~ ~]"
             "[0 0 ~ 0][~ ~ 0 ~][0 0 ~ ~][0 ~ 0 ~]")
 :oh (concat "[~ ~ ~ ~][~ ~ ~ ~][~ ~ ~ ~][~ ~ 0 ~]"))

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
 "Lady Marmalade"
 'ladym
 :bd (concat "[0 ~ ~ ~][~ ~ 0 ~][0 ~ ~ ~][~ ~ 0 ~]"
             "[~ ~ ~ ~][~ ~ ~ ~][~ ~ ~ ~][0 ~ ~ ~]")
 :sn (concat "[~ ~ ~ ~][0 ~ ~ ~][~ ~ ~ ~][0 ~ ~ ~]"
             "[~ ~ ~ ~][0 ~ ~ ~][0 ~ ~ ~][~ ~ ~ ~]")
 :ch (concat "[0 ~ 0 ~][0 ~ 0 ~][0 ~ 0 ~][0 ~ 0 ~]")
 :oh (concat "[~ ~ ~ ~][~ ~ ~ ~][~ ~ 0 ~][~ ~ ~ ~]"))

(make-pattern
 "Let A Woman Be A Woman Let A Man Be A Man"
 'wm
 :bd (concat "[~ ~ 0 ~][~ ~ ~ ~][0 ~ 0 0][~ 0 0 ~]"
             "[~ ~ 0 ~][~ ~ ~ ~][~ ~ 0 ~][~ ~ 0 ~]")
 :sn (concat "[~ ~ ~ ~][0 ~ ~ 0][~ 0 0 ~][0 0 ~ ~]"
             "[~ ~ ~ ~][0 ~ ~ 0][~ 0 ~ 0][0 ~ ~ ~]")
 :ch (concat "[0 ~ 0 ~][0 ~ 0 ~][0 ~ 0 ~][0 ~ ~ ~]")
 :oh (concat "[~ ~ ~ ~][~ ~ ~ ~][~ ~ ~ ~][~ ~ 0 ~]"))

(make-pattern
 "Look Ka Py Py"
 'pypy
 :bd (concat "[0 ~ ~ 0][~ 0 ~ ~][~ ~ 0 ~][~ 0 0 ~]"
             "[0 ~ ~ 0][~ 0 ~ 0][0 ~ 0 ~][~ 0 0 ~]")
 :sn (concat "[~ 0 ~ ~][0 ~ ~ 0][0 ~ 0 ~][~ ~ 0 ~]"
             "[~ 0 ~ ~][0 0 ~ 0][0 ~ 0 ~][~ ~ 0 ~]")
 :ch (concat "[0 ~ 0 ~][0 ~ 0 ~][0 ~ ~ ~][0 ~ 0 ~]")
 :oh (concat "[~ ~ ~ ~][~ ~ ~ ~][~ ~ 0 ~][~ ~ ~ ~]"))

(make-pattern
 "I Got You"
 'igotyou
 :bd (concat "[0 ~ ~ ~][~ ~ ~ ~][~ ~ 0 ~][~ ~ ~ ~]"
             "[~ ~ 0 ~][~ ~ 0 ~][~ ~ 0 ~][~ ~ 0 ~]")
 :sn "[~ ~ ~ ~][0 ~ ~ ~][~ ~ ~ ~][0 ~ ~ ~]"
 :ch "[0 ~ ~ ~][0 ~ 0 ~][0 ~ ~ ~][0 ~ 0 ~]"
 :oh "[~ ~ 0 ~][~ ~ ~ ~][~ ~ 0 ~][~ ~ ~ ~]")

(make-pattern
 "I Got The Feelin"
 'feelin
 :bd (concat "[0 ~ 0 ~][~ ~ ~ ~][~ ~ 0 ~][~ ~ ~ ~]"
             "[~ ~ 0 ~][~ ~ ~ ~][0 ~ ~ ~][0 ~ 0 ~]")
 :sn (concat "[~ ~ ~ ~][~ ~ 0 ~][~ 0 ~ ~][~ ~ 0 ~]"
             "[~ 0 ~ ~][0 0 ~ 0][~ 0 0 0][~ 0 0 0]")
 :ch (concat "[0 ~ 0 ~]"))

(make-pattern
 "Its A New Day"
 'newday
 :bd "[0 ~ 0 ~][~ ~ ~ ~][~ ~ 0 0][~ ~ ~ 0]"
 :sn "[~ ~ ~ ~][0 ~ ~ ~][~ ~ ~ ~][0 ~ ~ ~]"
 :ch "[0 ~ 0 ~]")

(make-pattern
 "Palm Grease"
 'palm
 :bd (concat "[0 ~ ~ ~][~ ~ ~ ~][0 ~ ~ ~][~ ~ ~ 0]"
             "[~ ~ 0 ~][~ ~ ~ ~][~ ~ ~ ~][~ ~ ~ ~]")
 :sn (concat "[~ ~ ~ ~][0 ~ ~ 0][~ 0 ~ ~][0 ~ ~ 0]"
             "[~ 0 ~ ~][~ ~ 0 ~][~ ~ ~ ~][~ ~ 0 ~]")
 :ch (concat "[0 0 0 0][~ 0 0 ~][0 ~ 0 0][~ 0 0 ~]"
             "[0 ~ 0 ~][~ ~ ~ ~][~ ~ ~ ~][~ ~ ~ ~]")
 :oh (concat "[~ ~ ~ ~][~ ~ ~ ~][~ ~ 0 ~][~ ~ ~ ~]"))


(make-pattern
 "Funky President"
 'funkyp
 :bd "[0 ~ ~ 0][~ ~ ~ 0][~ 0 0 ~][~ ~ ~ ~]"
 :sn "[~ ~ ~ ~][0 ~ ~ ~][~ ~ ~ ~][0 ~ ~ ~]"
 :ch "[0 ~ 0 ~][0 ~ 0 ~][0 ~ ~ ~][0 ~ 0 ~]"
 :oh "[~ ~ ~ ~][0 ~ ~ ~][~ ~ 0 ~][~ ~ ~ ~]")

(make-pattern
 "Funky Drummer"
 'funkyd
 :bd (concat "[0 ~ 0 ~][~ ~ ~ ~][~ ~ 0 ~][~ 0 ~ ~]"
             "[0 ~ 0 ~][~ ~ ~ ~][~ ~ 0 ~][~ 0 ~ ~]")
 :sn (concat "[~ ~ ~ ~][0 ~ ~ 0][~ 0 ~ 0][0 ~ ~ 0]"
             "[~ ~ ~ ~][0 ~ ~ 0][~ 0 ~ 0][0 ~ ~ 0]")
 :ch (concat "[0 0 0 0]"))

(make-pattern
 "Galactic"
 'galactic
 :bd "xx-- x-x- xx-- -x--"
 :sn "--x-"
 :ch "xxxx")

;; --------------------------------------------------

;; TODO: track the beat progression to skip ahead on redefinition
;;       add metro?

(defmacro defpattern (name (pattern dur) &body body)
  `(let ((bdp (make-cycle (bd ,pattern)))
         (snp (make-cycle (sn ,pattern)))
         (chp (make-cycle (ch ,pattern)))
         (ohp (make-cycle (oh ,pattern))))
     (defun ,name (time)
       (when (next bdp)
         ,(first body))
       (when (next snp)
         ,(second body))
       (when (next chp)
         ,(third body))
       (when (next ohp)
         ,(fourth body))
       (aat (+ time (* *sample-rate* ,dur)) #',name it))))

(defmacro defbeat (name (pattern dur) &body body)
  `(let ((ppattern
          (cond ((stringp ,pattern)
                 (parse-patternc ,pattern))
                ((listp ,pattern) (make-cycle ,pattern)))))
     (defun ,name (time)
       (when (next ppattern)
         ,@body)
       (aat (+ time (* *sample-rate* ,dur)) #',name it))))

;; (defmacro defbeats (name duration beats notes &body body)
;;   (let ((i-name (intern (format nil "%~a" name)))
;;         (d (intern "D")))
;;     `(let* ((l-beats (coerce ,beats 'list))
;;             (l-beats (substitute #\x #\1 l-beats))
;;             (n-on-beats (loop :for b :in l-beats :count (eq #\x b)))
;;             (onotes  ,notes)
;;             (notes   ,notes)
;;             (notes   (subseq notes 0 (min n-on-beats (length notes))))
;;             (beats   ,beats)
;;             (,d      ,duration))
;;        (defun ,i-name (time lbeats bbeats lnotes lonotes)
;;          (when (not (equal onotes lonotes))
;;            (setf lnotes onotes
;;                  lonotes onotes))
;;          (when (not (equal ,beats bbeats))
;;            (setf bbeats beats
;;                  lbeats (coerce bbeats 'list)))
;;          (cond ((or (eq #\1 (first lbeats))
;;                     (eq #\x (first lbeats)))
;;                 (let ((note (first lnotes)))
;;                   ,@body
;;                   (setf lnotes (alexandria:rotate lnotes -1))))
;;                ((eq #\X (first lbeats))
;;                 (let ((,d (* 2 ,d)))
;;                   ,@body)))
;;          (callback (+ time ,duration) #',i-name
;;                    (+ time ,duration)
;;                    (alexandria:rotate lbeats -1)
;;                    bbeats
;;                    lnotes
;;                    lonotes))
;;        (defun ,name ()
;;          (,i-name (quant 4) l-beats beats notes onotes)))))
