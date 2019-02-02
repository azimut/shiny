(in-package :shiny)

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

(defgeneric parse-pattern (pattern))
(defmethod parse-pattern (pattern))
(defmethod parse-pattern ((pattern string))
  (loop
     :for c
     :across (string-downcase pattern)
     :when (or (eq c #\1) (eq c #\x) (eq c #\0)
               (eq c #\~) (eq c #\-))
     :collect
     (if (or (eq c #\x)
             (eq c #\0)
             (eq c #\1))
         t
         nil)))

(defmethod parse-pattern ((pattern list))
  (loop :for beat :in pattern :collect
     (if (= 0 beat) nil t)))

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

(defun list-patterns ()
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

(defmacro defpattern (name (patterns dur) &body body)
  "takes a list of patterns in the x-x-x- format OR as a list of NIL and T
   and the same number of functions in BODY to play at beat x/T

  (defpattern k ((\"x---\" \"xxxx\") .4)
     (p time 60 60 1 0))
  (defpattern k (((T NIL NIL NIL) (TTTT)) .4)
     (p time 60 60 1 0))"
  (let ((lets (loop :for pattern :in patterns :collect
                   `(,(gensym) (make-cycle (if (stringp ,pattern)
                                               (parse-pattern ,pattern)
                                               ,pattern))))))
    `(let (,@lets
           (d  ,dur))
       (defun ,name (time)
         ,@(loop
              :for l :in lets
              :for b :in body
              :collect
                `(when (next ,(car l))
                   ,b))
         (aat (+ time (calc-beats ,dur))
              #',name it)))))

(defmacro defbeat (name (pattern dur) &body body)
  "Defines a function that can be used to play ONE pattern.
   From list of NIL and T or a string like \"x--x\".

   > (defbeat kick ((bd (get-pattern (pickl (list-patterns)))) .5)
       (p time 60 60 .2 0))
   > (kick (now))"
  `(let ((d ,dur)
         (ppattern
          (cond
            ((stringp ,pattern) (parse-patternc ,pattern))
            ((listp   ,pattern) (make-cycle ,pattern)))))
     (defun ,name (time)
       (when (next ppattern)
         ,@body)
       (aat (+ time (calc-beats ,dur))
            #',name it))))
