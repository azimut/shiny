(in-package :shiny)

;; TODO:
;; - THere are ways to tell cffi to the allocation/conversion automatically
;; - Make an instrument library abstraction, so I can push an ORC with a custom
;;   set of instruments...nvm, tables are to thightly related to instruments
;;   combine or compose them won't be that easy...
;; - Make a parser for orcs, initially to get the signature of the insts, like
;;  if they need extra arguments or not. And create the classes accordingly
;; - Synths bring to the table the fact that I don't do much over time changes
;; - Then parse the instrument part too might be to get good default values
;; - But like baggers said "when you get a string in lisp is like it insults you"
;; - Aaaaaaand test ORCAsync

;; Copy csound's interfaces/csound.{lisp,asd} into
;; ~/.quicklisp/local-projects/csound/ then
;; I also re-defined :string instead of :pointer for csound:csoundreadscore and :orcproc

(ql:quickload :csound)

(defvar *csound-globals*
  ";; Initialize the global variables.
   sr = 44100
   kr = 4410
   ksmps = 10
   nchnls = 2")
(defvar *c* nil)
(defvar *orcs* (make-hash-table))

;; Assumes only that "i1 0 10" will be the only constant params
(defclass cinstrument ()
  ((iname  :initarg :iname)
   (params :initarg :params :reader params)))

;; FIXME: meh, needs to be a macro (?
(defmethod print-object ((obj cinstrument) out)
  (print-unreadable-object (obj out :type t)
    (format out "~s" (params obj))))

(defgeneric playcsound (instrument duration &rest rest))
(defmethod playcsound
    ((i cinstrument) (duration number) &rest rest)
  "plays a single note with INSTRUMENT"
  (when (> duration 0)
    (with-slots (iname) i
      ;; finally send only the parameter values
      (let ((vars-only (remove-if #'keywordp rest)))
        (csound:csoundreadscore
         *c* 
         (format nil "~{~A~^ ~}"
                 (append (list iname 0 duration) vars-only)))))))

;; (defmethod playcsound
;;     ((i cinstrument) (keynum list) (duration number) &rest rest)
;;   "plays chord of KEYNUM list
;;    (playcsound *ins* '(60 62 64) 1 2.0 .02)"
;;   (with-slots (extra iname) i
;;     (let ((pchs (mapcar (lambda (x) (incudine:keynum->pch x)) keynum)))
;;       (mapcar
;;        (lambda (x)
;;          (csound:csoundreadscore
;;           *c*
;;           (format nil "~{~A~^ ~}"
;;                   (append (list iname 0 duration p4 x)
;;                           rest))))
;;        pchs))))

(defmacro make-play (name i &rest rest)
  (let ((fname (intern (format nil "~A-~A" 'play name)))
        (keynum-exists (position :keynum rest)))
    `(let ((c (make-instance 'cinstrument :iname ,i :params ',rest)))
       ,(if keynum-exists
            ;; Handles normal instrumentes with a single keynum that defines the
            ;; instrument pitch in pch
           `(defun ,fname (keynum duration &key ,@(remove-if
                                                  #'null
                                                  (loop :for (x y) :on rest :by #'cddr
                                                     :collect
                                                     (let* ((sn (symbol-name x))
                                                            (k  (intern sn)))
                                                       (when (not (eq :keynum x))
                                                         (list k y))))))
             (playcsound c duration
                         ,@(loop :for (k v) :on rest :by #'cddr :append
                              (if (eq :keynum k)
                                  (list k '(incudine:keynum->pch keynum))
                                  (list k (intern (symbol-name k)))))))
           ;; Handles instruments without explicit frequency or without frequency
           ;; in pch terms
           `(defun ,fname (duration &key ,@(remove-if
                                           #'null
                                           (loop :for (x y) :on rest :by #'cddr
                                              :collect
                                              (let* ((sn (symbol-name x))
                                                     (k  (intern sn)))
                                                (list k y)))))
             (playcsound c duration
                         ,@(loop :for (k v) :on rest :by #'cddr :append
                              (if (eq :keynum1 k)
                                  (list k '(incudine:keynum->pch keynum))
                                  (list k (intern (symbol-name k)))))))))))

(defclass orc ()
  ((name  :initarg :name)
   (orc   :initarg :orc)
   (table :initarg :table)))

(defun make-orc (name &key table orc)
  (declare (symbol name))
  (let ((o (make-instance 'orc :name name :table table :orc orc)))
    (setf (gethash name *orcs*) o)))

(defun list-orcs ()
  (alexandria:hash-table-keys *orcs*))

;; (set-csound-options '("-odac" "--nchnls=2" "-+rtaudio=jack" "-M0" "-b128" "-B1048" "-+rtmidi=null"))
(defun set-csound-options (&optional (options '("-odac" "--nchnls=2")))
  (declare (list options))
  (loop :for option :in options :when (stringp option) :do
     (cffi:with-foreign-string (coption option)
       (csound:csoundsetoption *c* coption))))

(defun get-orc (orc-name)
  (assert (keywordp orc-name))
  (with-slots (orc) (gethash orc-name *orcs*)
    (if orc
        orc
        (let* ((n (format nil "lib/csound/~a.orc" (symbol-name orc-name)))
               (f (asdf:system-relative-pathname :shiny n))
               (orc (alexandria:read-file-into-string f)))
          orc))))

(defun get-table (orc-name)
  (assert (keywordp orc-name))
  (slot-value (gethash orc-name *orcs*) 'table))

;; TODO: ew
(defun start-csound (orchestra)
  (declare (type orc orchestra))
  (unless *c*
    (with-slots (name table) orchestra
      (csound:csoundinitialize 3)
       (setf *c* (csound:csoundcreate (cffi:null-pointer)))
      (set-csound-options '("-odac" "--nchnls=2" "-M0" "-+rtmidi=null"))
      (csound:csoundcompileorc *c* (get-orc name))
      (csound:csoundstart *c*)
      (csound:csoundreadscore *c* *csound-globals*)
      (csound:csoundreadscore *c* table))))

;;--------------------------------------------------
;; TODO: xml/html parser to put all on a .csd
;; Partial definition of the ORCs, see lib/csound/*.orc for the source

(make-orc :xanadu
          :table "
f1 0 8192 10 1
f2 0 8192 11 1
f3 0 8192 -12 20.0")

(make-orc :trapped
          :table "
f1   0  8192  10   1
f2   0  512   10   10  8   0   6   0   4   0   1
f3   0  512   10   10  0   5   5   0   4   3   0   1
f4   0  2048  10   10  0   9   0   0   8   0   7   0  4  0  2  0  1
f5   0  2048  10   5   3   2   1   0
f6   0  2048  10   8   10  7   4   3   1
f7   0  2048  10   7   9   11  4   2   0   1   1
f8   0  2048  10   0   0   0   0   7   0   0   0   0  2  0  0  0  1  1
f9   0  2048  10   10  9   8   7   6   5   4   3   2  1
f10  0  2048  10   10  0   9   0   8   0   7   0   6  0  5
f11  0  2048  10   10  10  9   0   0   0   3   2   0  0  1
f12  0  2048  10   10  0   0   0   5   0   0   0   0  0  3
f13  0  2048  10   10  0   0   0   0   3   1
f14  0  512   9    1   3   0   3   1   0   9  .333   180
f15  0  8192  9    1   1   90
f16  0  2048  9    1   3   0   3   1   0   6   1   0
f17  0  9     5   .1   8   1
f18  0  17    5   .1   10  1   6  .4
f19  0  16    2    1   7   10  7   6   5   4   2   1   1  1  1  1  1  1  1
f20  0  16   -2    0   30  40  45  50  40  30  20  10  5  4  3  2  1  0  0  0
f21  0  16   -2    0   20  15  10  9   8   7   6   5   4  3  2  1  0  0
f22  0  9    -2   .001 .004 .007 .003 .002 .005 .009 .006")

(make-orc :kkel
          :table "
igrainrate	= p4		; grain rate
igrainsize	= p5		; grain size in ms
igrainFreq	= p6		; frequency of source wave within grain
iosc2Dev	= p7		; partikkel instance 2 grain rate deviation factor
iMaxSync	= p8		; max soft sync amount (increasing to this value during length of note)")

(make-orc :asynth
          :table "
 f1 0 8192 10 1                         ; Sine
 f2 0 4096 7 0 1024 1 2048 -1 1024 0    ; Triangle
 f3 0 1024 7 1 512 1 0 -1 512 -1        ; Square
 f4 0 4096 -7 0 1024 .5 2048 -.5 1024 0 ; Half Level Tri for Shallow PWM
 f5 0 1024 7 0 1024 0                   ; Constant 0 for No PWM/Sqr/Tri")

(make-orc
 :bass
 :table "
f1 0 8192 10 1
; sine wave
f3 0 8193   8  0 512 1 1024 1 512 .5 2048 .2 4096  0
; accent curve
f4 0   16  -2  12 24 12 14 15 12 0 12 12 24 12 14 15 6 13 16
; sequencer (pitches are 6.00 + p/100) 
f5 0   32  -2   0  1  0  0  0  0 0  0  0  1  0  1  1 1  0  0 0 1 0 0 1 0 1 1 1 1 0 0 0 0 0 1
; accent sequence
f6 0   16  -2   2     1  1  2    1  1  1  2     1  1 3       1 4 0 0 0
; fill with zeroes till next power of 2
 
 ; f6 = durations of events, 1 = note per note, 2 = two tied notes... .
 ; note: f4-f5-f6 don´t need to be syncronized... like here (16-32-21)
 
 f7 0 1024   8 -.8 42 -.78  200 -.74 200 -.7 140 .7  200 .74 200 .78 42 .8 ; distortion table
 ; f7 borrowed from H.Mikelson´s TB-303 emulator.")

(make-orc :drumkit
          :table "
; wave tables
;low res works well!
f1 0 32 10 1
f2 0 8192 10 1

;envelopes
f10 0 2048 7 0 20 1 1000 0
f11 0 2048 7 0 10 1 500 0
f12 0 2048 7 0 40 1 1200 0
f13 0 2048 7 0 10 1 700 0
f14 0 2048 5 .01 3 1 445 .01 1400 .01
f15 0 2048 5 .01 10 1 848 .01

;for gliss
f20 0 1024 -5 10 650 1 724 1
f21 0 1024 -5 6 350 1 724 1

;rhythm
f30 0 16 -2 1 0 0 .5 1 0 1 0 1 0 0 .5 1 0 1 .5
f32 0 16 2 1 0 1 0 1 0 1 0 1 1 0 1 0 1 0 1
f31 0 32 -2 1 0 0 0 1 0 0 .5 1 0 0 0 1 0 0 .5 1 0 0 1 0 0 1 0 0 1 0 0 1 0 0 .5
f33 0 16 2 0 0 1 0 0 1 0 1 0 0 1 0 0 1 1 0
f34 0 4 2 1 0 1 0 ; 0 0 1 0
f35 0 16 2 0 1 1 0 0 1 1 1 0 1 1 0 1 1 1 1
f36 0 16 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1

f111 0 2048 5 .01 10 1 1448 .01
f112 0 2048 5 .01 10 1 1500 .01
f113 0 2048 5 .01 10 1 1900 .01
f114 0 2048 5 .01 10 1 1600 .01
f115 0 2048 5 .01 10 1 1400 .01
f116 0 2048 5 .01 10 1 1200 .01
f117 0 2048 5 .01 10 1 1000 .01
f118 0 2048 5 .01 1 1 200 .01")
