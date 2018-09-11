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
;; - CLOS object for the server: thread, status, reboot, load methods there

;; Copy csound's interfaces/csound.{lisp,asd} into
;; ~/.quicklisp/local-projects/csound/ then
;; I also re-defined :string instead of :pointer for csound:csoundreadscore and :orcproc

;;(ql:quickload :csound)

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
;;   (mapcar
;;    (lambda (x)
;;      (playcsound i x ))
;;    keynum))

(defmacro make-play (name i &rest rest)
  (assert (and (symbolp name) (not (keywordp name))))
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
  ((name :initarg :name)
   (orc  :initarg :orc)
   (sco  :initarg :sco)))

(defun parse-sco (s)
  "returns only the fN wavetables on the sco"
  (declare (string s))
  (format nil
          "~{~A~% ~}"
          (cl-ppcre:all-matches-as-strings "f\\d+.*" s)))

(defun parse-orc (s)
  "returns the orc, changes mono to stereo"
  (declare (string s))
  (if (cl-ppcre:scan "soundin" s)
      (error "soundin required"))
  (if (cl-ppcre:scan "nchnls\\s+=\\s+1" s)
      ;; FIXME: flaky
      (cl-ppcre:regex-replace " out\\s+\(.+\)" s "outs \\1,\\1")
      s))

;; NOTE: before running this try the sound on the CLI with:
;; $ csound -odac 326a.{orc,sco}
(defun make-orc (name &key sco orc filename filepath orc-path sco-path)
  "This function creates a new orchestra file, reading score wave tables too"
  (assert (keywordp name))
  ;; Find files if parameters provided
  (when filename
    (if (not filepath)
        (setf filepath (asdf:system-relative-pathname :shiny "lib/csound/")))
    (setf orc-path (merge-pathnames filepath (concatenate 'string filename ".orc")))
    (setf sco-path (merge-pathnames filepath (concatenate 'string filename ".sco")))
    (assert (and (uiop:file-exists-p sco-path) (uiop:file-exists-p orc-path))))
  ;; Read into vars
  (when (and orc-path sco-path)
    (setf orc (parse-orc (alexandria:read-file-into-string orc-path)))
    (setf sco (parse-sco (alexandria:read-file-into-string sco-path))))
  (setf (gethash name *orcs*)
        (make-instance 'orc
                       :name name
                       :sco sco
                       :orc orc)))

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

(defun get-sco (orc-name)
  (assert (keywordp orc-name))
  (slot-value (gethash orc-name *orcs*) 'sco))

;; TODO: ew
(defun start-csound (orchestra)
  (declare (type orc orchestra))
  (unless *c*
    (with-slots (name sco) orchestra
      ;; Set headless flags
      (csound:csoundinitialize 3)
      (setf *c* (csound:csoundcreate (cffi:null-pointer)))
      (set-csound-options '("-odac" "--nchnls=2" "-M0" "-+rtmidi=null"))
      ;; Initialize ORC 
      (csound:csoundcompileorc *c* (get-orc name))
      ;; GOGOGO!
      (csound:csoundstart *c*)
      ;; Global vars init
      (csound:csoundreadscore *c* *csound-globals*)
      ;; Init fN wave tables for this ORC
      (csound:csoundreadscore *c* sco))))

;;--------------------------------------------------
;; TODO: xml/html parser to put all on a .csd
;; Partial definition of the ORCs, see lib/csound/*.orc for the source

(make-orc :xanadu  :filename "XANADU")
(make-orc :trapped :filename "TRAPPED")
(make-orc :kkel    :filename "KKEL")
(make-orc :asynth  :filename "ASYNTH")
(make-orc :bass    :filename "BASS")
(make-orc :drumkit :filename "DRUMKIT")
(make-orc :drumkit :filename "DRUMKIT")

