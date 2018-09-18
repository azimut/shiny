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
(defvar *csound-options* '("-odac" "--nchnls=2" "-M0" "-+rtmidi=null"))
(defvar *c* nil)
(defvar *orcs* (make-hash-table))

(defgeneric playcsound (instrument duration &rest rest))
(defmethod playcsound
    ((iname string) (duration number) &rest rest)
  "plays a single note with INSTRUMENT"
  (when (> duration 0)
    ;; finally send only the parameter values
    (let ((vars-only (remove-if #'keywordp rest)))
      (csound:csoundreadscore
       *c*
       (format nil "~a 0 ~a ~{~A~^ ~}"
               iname duration vars-only)))))

(defgeneric playcsound-key (instrument duration keynum &rest rest))
(defmethod playcsound-key
    ((iname string) (duration number) (keynum integer) &rest rest)
  "plays a single note with INSTRUMENT"
  (when (and (> duration 0) (> keynum 0))
    (let ((kpos (1+ (position :keynum rest))))
      (setf (nth kpos rest)
            (keynum->pch keynum)))
    ;; finally send only the parameter values
    (let ((vars-only (remove-if #'keywordp rest)))
      (csound:csoundreadscore
       *c* 
       (format nil "~a 0 ~a ~{~A~^ ~}"
               iname duration vars-only)))))

(defmethod playcsound-key
    ((iname string) (duration number) (keynum list) &rest rest)
  "plays a single note with INSTRUMENT"
  (let ((kpos (1+ (position :keynum rest))))
    (mapcar
     (lambda (keynum)
       (when (and (> duration 0) (> keynum 0))
         (setf (nth kpos rest) (keynum->pch keynum))
         ;; finally send only the parameter values
         (let ((vars-only (remove-if #'keywordp rest)))
           (csound:csoundreadscore
            *c* 
            (format nil "~a 0 ~a ~{~A~^ ~}"
                    iname duration vars-only)))))
     keynum)))

(defmacro make-play (name i &rest rest)
  (assert (and (symbolp name) (not (keywordp name))))
  (let ((fname (intern (format nil "~A-~A" 'play name)))
        (keynum-exists (position :keynum rest)))
    (if keynum-exists
        ;; Handles normal instrumentes with a single keynum
        `(defun ,fname (keynum duration &key ,@(remove-if
                                                #'null
                                                (loop :for (x y) :on rest :by #'cddr
                                                   :collect
                                                   (let* ((sn (symbol-name x))
                                                          (k  (intern sn)))
                                                     (when (not (eq :keynum x))
                                                       (list k y))))))
           (playcsound-key ,i duration keynum
                           ,@(loop :for (k v) :on rest :by #'cddr :append
                                (if (eq k :keynum)
                                    (list k 127)
                                    (list k (intern (symbol-name k)))))))
        ;; Handles instruments without keynum
        `(defun ,fname (duration &key ,@(loop :for (x y) :on rest :by #'cddr
                                           :collect
                                           (let* ((sn (symbol-name x))
                                                  (k  (intern sn)))
                                             (list k y))))
           (playcsound ,i duration
                       ,@(loop :for (k v) :on rest :by #'cddr :append
                            (list k (intern (symbol-name k)))))))))

(defclass orc ()
  ((name :initarg :name)
   (orc  :initarg :orc :reader orc)
   (sco  :initarg :sco :reader sco)))

(defun parse-sco (score)
  "returns only the fN wavetables on the score, remove comments and spaces and
   zeros from fN wavetable definitions"
  (declare (string score))
  (let* ((score (cl-ppcre:regex-replace-all ";.*" score ""))
         (score (cl-ppcre:regex-replace-all
                 "f[ ]+\(\\d+\)"
                 score
                 "f\\1"))
         (score (cl-ppcre:regex-replace-all
                 "f0+\(\\d+\)"
                 score
                 "f\\1"))
         (score (format
                 nil
                 "~{~A~% ~}"
                 (cl-ppcre:all-matches-as-strings "f\\d+ .*" score))))
    score))

(defun parse-orc (s)
  "returns the orc, changes mono to stereo, remove comments"
  (declare (string s))
  (let ((orc (cl-ppcre:regex-replace-all ";.*" s "")))
    (if (cl-ppcre:scan "soundin" orc)
        (error "soundin required"))
    (if (cl-ppcre:scan "nchnls\\s+=\\s+1" orc)
        (cl-ppcre:regex-replace " out\\s+\([^\\s]+\)" orc "outs \\1,\\1")
        orc)))

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

(defun get-orchestra (orc-name)
  (assert (keywordp orc-name))
  (gethash orc-name *orcs*))

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
(defun make-csound (orchestra)
  (declare (type orc orchestra))
  (unless *c*
    (with-slots (name) orchestra
      ;; Set headless flags
      (csound:csoundinitialize 3)
      (setf *c* (csound:csoundcreate (cffi:null-pointer)))
      (set-csound-options *csound-options*)
      ;; Initialize ORC 
      (csound:csoundcompileorc *c* (get-orc name))
      ;; GOGOGO!
      (csound:csoundstart *c*)
      ;; Global vars init
      (csound:csoundreadscore *c* *csound-globals*)
      ;; Init fN wave tables for this ORC
      (csound:csoundreadscore *c* (get-sco name)))))


;;--------------------------------------------------
;; Merge utils

(defun regex-count (regex s)
  (declare (string regex s))
  (let ((matches (length (cl-ppcre:all-matches regex s))))
    (when matches
      (/ matches 2))))

(defun get-fn (score)
  "returns a list of string of integers with each match of the form Fn,
   where n is a number"
  (declare (string score))
  (let* ((l (cl-ppcre:all-matches-as-strings
                 "f\\d+"
                 score))
         (l (mapcar
             (lambda (x) (subseq x 1))
             l)))
    l))

(defun replace-wavetables (orc wavetables-hash)
  (let ((indexes (alexandria:hash-table-keys wavetables-hash)))
    (print indexes)
    (loop :for index :in indexes :do
       (let ((r (format nil "~a~a~a" "\\{1}" (gethash index wavetables-hash) "\\2")))
         (setf orc (cl-ppcre:regex-replace-all
                    "\(oscil\\s+[^,]+,[^,]+,\)[^,]+\(.*\)"
                    orc
                    r))
         (setf orc (cl-ppcre:regex-replace-all
                    "\(oscili\\s+[^,]+,[^,]+,\)[^,]+\(.*\)"
                    orc
                    r))
         (setf orc (cl-ppcre:regex-replace-all
                    "\(table\\s+[^,]+,\)[^,]+\(.*\)"
                    orc
                    r))
         (setf orc (cl-ppcre:regex-replace-all
                    "\(tablei\\s+[^,]+,\)[^,]+\(.*\)"
                    orc
                    r))))
    orc))

(defun merge-orcs (&rest orchestras)
  (let ((n-instruments 0)
        (instruments)
        (n-wavetables 0)
        (wavetables)
        (wavetables-hash (make-hash-table :test #'equal)))
    (loop
       :for orchestra :in orchestras
       :do
       (with-slots (orc sco) orchestra
         ;; SCO
         (loop
            :for f :in (get-fn sco)
            :for fn :from n-wavetables
            :with temp-wavetable = sco
            :finally (setf wavetables
                           (concatenate 'string
                                        wavetables
                                        temp-wavetable))
            :do
            (setf (gethash f wavetables-hash) fn)
            (setf temp-wavetable (cl-ppcre:regex-replace
                                  (format nil "~a~a" "f" f)
                                  temp-wavetable
                                  (format nil "f~d" fn)))
            (incf n-wavetables))
         ;; ORC
         (setf orc (replace-wavetables orc wavetables-hash))
         (clrhash wavetables-hash)
         (setf instruments (concatenate 'string instruments orc))
         (incf n-instruments (regex-count "instr\\s+\\d+" orc))))
    ;; ORC: Template instruments
    (setf instruments (cl-ppcre:regex-replace-all "instr\\s+\\d+"
                                                  instruments
                                                  "instr ~a"))
    ;; ORC: New Instruments numbers
    ;; FIXME: Ensure we support these many instruments
    (assert (< n-instruments 10))    
    (setf instruments (format nil instruments 1 2 3 4 5 6 7 8 9 10))
    ;; RETURN both a new ORC and SCO
    (values instruments
            wavetables)))

;;--------------------------------------------------
;; TODO: xml/html parser to put all on a .csd
;; Partial definition of the ORCs, see lib/csound/*.orc for the source

(make-orc :xanadu  :filename "XANADU")
(make-orc :trapped :filename "TRAPPED")
(make-orc :kkel    :filename "KKEL")
(make-orc :asynth  :filename "ASYNTH")
(make-orc :bass    :filename "BASS")
(make-orc :drumkit :filename "DRUMKIT")


