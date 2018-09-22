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
;; - I get a freeze state (? when doing (reset), might be due stop handling sigs
;; - Support "<" ">" "+" on scores...whatever that is...
;; - score debug helper to send messages
;; - debug more to show messages send
;; - seems like some instruments only work with determined global values of:
;;   sr,kr,ksmps,chnls...great!...
;; - add panning parameters when converting from mono to stereo (or always)
;; - everything crashes with an invalid float point operation when I do weird
;;   things with (some) params, appears to happen on the perform thread
;; - boy oh boy...might be this is why it never got popular...

;; Usage:
;; Copy csound's interfaces/csound.{lisp,asd} into
;; ~/.quicklisp/local-projects/csound/ then
;; I also re-defined :string instead of :pointer for csound:csoundreadscore and :orcproc

;;(ql:quickload :csound)

(defvar *c* nil)
(defvar *csound-globals*
  ";; Initialize the global variables.
   sr = 44100
   kr = 4410
   ksmps = 10
   nchnls = 2")
;; JACK?
;;(defparameter *csound-options* '("-odac" "--nchnls=2" "-+rtaudio=jack" "-M0" "-b128" "-B1048" "-+rtmidi=null" "--sample-rate=44100"))
(defparameter *csound-options* '("-odac" "--nchnls=2" "-+rtmidi=null"))
(defvar *orcs* (make-hash-table))
(defclass orc ()
  ((name    :initarg :name)
   (globals :initarg :globals :reader globals)
   (orc     :initarg :orc :reader orc)
   (sco     :initarg :sco :reader sco)
   (file    :initarg :file)))
(defclass csound-server ()
  ((thread)
   (server)))
(defvar *tmporc* NIL)
(defvar *tmppartialorc* NIL)
(defvar *thread* NIL)

(defun stich (&rest rest)
  (format nil "~{~%~a~%~}" (remove-if #'null rest)))

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

(defgeneric playcsound-freq (instrument duration keynum &rest rest))
(defmethod playcsound-freq
    ((iname string) (duration number) (keynum integer) &rest rest)
  "plays a single note with INSTRUMENT"
  (when (and (> duration 0) (> keynum 0))
    (let ((kpos (1+ (position :freq rest))))
      (setf (nth kpos rest)
            (midihz keynum)))
    ;; finally send only the parameter values
    (let ((vars-only (remove-if #'keywordp rest)))
      (csound:csoundreadscore
       *c* 
       (format nil "~a 0 ~a ~{~A~^ ~}"
               iname duration vars-only)))))
(defmethod playcsound-freq
    ((iname string) (duration number) (keynum list) &rest rest)
  "plays a single note with INSTRUMENT"
  (let ((kpos (1+ (position :freq rest))))
    (mapcar
     (lambda (keynum)
       (when (and (> duration 0) (> keynum 0))
         (setf (nth kpos rest) (midihz keynum))
         ;; finally send only the parameter values
         (let ((vars-only (remove-if #'keywordp rest)))
           (csound:csoundreadscore
            *c* 
            (format nil "~a 0 ~a ~{~A~^ ~}"
                    iname duration vars-only)))))
     keynum)))

(defgeneric playcsound-key (instrument duration keynum &rest rest))
(defmethod playcsound-key
    ((iname string) (duration number) (keynum integer) &rest rest)
  "plays a single note with INSTRUMENT"
  (when (and (not (= duration 0)) (> keynum 0))
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
  "this macro will create a new (play-NAME) function wrapper of either
   playcsound or playcsound-key, with each &key defined on the function"
  (assert (and (symbolp name) (not (keywordp name))))
  (let ((fname (intern (format nil "~A-~A" 'play name)))
        (fnamearp (intern (format nil "~A-~A-~A" 'play name 'arp))))
    (cond ((position :keynum rest)
           ;; Handles normal instrumentes with a single note in PCH
           ;; A function for notes and chords, and other for arpeggios
           `(progn
             (defun ,fname (keynum duration &key ,@(remove-if
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
                                        (list k 127) ;; dummy value...
                                        (list k (intern (symbol-name k)))))))
             (defun ,fnamearp (keynums offset &key ,@(remove-if
                                                    #'null
                                                    (loop :for (x y) :on rest :by #'cddr
                                                       :collect
                                                       (let* ((sn (symbol-name x))
                                                              (k  (intern sn)))
                                                         (when (not (eq :keynum x))
                                                           (list k y))))))
               (loop
                  :for keynum :in (cdr keynums)
                  :with i = 0
                  :initially (,fname (car keynums) offset
                                     ,@(remove-if
                                        #'null
                                        (loop :for (x y) :on rest :by #'cddr
                                           :append
                                           (let* ((sn (symbol-name x))
                                                  (k  (intern sn)))
                                             (when (not (eq :keynum x))
                                               (list x k))))))
                  :collect
                  (progn
                    (incf i offset)
                    (at (+ (now) (* *SAMPLE-RATE* (* (SAMPLE I) (SPB *TEMPO*))))
                        #',fname keynum offset
                        ,@(remove-if
                                       #'null
                                       (loop :for (x y) :on rest :by #'cddr
                                          :append
                                          (let* ((sn (symbol-name x))
                                                 (k  (intern sn)))
                                            (when (not (eq :keynum x))
                                              (list x k)))))))))))
          ;; Handles normal instruments with a single note in Hz
          ((position :freq rest)
           `(progn
              (defun ,fname (keynum duration &key ,@(remove-if
                                                     #'null
                                                     (loop :for (x y) :on rest :by #'cddr
                                                        :collect
                                                        (let* ((sn (symbol-name x))
                                                               (k  (intern sn)))
                                                          (when (not (eq :freq x))
                                                            (list k y))))))
                (playcsound-freq ,i duration keynum
                                 ,@(loop :for (k v) :on rest :by #'cddr :append
                                      (if (eq k :freq)
                                          (list k 440) ;; dummy value...
                                          (list k (intern (symbol-name k)))))))
              (defun ,fnamearp (keynums offset &key ,@(remove-if
                                                    #'null
                                                    (loop :for (x y) :on rest :by #'cddr
                                                       :collect
                                                       (let* ((sn (symbol-name x))
                                                              (k  (intern sn)))
                                                         (when (not (eq :freq x))
                                                           (list k y))))))
               (loop
                  :for keynum :in (cdr keynums)
                  :with i = 0
                  :initially (,fname (car keynums) offset
                                     ,@(remove-if
                                        #'null
                                        (loop :for (x y) :on rest :by #'cddr
                                           :append
                                           (let* ((sn (symbol-name x))
                                                  (k  (intern sn)))
                                             (when (not (eq :freq x))
                                               (list x k))))))
                  :collect
                  (progn
                    (incf i offset)
                    (at (+ (now) (* *SAMPLE-RATE* (* (SAMPLE I) (SPB *TEMPO*))))
                        #',fname keynum offset
                        ,@(remove-if
                                       #'null
                                       (loop :for (x y) :on rest :by #'cddr
                                          :append
                                          (let* ((sn (symbol-name x))
                                                 (k  (intern sn)))
                                            (when (not (eq :freq x))
                                              (list x k)))))))))))
          (t 
           ;; Handles instruments without keynum
           `(defun ,fname (duration &key ,@(loop :for (x y) :on rest :by #'cddr
                                              :collect
                                              (let* ((sn (symbol-name x))
                                                     (k  (intern sn)))
                                                (list k y))))
              (playcsound ,i duration
                          ,@(loop :for (k v) :on rest :by #'cddr :append
                               (list k (intern (symbol-name k))))))))))

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
  (let* ((orc (cl-ppcre:regex-replace-all ";.*" s ""))
         (soundin-p (cl-ppcre:scan "soundin" orc))
         (mono-p (cl-ppcre:scan "nchnls\\s*=\\s*1" orc))
         (start-instr (cl-ppcre:scan "instr\\s+\\d+" orc))
         (orc (subseq orc start-instr)))
    (when soundin-p
      (error "soundin required"))
    (when mono-p
      (let* ((inst-pos (cl-ppcre:all-matches "instr\\s+\\d+" orc))
             (inst-pos (loop for i in inst-pos by #'cddr collect i)))
        (setf
         orc
         (format
          nil "~{~a~%~}"
          (loop
             :for (start end) :on inst-pos
             :collect
             (let* ((instr (subseq orc start end))
                    (pos (cl-ppcre:all-matches-as-strings "p\\d+" instr))
                    (pos (mapcar (lambda (x) (parse-integer (subseq x 1))) pos))
                    (pos (sort pos #'>))
                    (last-pos (car pos)))
               (cl-ppcre:regex-replace
                " out\\s+\(.*\)"
                instr
                (format nil "outs (\\{1})*p~d,(\\{1})*p~d"
                        (+ 1 last-pos)
                        (+ 2 last-pos)))))))))
    orc))

(defun parse-globals (s)
  (declare (string s))
  (let* ((start-instr (cl-ppcre:scan "instr\\s+\\d+" s))
         (globals (subseq s 0 start-instr)))
    globals))

;; NOTE: before running this try the sound on the CLI with:
;; $ csound -odac 326a.{orc,sco}
(defun make-orc (name &key sco orc globals filename filepath orc-path sco-path)
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
    (setf globals (parse-globals (alexandria:read-file-into-string orc-path)))
    (setf sco (parse-sco (alexandria:read-file-into-string sco-path))))
  (setf (gethash name *orcs*)
        (make-instance 'orc
                       :name name
                       :globals globals
                       :file filepath
                       :sco sco
                       :orc orc)))

(defun list-orcs ()
  (alexandria:hash-table-keys *orcs*))

(defun set-csound-options (&optional (options *csound-options*))
  (declare (list options))
  (loop :for option :in options :when (stringp option) :do
     (cffi:with-foreign-string (coption option)
       (csound:csoundsetoption *c* coption))))

(defun get-orchestra (orc-name &optional n-instr)
  (assert (keywordp orc-name))
  (if n-instr
      (setf *tmppartialorc*
            (make-instance 'orc
                           :sco (get-sco orc-name)
                           :orc (get-orc orc-name n-instr)
                           :globals (get-globals orc-name)
                           :name "slice"))
      (gethash orc-name *orcs*)))

(defun get-orc (orc-name &optional n-instr)
  (assert (keywordp orc-name))
  (let ((result))
    (with-slots (orc) (gethash orc-name *orcs*)
      ;; Load it from Disk
      (if orc
          (setf result orc)
          (let* ((n (format nil "lib/csound/~a.orc" (symbol-name orc-name)))
                 (f (asdf:system-relative-pathname :shiny n)))
            (setf result (alexandria:read-file-into-string f))))
      ;; Pick N-TH: not even sure if this is a good idea, but I "need" it
      (when n-instr
        (let* ((i-list (cl-ppcre:split "\(instr\\s+\\d+\)" orc
                                       :with-registers-p t
                                       :omit-unmatched-p t))
               ;; remove empty strings
               (i-list (remove-if (lambda (x) (= (length x) 0)) i-list)))
          (setf result
                (nth n-instr
                     (loop
                        :for (x y) :on i-list
                        :by #'cddr
                        :collect (stich x y))))))
      result)))

(defun get-sco (orc-name)
  (assert (keywordp orc-name))
  (with-slots (sco) (gethash orc-name *orcs*)
    (if sco
        sco
        (let* ((n (format nil "lib/csound/~a.sco" (symbol-name orc-name)))
               (f (asdf:system-relative-pathname :shiny n))
               (sco (alexandria:read-file-into-string f)))
          sco))))

(defun get-globals (orc-name &optional filter-globals)
  (assert (keywordp orc-name))
  (with-slots (globals) (gethash orc-name *orcs*)
    (if filter-globals
        (loop
           :for default :in '("sr" "kr" "ksmps" "nchnls")
           :finally (return result)
           :with result = globals :do
           (setf result (cl-ppcre:regex-replace-all
                          (format nil "\\s*~a\\s*=\\s*.*\\n" default)
                          result (format nil "~%"))))
        globals)))

;; TODO: ew
(defun start-csound (orchestra)
  (declare (type orc orchestra))
  (with-slots (name orc sco globals) orchestra
    (let ((server-up-p *c*))
      (unless server-up-p
        ;; Set headless flags
        (csound:csoundinitialize 3)
        (setf *c* (csound:csoundcreate (cffi:null-pointer)))
        (set-csound-options *csound-options*))
      ;; Initialize ORC 
      (csound:csoundcompileorc
       *c*
       (stich *csound-globals* globals orc))
      ;; GOGOGO!
      (unless server-up-p
        (csound:csoundstart *c*))
      ;; Init fN wave tables for this ORC
      (csound:csoundreadscore *c* sco))))

(defun load-csound (orchestra)
  "one can load sco and orc without stop the perfomance thread
   or restart the server"
  (declare (type orc orchestra))
  (with-slots (orc sco globals) orchestra
    ;; Initialize ORC 
    (csound:csoundcompileorc *c* (stich globals orc))
    ;; Init fN wave tables for this ORC
    (csound:csoundreadscore *c* sco)))

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

;; NOTE: this was so weird and annoying...regex...i didn't miss you
(defun replace-wavetables (orc wavetables-hash)
  (declare (string orc))
  (let ((indexes (alexandria:hash-table-keys wavetables-hash)))
    (loop :for index :in indexes :do
       (let ((r (format nil "~a~a~a~%"
                        "\\{1}" (gethash index wavetables-hash) "\\{2}")))
         (setf orc (cl-ppcre:regex-replace-all
                    (format nil "\(oscil\\s+[^,]+,[^,]+,\)\\s*~a\(.*\)\\n"
                            index)
                    orc
                    r))
         (setf orc (cl-ppcre:regex-replace-all
                    (format nil "\(oscili\\s+[^,]+,[^,]+,\)\\s*~a\(.*\)\\n"
                            index)
                    orc
                    r))
         (setf orc (cl-ppcre:regex-replace-all
                    (format nil "\(table\\s+[^,]+,\)\\s*~a\(.*\)\\n"
                            index)
                    orc
                    r))
         (setf orc (cl-ppcre:regex-replace-all
                    (format nil "\(tablei\\s+[^,]+,\)\\s*~a\(.*\)\\n"
                            index)
                    orc
                    r))
         (setf orc (cl-ppcre:regex-replace-all
                    (format nil "\(vco\\s+[^,]+,[^,]+,[^,]+,[^,]+,\)\\s*~a\(.*\)\\n"
                            index)
                    orc
                    r))))
    orc))

(defun merge-orcs (&rest orchestras)
  (let ((n-instruments 0)
        (instruments)
        (n-wavetables 1)
        (wavetables)
        (wavetables-hash (make-hash-table :test #'equal))
        (globals))
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
         ;; GLOBALS
         (setf globals (concatenate 'string globals (globals orchestra)))
         ;; ORC
         (let ((tmporc (replace-wavetables orc wavetables-hash)))
           (clrhash wavetables-hash)
           (setf instruments (stich instruments tmporc))
           (incf n-instruments (regex-count "instr\\s+\\d+" tmporc)))))
    ;; ORC: Template instruments
    (setf instruments (cl-ppcre:regex-replace-all "instr\\s+\\d+"
                                                  instruments
                                                  "instr ~a"))
    ;; ORC: New Instruments numbers
    ;; FIXME: Ensure we support these many instruments
    (assert (< n-instruments 10))    
    (setf instruments (format nil instruments 1 2 3 4 5 6 7 8 9 10))
    ;; RETURN both a new ORC and SCO
    (setf *tmporc*
          (make-instance
           'orc
           :globals nil
           :name :tmporc
           :orc (stich *csound-globals* globals instruments)
           :sco wavetables))
    *tmporc*))

(defun start-thread ()
  (setf *thread*
        (bt:make-thread
         (lambda ()
           (loop
              :for frame = (csound:csoundperformksmps *c*)
              :while (= frame 0))))))

(defun stop-thread ()
  (bt:destroy-thread *thread*))

;;--------------------------------------------------
;; TODO: xml/html parser to put all on a .csd
;; Partial definition of the ORCs, see lib/csound/*.{sco,orc} for the source

(make-orc :xanadu  :filename "XANADU")
(make-orc :trapped :filename "TRAPPED")
(make-orc :kkel    :filename "KKEL")
(make-orc :asynth  :filename "ASYNTH")
(make-orc :bass    :filename "BASS")
(make-orc :drumkit :filename "DRUMKIT")


