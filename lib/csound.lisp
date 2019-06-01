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
;; - overall support reload of instruments
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
;;(defparameter *csound-options* '("-odac" "--nchnls=2" "-+rtaudio=jack" "-b128" "-B1048" "--sample-rate=44100"))
(defparameter *csound-options* '("-odac" "-m3" "--nchnls=2"))
(defvar *orcs* (make-hash-table))
(defvar *default-csound-path* (asdf:system-relative-pathname :shiny "lib/csound/"))
(defclass orc ()
  ((name    :initarg :name)
   (globals :initarg :globals :reader globals
            :documentation "global variables string")
   (orc     :initarg :orc :reader orc
            :documentation "orchestra string, has the instruments definitions")
   (sco     :initarg :sco :reader sco
            :documentation "score string, has the wavetables")
   (file    :initarg :file)))
(defclass csound-server ()
  ((thread)
   (server)))
(defvar *tmporc* NIL)
(defvar *tmppartialorc* NIL)
(defvar *thread* NIL)

;; A really useful helper!
(defun stich (&rest rest)
  "takes either several arguments or a list, puts them together
   in a string separated by new lines"
  (format nil "~{~%~a~%~}"
          (alexandria:flatten (remove-if #'null rest))))

;;--------------------------------------------------

(defun csound-send-event (iname duration vars)
  (declare (type string iname) (type number duration) (type list vars))
  (csound:csoundreadscore *c* (format nil "~a 0 ~a ~{~A~^ ~}" iname duration vars)))

(defgeneric playcsound (instrument duration &rest rest))
(defmethod playcsound ((iname string) (duration number) &rest rest)
  "pitchless sound"
  (when (> duration 0)
    (let ((vars-only (remove-if #'keywordp rest)))
      (csound-send-event iname duration vars-only))))

(defgeneric playcsound-freq (instrument duration keynum &rest rest))
(defmethod playcsound-freq ((iname string) (duration number) (keynum fixnum) &rest rest)
  "midi number to hz sound"
  (when (and (> duration 0) (> keynum 0))
    (setf (getf rest :freq) (midihz keynum))
    (let ((vars-only (remove-if #'keywordp rest)))
      (csound-send-event iname duration vars-only)))
  keynum)
(defmethod playcsound-freq ((iname string) (duration number) (keynum list) &rest rest)
  (mapc (lambda (k)
          (when (and (> duration 0) (> k 0))
            (setf (getf rest :freq) (midihz k))
            (let ((vars-only (remove-if #'keywordp rest)))
              (csound-send-event iname duration vars-only))))
        keynum))

(defgeneric playcsound-key (instrument duration keynum &rest rest))
(defmethod playcsound-key ((iname string) (duration number) (keynum integer) &rest rest)
  "midi keynum to pch"
  (when (and (> duration 0) (> keynum 0))
    (setf (getf rest :keynum) (keynum->pch keynum))
    (let ((vars-only (remove-if #'keywordp rest)))
      (csound-send-event iname duration vars-only)))
  keynum)
(defmethod playcsound-key ((iname string) (duration number) (keynum list) &rest rest)
  (mapc (lambda (k)
          (when (and (> duration 0) (> k 0))
            (setf (getf rest :keynum) (keynum->pch k))
            (let ((vars-only (remove-if #'keywordp rest)))
              (csound-send-event iname duration vars-only))))
        keynum))

(defgeneric playcsound-midi (instrument duration keynum &rest rest))
(defmethod playcsound-midi ((iname string) (duration number) (keynum fixnum) &rest rest)
  "midi keynum play"
  (when (and (> duration 0) (> keynum 0))
    (setf (getf rest :midi) keynum)
    (let ((vars-only (remove-if #'keywordp rest)))
      (csound-send-event iname duration vars-only)))
  keynum)
(defmethod playcsound-midi ((iname string) (duration number) (keynum list) &rest rest)
  (mapc (lambda (k)
          (when (and (> duration 0) (> k 0))
            (setf (getf rest :midi) k)
            (let ((vars-only (remove-if #'keywordp rest)))
              (csound-send-event iname duration vars-only))))
        keynum))

;;--------------------------------------------------

(defmacro make-play (name i &rest rest)
  "this macro will create a new (play-NAME) function wrapper of either
   playcsound or playcsound-key, with each &key defined on the function"
  (assert (and (symbolp name) (not (keywordp name))))
  (let ((fname    (intern (format nil "~A-~A" 'play name)))
        (fnamearp (intern (format nil "~A-~A-~A" 'play name 'arp))))
    (cond ((position :keynum rest)
           ;;--------------------------------------------------
           ;; CPH - Handles normal instrumentes with a single note in PCH
           ;;--------------------------------------------------
           `(progn
              (defun ,fname (keynum duration &key ,@(remove-if
                                                     #'null
                                                     (loop :for (x y) :on rest :by #'cddr
                                                           :collect
                                                              (let* ((sn (symbol-name x))
                                                                     (k  (intern sn)))
                                                                (when (not (eq :keynum x))
                                                                  (list k y))))))
                (declare (type (or integer list) keynum) (number duration)
                         (optimize speed))
                (playcsound-key ,i duration keynum
                                ,@(loop :for (k v) :on rest :by #'cddr :append
                                           (if (eq k :keynum)
                                               (list k 127) ;; dummy value...
                                               (list k (intern (symbol-name k)))))))
              (defun ,fnamearp (keynums duration offset
                                &key ,@(remove-if
                                        #'null
                                        (loop :for (x y) :on rest :by #'cddr
                                              :collect
                                                 (let* ((sn (symbol-name x))
                                                        (k  (intern sn)))
                                                   (when (not (eq :keynum x))
                                                     (list k y))))))
                (declare (list keynums) (number duration offset)
                         (optimize speed))
                (loop :for keynum :in (cdr keynums)
                      :for i :from offset :by offset
                      :with now = (now)
                      :initially (playcsound-key
                                  ,i duration (car keynums)
                                  ,@(loop :for (k v) :on rest :by #'cddr :append
                                             (if (eq k :keynum)
                                                 (list k 127) ;; dummy value...
                                                 (list k (intern (symbol-name k))))))
                      :do
                         (at (+ now (* *SAMPLE-RATE* (* (SAMPLE I) (SPB *TEMPO*))))
                             #'playcsound-key ,i duration keynum
                             ,@(loop :for (k v) :on rest :by #'cddr :append
                                        (if (eq k :keynum)
                                            (list k 127) ;; dummy value...
                                            (list k (intern (symbol-name k)))))))
                NIL)))
          ;;--------------------------------------------------
          ;; FREQ - Handles normal instruments with a single note in Hz
          ;;--------------------------------------------------
          ((position :freq rest)
           `(progn
              (defun ,fname (keynum duration
                             &key ,@(remove-if
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
              (defun ,fnamearp (keynums duration offset
                                &key ,@(remove-if
                                        #'null
                                        (loop :for (x y) :on rest :by #'cddr
                                              :collect
                                                 (let* ((sn (symbol-name x))
                                                        (k  (intern sn)))
                                                   (when (not (eq :freq x))
                                                     (list k y))))))
                (loop :for keynum :in (cdr keynums)
                      :for i :from offset :by offset
                      :with now = (now)
                      :initially (,fname (car keynums) duration
                                         ,@(remove-if
                                            #'null
                                            (loop :for (x y) :on rest :by #'cddr
                                                  :append
                                                     (let* ((sn (symbol-name x))
                                                            (k  (intern sn)))
                                                       (when (not (eq :freq x))
                                                         (list x k))))))
                      :do
                         (at (+ now (* *SAMPLE-RATE* (* (SAMPLE I) (SPB *TEMPO*))))
                             #',fname keynum duration
                             ,@(remove-if
                                #'null
                                (loop :for (x y) :on rest :by #'cddr
                                      :append
                                         (let* ((sn (symbol-name x))
                                                (k  (intern sn)))
                                           (when (not (eq :freq x))
                                             (list x k)))))))
                NIL)))
          ;;--------------------------------------------------
          ((position :midi rest)
           `(progn
              (defun ,fname (midi duration &key ,@(remove-if
                                                   #'null
                                                   (loop :for (x y) :on rest :by #'cddr
                                                         :collect
                                                            (let* ((sn (symbol-name x))
                                                                   (k  (intern sn)))
                                                              (when (not (eq :midi x))
                                                                (list k y))))))
                (declare (type (or integer list) midi) (number duration)
                         (optimize speed))
                (playcsound-midi ,i duration midi
                                 ,@(loop :for (k v) :on rest :by #'cddr :append
                                            (if (eq k :midi)
                                                (list k 127) ;; dummy value...
                                                (list k (intern (symbol-name k)))))))
              (defun ,fnamearp (midis duration offset
                                &key ,@(remove-if
                                        #'null
                                        (loop :for (x y) :on rest :by #'cddr
                                              :collect
                                                 (let* ((sn (symbol-name x))
                                                        (k  (intern sn)))
                                                   (when (not (eq :midi x))
                                                     (list k y))))))
                (declare (list midis) (number duration offset)
                         (optimize speed))
                (loop
                  :for midi :in (cdr midis)
                  :for i :from offset :by offset
                  :with now = (now)
                  :initially (playcsound-midi
                              ,i duration (car midis)
                              ,@(loop :for (k v) :on rest :by #'cddr :append
                                         (if (eq k :midi)
                                             (list k 127) ;; dummy value...
                                             (list k (intern (symbol-name k))))))
                  :do
                     (at (+ now (* *SAMPLE-RATE* (* (SAMPLE I) (SPB *TEMPO*))))
                         #'playcsound-midi ,i duration midi
                         ,@(loop :for (k v) :on rest :by #'cddr :append
                                    (if (eq k :midi)
                                        (list k 127) ;; dummy value...
                                        (list k (intern (symbol-name k)))))))
                NIL)))
          ;;--------------------------------------------------
          (t
           ;; Handles instruments without keynum
           `(defun ,fname (duration &key ,@(loop :for (x y) :on rest :by #'cddr
                                                 :collect
                                                    (let* ((sn (symbol-name x))
                                                           (k  (intern sn)))
                                                      (list k y))))
              (declare (number duration) (optimize speed))
              (playcsound ,i duration
                          ,@(loop :for (k v) :on rest :by #'cddr :append
                                     (list k (intern (symbol-name k))))))))))

;;--------------------------------------------------

(defun parse-sco (s)
  "returns only the fN wavetables on the score, remove comments, spaces and
   zeros from fN wavetable definitions"
  (declare (type string s))
  (let* ((score (cl-ppcre:regex-replace-all ";.*" s ""))
         (score (cl-ppcre:regex-replace-all "f[ ]+\(\\d+\)" score "f\\1"))
         (score (cl-ppcre:regex-replace-all "f0+\(\\d+\)" score "f\\1"))
         ;; Return only wavetables
         (score (format
                 nil
                 "~{~A~% ~}"
                 (cl-ppcre:all-matches-as-strings "f\\d+ .*" score))))
    score))

(defun get-instr (s)
  "return a list of strings with each instr in orc S"
  (declare (type string s))
  (let* ((instr (serapeum:collecting
                  (cl-ppcre:do-scans
                      (ms me rs re "(?:^|\\n)\\s*(instr\\s+[0-9a-zA-Z]+)" s)
                    (collect (aref rs 0)))))
         (endin (serapeum:collecting
                  (cl-ppcre:do-scans (ms me rs re "(?:^|\\n)\\s*(endin)" s)
                    (collect (aref re 0))))))
    (loop :for start :in instr
          :for end   :in endin
          :collect (subseq s start end))))

;; FIXME: not sure what to return when no pN is used
(defun get-p-max (s)
  "returns the max parameter number used in instrument S"
  (declare (type string s))
  (let* ((pns (cl-ppcre:all-matches-as-strings "p\\d+" s))
         (ns  (mapcar (alexandria:compose #'parse-integer
                                          #'str:s-last)
                      pns))
         (n   (alexandria:extremum ns #'>)))
    n))

(defun mono-to-stereo (s)
  "adds 2 new params to instr S"
  (let ((p-max (get-p-max s)))
    (cl-ppcre:regex-replace " out\\s+\(.*\)"
                            s
                            (format nil "outs (\\{1})*p~d,(\\{1})*p~d"
                                    (+ 1 p-max)
                                    (+ 2 p-max)))))

(defun parse-orc (s)
  "returns the orc, changes mono to stereo, remove comments"
  (declare (string s))
  (let* ((orc         (cl-ppcre:regex-replace-all ";.*" s ""))
         (soundin-p   (cl-ppcre:scan "soundin" orc))
         (mono-p      (cl-ppcre:scan "nchnls\\s*=\\s*1" orc)))
    (when soundin-p (error "soundin required"))
    (str:unlines
     (loop :for instr :in (get-instr orc)
           :if mono-p
           :collect (mono-to-stereo instr)
           :else
           :collect instr))))

;; FIXME: assumes globals are only defined before the first instr
;;        declaration appears
(defun parse-globals (s)
  "returns a string with all globals"
  (declare (type string s))
  (let* ((orc         (cl-ppcre:regex-replace-all ";.*" s ""))
         (first-instr (cl-ppcre:scan "instr\\s+\\d+" orc))
         (globals     (subseq orc 0 first-instr)))
    globals))

(defun resolve-csound-path (key-name &optional (extension ".orc")
                                               (filepath *default-csound-path*))
  "returns the path"
  (let* ((name (symbol-name key-name))
         (file (merge-pathnames
                (str:concat name extension) filepath)))
    (or (probe-file file)
        (error "file missing"))))

;; NOTE: before running this try the sound on the CLI with:
;; $ csound -odac 326a.{orc,sco}
;; TODO: support only .orc
(defun make-orc (name &key sco orc globals
                           filename (filepath *default-csound-path*)
                           orc-path sco-path)
  "This function creates a new orchestra file, reading score wave tables too"
  (assert (keywordp name))
  ;; Find files if parameters provided
  (when filename
    (setf orc-path (resolve-csound-path name ".orc" filepath))
    (setf sco-path (resolve-csound-path name ".sco" filepath)))
  ;; Read into vars
  (when (and orc-path sco-path)
    (setf globals (parse-globals (alexandria:read-file-into-string orc-path)))
    (setf orc     (parse-orc (alexandria:read-file-into-string orc-path)))
    (setf sco     (parse-sco (alexandria:read-file-into-string sco-path))))
  (setf (gethash name *orcs*)
        (make-instance 'orc
                       :name name
                       :file filepath
                       :globals globals
                       :sco sco
                       :orc orc)))

(defun list-orcs ()
  (alexandria:hash-table-keys *orcs*))

(defun set-csound-options (&optional (options *csound-options*))
  (declare (type list options))
  (loop :for option :in options
        :when (stringp option) :do
           (cffi:with-foreign-string (coption option)
             (csound:csoundsetoption *c* coption))))

(defun get-orc (orc-name &key n-instr (filepath *default-csound-path*))
  (assert (keywordp orc-name))
  (with-slots (orc) (gethash orc-name *orcs*)
    (let ((orchestra (or orc
                         (alexandria:read-file-into-string
                          (resolve-csound-path orc-name ".orc" filepath)))))
      (if n-instr
          (nth n-instr (get-instr orchestra))
          orchestra))))

(defun get-sco (sco-name &key (filepath *default-csound-path*))
  "returns the score string"
  (assert (keywordp sco-name))
  (with-slots (sco) (gethash sco-name *orcs*)
    (or sco
        (alexandria:read-file-into-string
         (resolve-csound-path sco-name ".sco" filepath)))))

(defun get-globals (orc-name &optional filter-globals)
  "returns the string with the global definitions"
  (assert (keywordp orc-name))
  (with-slots (globals) (gethash orc-name *orcs*)
    (if filter-globals
        (loop :for default :in '("sr" "kr" "ksmps" "nchnls")
              :finally (return result)
              :with result := globals
              :do (setf result (cl-ppcre:regex-replace-all
                                (format nil "\\s*~a\\s*=\\s*.*\\n" default)
                                result (format nil "~%"))))
        globals)))

(defun get-orchestra (orc-name &optional n-instr)
  "returns an orchestra object"
  (assert (keywordp orc-name))
  (if n-instr
      (setf *tmppartialorc*
            (make-instance 'orc
                           :sco (get-sco orc-name)
                           :orc (get-orc orc-name n-instr)
                           :globals (get-globals orc-name)
                           :name "slice"))
      (gethash orc-name *orcs*)))

;;--------------------------------------------------
;;
;; Merge utils
;;

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

(defun replace-var (orc index key value)
  (declare (type string orc))
  (let ((replacement (format nil "~a~a~a~%" "\\{1}" value "\\{2}")))
    (cl-ppcre:regex-replace-all
     (concatenate 'string
                  "\(" key "\\s+[^,]+,[^,]+,\)\\s*" index "\(.*\)\\n")
     orc
     replacement)))

;; NOTE: this was so weird and annoying...regex...i didn't miss you
(defun replace-wavetables (orc wavetables-hash)
  (declare (type string orc) (type hash-table wavetables-hash))
  (maphash (lambda (k v)
             (setf orc (replace-var orc k "oscili" v))
             (setf orc (replace-var orc k "table"  v))
             (setf orc (replace-var orc k "tablei" v))
             (setf orc (replace-var orc k "vco"    v)))
           wavetables-hash)
  orc)

(defun merge-orcs (&rest orchestras)
  (let ((n-instruments 0)
        (instruments)
        (n-wavetables 1)
        (wavetables)
        (wavetables-hash (make-hash-table :test #'equal))
        (globals))
    (loop :for orchestra :in orchestras
          :do
             (with-slots (orc sco) orchestra
               ;; SCO
               (loop :for f :in (get-fn sco)
                     :for fn :from n-wavetables
                     :with temp-wavetable = sco
                     :finally (setf wavetables (concatenate 'string
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

;;--------------------------------------------------
;;
;; Process
;;

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

(defun start-thread ()
  (setf *thread*
        (bt:make-thread
         (lambda ()
           (loop :for frame = (csound:csoundperformksmps *c*)
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
