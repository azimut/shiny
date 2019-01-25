(in-package #:espeak-ng)

(defvar *espeak-buffers* (make-hash-table :test #'equal))

(defun chunk-to-buffer (pointer length)
  "Returns a new buffer filled with the content on pointer
   with size LENGTH"
  (declare (type cffi:foreign-pointer pointer)
           (type fixnum length))
  (let ((buf (incudine:make-buffer length :sample-rate 22050)))
    (incudine.external:foreign-copy-samples
     (incudine:buffer-data buf)
     pointer
     length)
    buf))

(defun get-syl (syl-counter syls-length)
  "Helper that returns the syllabic state for ecantorix,
   based on the sentence length on SYLS-LENGTH and the
   current SYL-COUNTER."
  (declare (type fixnum syl-counter syls-length))
  (cond ((= syls-length 1) 0)
        ((and (= syls-length 2) (= syl-counter 0)) 1)
        ((and (= syls-length 2) (= syl-counter 1)) 3)
        ((and (> syls-length 2) (= syl-counter 0)) 1)
        ((and (> syls-length 2)
              (< syl-counter (1- syls-length))) 2)
        (t 3)))

(defun espeak-phonemes (text)
  "Returns a list with the phonemes of the given TEXT.
   Should be run inside a initialized espeak
   state block."
  (declare (type string text))
  (cffi:with-foreign-string (s text)
    (cffi:with-foreign-object (p :pointer)
      (setf (cffi:mem-aref p :pointer) s)
      (let* ((phonemes (espeak_texttophonemes
                        p
                        ESPEAKCHARS_AUTO
                        0))
             (phonemes-list phonemes))
        phonemes-list))))

;; libespeak-ng PATCHES:
;; - Samples are doubles instead of singles
;; - Synthezise accepts phonemes
;; - Init and Term
;; (defun sinsytest (speech &optional (language "es") (speed 960))
;;   "Returns a new buffer with the SPEECH rendered into it."
;;   (declare (type string language))
;;   (with-espeak (:AUDIO_OUTPUT_SYNCHRONOUS 0 (cffi:null-pointer) 0)
;;     (with-sinsy
;;       (espeak_setsynthcallback (cffi:callback ctest))
;;       (cffi:with-foreign-object (l :int)
;;         (espeak_setvoicebyname language)
;;         (dolist (sentence (cl-ppcre:split :whitespace-char-class speech))
;;           (sinsy_ng_addrest (round (* .1 speed)))
;;           (let* ((syls (if (> (length sentence) 3)
;;                            (espeak-phonemes sentence)
;;                            (list sentence)))
;;                  (syls-length (length syls))
;;                  (syl-counter 0)
;;                  (syl-type 0)
;;                  (breath 0))
;;             (dolist (syl syls)
;;               (incf breath (length syls))
;;               (setf syl-type (get-syl syl-counter syls-length))
;;               (format t "~%~a~C~a~C~a"
;;                       syl-type #\tab
;;                       sentence #\tab
;;                       syl)
;;               (sinsy_ng_addnote
;;                (round
;;                 (* speed (cm:interp (length syl)
;;                                   0f0 .15 3 1f0)))
;;                (if (> (length sentence) 3)
;;                    (format NIL "[[~a]]" syl)
;;                    syl)
;;                (cm:pickl (shiny::ov-scale :C3 :minor))
;;                NIL NIL 1 syl-type (if (> breath 8)
;;                                     (progn (setf breath 0) T)
;;                                     NIL))
;;               (incf syl-counter))))
;;         (chunk-to-buffer (sinsy_ng_getaudiodata l)
;;                          (cffi:mem-ref l :int))))))

(defun mixed-parser (s &optional (split-by ",|'|[ ]"))
  "takes a string separated by spaces and further down by hyphens
   to denote syllabic separation"
  (loop :for word :in (cl-ppcre:split :whitespace-char-class s)
     :append (cond ((position #\- word :test #'equal) ;; Split by dashes?
                    (cl-ppcre:split "-" word))
                   ;; Is supposed to be parsed TO phonemes
                   ((string= "<" (subseq word 0 1))
                    (let* ((word (cl-ppcre:regex-replace-all "<|>" word ""))
                           (split-char (subseq word 0 1))
                           (split-p    (not (cl-ppcre:scan "[a-zA-Z]" split-char))))
                      (when split-p
                        (setf split-by split-char)
                        (setf word (subseq word 1)))
                      (text-to-phonemes word "en" T split-by)))
                   (t (list word)))))

(defvar *tmp* nil)
(defun sinsytest (phonemes notes durations breaths &optional stypes (language "en") (speed 960))
  "Returns a new buffer with the SPEECH rendered into it."
  (declare (type string language))
  (when *tmp* (incudine:free *tmp*))
  (with-espeak (:AUDIO_OUTPUT_SYNCHRONOUS 0 (cffi:null-pointer) 0)
    (with-sinsy ()
      (espeak_setsynthcallback (cffi:callback ctest))
      (cffi:with-foreign-object (l :int)
        (espeak_setvoicebyname language)
        (let* ((syls phonemes)
               (csyls (shiny::make-cycle syls))
               (syls-length (if (string= "[" (subseq (first syls) 0 1))
                                (length syls)
                                1))
               (cdurations (shiny::make-cycle durations))
               (cnotes (shiny::make-cycle notes))
               (cbreaths (shiny::make-cycle breaths))
               (cstypes (if stypes (shiny::make-cycle stypes)))
               (syl-counter 0)
               (syl-type 0))
          (sinsy_ng_addrest (round (* .1 speed)))
          (mapcar (lambda (syl)
                    (if stypes
                        (cm:next cstypes)
                        (setf syl-type (get-syl syl-counter syls-length)))
                    (format t "~%~a~C~a"
                            syl-type #\tab
                            syl)
                    (sinsy_ng_addnote (round (* speed (cm:next cdurations)))
                                      syl
                                      (cm:next cnotes)
                                      NIL
                                      NIL
                                      0
                                      syl-type
                                      (cm:next cbreaths))
                    (incf syl-counter))
                  syls))
        (sinsy_ng_addrest (round (* .1 speed)))
        (setf *tmp*
              (chunk-to-buffer (sinsy_ng_getaudiodata l)
                               (cffi:mem-ref l :int)))))))




;; (defun sim (phonemes notes durations breaths &optional stypes (language "en") (speed 960))
;;   "Returns a new buffer with the SPEECH rendered into it."
;;   (declare (type string language))
;;   (when (gethash phonemes shiny::*buffers*)
;;     (incudine:free (gethash phonemes shiny::*buffers*)))
;;   (with-espeak (:AUDIO_OUTPUT_SYNCHRONOUS 0 (cffi:null-pointer) 0)
;;     (with-sinsy ()
;;       (espeak_setsynthcallback (cffi:callback ctest))
;;       (cffi:with-foreign-object (l :int)
;;         (espeak_setvoicebyname language)
;;         (sinsy_ng_addnote (round (* speed durations))
;;                           phonemes
;;                           notes
;;                           NIL
;;                           NIL
;;                           0
;;                           0
;;                           breaths)
;;         (setf (gethash phonemes shiny::*buffers*)
;;               (chunk-to-buffer (sinsy_ng_getaudiodata l)
;;                                (cffi:mem-ref l :int)))))))

(defun sim (phonemes note duration breath &optional (language "en")
                                                    (speed 960))
  "Returns a new buffer with the SPEECH rendered into it, buffer is saved
   on *BUFFERS* with PHONEMES for key"
  (declare (type string phonemes language))
  (when (gethash phonemes shiny::*buffers*)
    (incudine:free (gethash phonemes shiny::*buffers*)))
  (with-espeak (:AUDIO_OUTPUT_SYNCHRONOUS 0 (cffi:null-pointer) 0)
    (with-sinsy ()
      (espeak_setsynthcallback (cffi:callback ctest))
      (cffi:with-foreign-object (l :int)
        (espeak_setvoicebyname language)
        (sinsy_ng_addnote (round (* speed duration))
                          phonemes
                          note
                          T
                          T
                          1
                          1
                          breath)
        (setf (gethash phonemes shiny::*buffers*)
              (chunk-to-buffer (sinsy_ng_getaudiodata l)
                               (cffi:mem-ref l :int)))))))

(defun sims (phrase notes durations breaths &optional (language "en"))
  "Creates separate buffers for each list element on PHRASE."
  (declare (type list phrase))
  (let ((cnotes     (shiny::make-cycle (alexandria:ensure-list notes)))
        (cdurations (shiny::make-cycle (alexandria:ensure-list durations)))
        (cbreaths   (shiny::make-cycle (alexandria:ensure-list breaths))))
    (mapcar
     (lambda (p)
       (sim p (cm:next cnotes) (cm:next cdurations) (cm:next cbreaths) language))
     phrase)))


(defun sim1 (word phonemes note duration breath &optional (language "en")
                                                          (syl-type 0)
                                                          accent
                                                          stacatto
                                                          (slur-type 0)
                                                          (speed 960))
  "WORD is the index on buffers
   PHONEMES chunks to render, might include a space"
  (declare (type string word language)
           (type list phonemes))
  (when (gethash word shiny::*buffers*)
    (incudine:free (gethash phonemes shiny::*buffers*)))
  (with-espeak (:AUDIO_OUTPUT_SYNCHRONOUS 0 (cffi:null-pointer) 0)
    (with-sinsy ()
      (espeak_setsynthcallback (cffi:callback ctest))
      (cffi:with-foreign-object (l :int)
        (espeak_setvoicebyname language)
        (let ((cduration (shiny::make-cycle (alexandria:ensure-list duration)))
              (cnotes (shiny::make-cycle (alexandria:ensure-list note)))
              (cbreath (shiny::make-cycle (alexandria:ensure-list breath)))
              (csyl-type (shiny::make-cycle (alexandria:ensure-list syl-type)))
              (caccent (shiny::make-cycle (alexandria:ensure-list accent)))
              (cstacatto (shiny::make-cycle (alexandria:ensure-list stacatto)))
              (cslur-type (shiny::make-cycle (alexandria:ensure-list slur-type))))
          (mapcar
           (lambda (phoneme)
             (if (string= " " phoneme)
                 (sinsy_ng_addrest (round (* speed .1)))
                 (sinsy_ng_addnote (round (* speed (cm:next cduration)))
                                   phoneme
                                   (cm:next cnotes)
                                   (cm:next caccent)
                                   (cm:next cstacatto)
                                   (cm:next cslur-type)
                                   (cm:next csyl-type)
                                   (cm:next cbreath))))
           phonemes))
        (setf (gethash word shiny::*buffers*)
              (chunk-to-buffer (sinsy_ng_getaudiodata l)
                               (cffi:mem-ref l :int)))))))
