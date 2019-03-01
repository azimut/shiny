(in-package #:shiny)
;; ----------- Overtone / Clojure

;; http://www.appservgrid.com/hyper/hyp/lisp#take
;; SOMECEPL> (take 2 '(0 1 2 3 4 5))
;; (0 1)

;; (defun take (n l)
;;   (cond ((< n 0) (error "index negative"))
;;         ((= n 0) ())
;;         ((null l) (error "index too large"))
;;         (t (cons (car l) (take (- n 1) (cdr l))))))


;; https://github.com/tororo060608/danmachi/blob/master/src/util.lisp

(defun take-nth (n seq)
  (do ((i 0 (+ i n))
       (res nil (cons (elt seq i) res))
       (len (length seq)))
      ((<= len i) (nreverse res))))

;; http://www.appservgrid.com/hyper/hyp/lisp#take
;; SOMECEPL> (take 2 '(0 1 2 3 4 5))
;; (0 1)

(defun take (n l)
  (cond ((< n 0) (error "index negative"))
        ((= n 0) ())
        ((null l) (error "index too large"))
        (t (cons (car l) (take (- n 1) (cdr l))))))

;; Taking a similar function to cycle
;; NOTE: use make-list
(defun repeat (n l &optional (nl '()))
  (when (not (listp l))
    (setf l (list l)))
  (if (zerop n)
      nl
      (repeat (- n 1)
              (rotate l 1)
              (if (not nl)
                  (list (first l))
                  (append nl (list (first l)))))))

(defun comp (a b)
  (lambda (&rest args)
    (funcall a (apply b args))))

(defun mapcat (function &rest lists)
  (apply #'concatenate 'list (apply #'mapcar function lists)))

(defun reductions (function sequence &rest args
                   &key key from-end (start 0) end initial-value)
  (declare (ignore key from-end start end initial-value))
  "Return a list of intermediate values from reducing SEQUENCE with FUNCTION."
  (let* ((reductions (list))
         (result (apply #'reduce
                        (lambda (&rest arguments)
                          (let ((result (apply function arguments)))
                            (push result reductions)
                            result))
                        sequence
                        args)))
    (values (or (nreverse reductions)
                (list result))
            result)))

;;;
;; pitch.clj
;;;

(defun to-keyword (s)
  (cond ((stringp s) (intern (string-upcase s) "KEYWORD"))
        ((keywordp s) s)))

(defun name (n)
  (cond ((keywordp n) (symbol-name n))
        ((stringp n) n)))

(defun octave-note (octave interval)
  "Convert an octave and interval to a midi note."
  (+ (* octave 12) interval 12))

(defvar +notes+ (alexandria:alist-hash-table
                       '((:C  . 0)  (:B# . 0)
                         (:C# . 1)  (:DB . 1)
                         (:D  . 2)
                         (:D# . 3)  (:EB . 3)
                         (:E  . 4)
                         (:E# . 5)  (:F  . 5)
                         (:F# . 6)  (:GB . 6)
                         (:G  . 7)
                         (:G# . 8)  (:AB . 8)
                         (:A  . 9)
                         (:A# . 10) (:BB . 10)
                         (:B  . 11) (:CB . 11))))

(defun notes (n)
  (if (keywordp n)
      (gethash n +notes+)
      (error "not a keyword")))

(defvar *reverse-notes*
  (alexandria:alist-hash-table
   '((0  . :C)   (1 . :C#)
     (2  . :D)   (3 . :Eb)
     (4  . :E)   (5 . :F)
     (6  . :F#)  (7 . :F#)
     (8  . :Ab)  (9 . :A)
     (10 . :Bb) (11 . :B))))

(defun reverse-notes (n)
  (if (and (< n 12) (>= n 0))
      (gethash n *reverse-notes*)
      (error "out of range")))

(defvar *midi-note-re-str* "([a-gA-G][#bB]?)([-0-9]+)")
(defvar *only-midi-note-re-str*
  (concatenate 'string "\\A" *midi-note-re-str* "\\Z"))

(defun midi-string-matcher (mk)
  "Determines whether a midi keyword is valid or not. If valid,
  returns a regexp match object"
  (multiple-value-bind (_ m) (cl-ppcre:scan-to-strings *only-midi-note-re-str*
                                                       (name mk))
    (declare (ignore _))
    m))

(defun validate-midi-string! (mk)
  (let ((matches (midi-string-matcher mk)))
    (unless matches
      (error "invalid midi-string"))
    (let ((octave (aref matches 1)))
      (when (< (parse-integer octave) -1)
        (error "invalid midi-string too low")))
    matches))

(defun canonical-pitch-class-name (pc)
  "Returns the canonical version of the specified pitch class pc."
  (reverse-notes (notes (to-keyword pc))))

(defun note-info (midi-string)
  "Takes a string representing a midi note such as C4 and returns a map
  of note info"
  (let* ((matches     (validate-midi-string! midi-string))
         (pitch-class (aref matches 0))
         (octave      (aref matches 1))
         (pitch-class (canonical-pitch-class-name pitch-class))
         (octave      (parse-integer octave))
         (interval    (notes (to-keyword pitch-class)))
         (midi-note   (octave-note octave interval)))
    (alexandria:alist-hash-table `((:pitch-class . ,pitch-class)
                                   (:octave      . ,octave)
                                   (:interval    . ,interval)
                                   (:midi-note   . ,midi-note)))))

(defun note (n)
  "Resolves note to MIDI number format. Resolves upper and lower-case
  keywords and strings in MIDI note format. If given an integer or
  nil, returns them unmodified. All other inputs will raise an
  exception.

  Usage examples:

  (note \"C4\")  ;=> 60
  (note \"C#4\") ;=> 61
  (note \"eb2\") ;=> 39
  (note :F#7)    ;=> 102
  (note :db5)    ;=> 73
  (note 60)      ;=> 60
  (note nil)     ;=> nil"
  (cond ((null n) nil)
        ((integerp n) (if (>= n 0)
                          n
                          (error "Value out of range.")))
        ((keywordp n) (note (symbol-name n)))
        ((stringp n)  (gethash :midi-note (note-info n)))
        (t (error "Bad argument."))))

;; NOTE: hack in
(defparameter +scale+
  (flet ((rotate (scale-sequence offset)
           (take (length scale-sequence)
                 (nthcdr offset (append scale-sequence scale-sequence)))))
    (let* ((ionian-sequence     '(2 2 1 2 2 2 1))
           (hex-sequence        '(2 2 1 2 2 3))
           (pentatonic-sequence '(3 2 2 3 2))
           (melodic-minor       '(2 1 2 2 2 2 1)))
      `((:diatonic     . ,ionian-sequence)
        (:ionian       . ,(rotate ionian-sequence 0))
        (:major        . ,(rotate ionian-sequence 0))
        (:dorian       . ,(rotate ionian-sequence 1))
        (:phrygian     . ,(rotate ionian-sequence 2))
        (:lydian       . ,(rotate ionian-sequence 3))
        (:mixolydian   . ,(rotate ionian-sequence 4))
        (:aeolian      . ,(rotate ionian-sequence 5))
        (:minor        . ,(rotate ionian-sequence 5))
        (:locrian      . ,(rotate ionian-sequence 6))
        (:hex-major6   . ,(rotate hex-sequence 0))
        (:hex-dorian   . ,(rotate hex-sequence 1))
        (:hex-phrygian . ,(rotate hex-sequence 2))
        (:hex-major7   . ,(rotate hex-sequence 3))
        (:hex-sus      . ,(rotate hex-sequence 4))
        (:hex-aeolian  . ,(rotate hex-sequence 5))
        (:minor-pentatonic . ,(rotate pentatonic-sequence 0))
        (:yu               . ,(rotate pentatonic-sequence 0))
        (:major-pentatonic . ,(rotate pentatonic-sequence 1))
        (:gong             . ,(rotate pentatonic-sequence 1))
        (:egyptian         . ,(rotate pentatonic-sequence 2))
        (:shang            . ,(rotate pentatonic-sequence 2))
        (:jiao             . ,(rotate pentatonic-sequence 3))
        (:pentatonic       . ,(rotate pentatonic-sequence 4))
        (:zhi              . ,(rotate pentatonic-sequence 4))
        (:ritusen          . ,(rotate pentatonic-sequence 4))
        ;; ?? - from rsg
        (:dorian-2         . ,(rotate melodic-minor 1))
        (:lydian-augmented . ,(rotate melodic-minor 2))
        (:lydian-dominant  . ,(rotate melodic-minor 3))
        (:mixolydian-6     . ,(rotate melodic-minor 4)) ;; bartok,hindu,mm
        (:half-diminished  . ,(rotate melodic-minor 5))
        (:altered-scale    . ,(rotate melodic-minor 6)) ;; super-locrian
        ;; from nudruz
        (:goodmode          . (1 1 3 1 1 2 3))
        (:stravmode         . (2 1 2))     ;; "Shur"
        (:hyperlydian       . (2 2 2 1))
        (:hyperphrygian     . (1 2 2))     ;; "Segah"
        (:shushtar          . (1 2 1 3))
        (:bayati            . (2 2 1 2 1)) ;; "Bayati Shiraz"
        (:humayun           . (1 3 1 2 2))
        ;; end of ??
        (:whole-tone        . (2 2 2 2 2 2))
        (:whole             . (2 2 2 2 2 2))
        (:chromatic         . (1 1 1 1 1 1 1 1 1 1 1 1))
        (:harmonic-minor    . (2 1 2 2 1 3 1))
        (:melodic-minor-asc . ,melodic-minor)
        (:hungarian-minor   . (2 1 3 1 1 3 1))
        (:octatonic         . (2 1 2 1 2 1 2 1))
        (:messiaen1         . (2 2 2 2 2 2))
        (:messiaen2         . (1 2 1 2 1 2 1 2))
        (:messiaen3         . (2 1 1 2 1 1 2 1 1))
        (:messiaen4         . (1 1 3 1 1 1 3 1))
        (:messiaen5         . (1 4 1 1 4 1))
        (:messiaen6         . (2 2 1 1 2 2 1 1))
        (:messiaen7         . (1 1 1 2 1 1 1 1 2 1))
        (:super-locrian     . (1 2 1 2 2 2 2))
        (:hirajoshi         . (2 1 4 1 4))
        (:kumoi             . (2 1 4 2 3))
        (:neapolitan-major  . (1 2 2 2 2 2 1))
        (:bartok            . (2 2 1 2 1 2 2))
        (:bhairav           . (1 3 1 2 1 3 1))
        (:locrian-major     . (2 2 1 1 2 2 2))
        (:ahirbhairav       . (1 3 1 2 2 1 2))
        (:enigmatic         . (1 3 2 2 2 1 1))
        (:neapolitan-minor  . (1 2 2 2 1 3 1))
        (:pelog             . (1 2 4 1 4))
        (:augmented2        . (1 3 1 3 1 3))
        (:scriabin          . (1 3 3 2 3))
        (:harmonic-major    . (2 2 1 2 1 3 1))
        (:melodic-minor-desc . (2 1 2 2 1 2 2))
        (:romanian-minor    . (2 1 3 1 2 1 2))
        (:hindu             . (2 2 1 2 1 2 2))
        (:iwato             . (1 4 1 4 2))
        (:melodic-minor     . ,melodic-minor)
        (:diminished2       . (2 1 2 1 2 1 2 1))
        (:marva             . (1 3 2 1 2 2 1))
        (:melodic-major     . (2 2 1 2 1 2 2))
        (:indian            . (4 1 2 3 2))
        (:spanish           . (1 3 1 2 1 2 2))
        (:prometheus        . (2 2 2 5 1))
        (:diminished        . (1 2 1 2 1 2 1 2))
        (:todi              . (1 2 3 1 1 3 1))
        (:leading-whole     . (2 2 2 2 2 1 1))
        (:augmented         . (3 1 3 1 3 1))
        (:purvi             . (1 3 2 1 1 3 1))
        (:chinese           . (4 2 1 4 1))
        (:lydian-minor      . (2 2 2 1 1 2 2))
        (:ryukyu            . (4 1 2 4 1))
        (:blues-major       . (2 1 1 3 2 3))
        (:blues-minor       . (3 2 1 1 3 2))))))

(defvar +degree+
  (let ((degrees '((:i   . 1) (:ii . 2) (:iii . 3)
                   (:iv  . 4) (:v  . 5) (:vi  . 6)
                   (:vii . 7) (:_  . nil))))
    (alexandria:alist-hash-table degrees)))

(defun degree (d)
  (gethash d +degree+))

(defun degree->interval (degree scale)
  "Converts the degree of a scale given as a roman numeral keyword and
  converts it to the number of semitones from the tonic of
  the specified scale.

  (degree->interval :ii :major) ;=> 2

  Trailing #, b, + - represent sharps, flats, octaves up and down
  respectively.  An arbitrary number may be added in any order."
  (cond ((null degree)   nil)
        ((eql :_ degree) nil)
        ((numberp degree) (nth-interval scale (1- degree)))
        ((keywordp degree) (let* ((degree     (resolve-degree degree))
                                  (interval   (nth-interval scale (1- (gethash :degree degree))))
                                  (oct-shift  (* 12 (gethash :octave-shift degree)))
                                  (semi-shift (gethash :semitone-shift degree)))
                             (+ interval oct-shift semi-shift)))))

(defun degrees->pitches (degrees scale root)
  "Convert intervals to pitches in MIDI number format.  Supports
  nested collections."
  (let ((root (note root)))
    (unless root
      (error "invalid root"))
    (mapcar (lambda (degree) (cond ((listp degree)
                               (degrees->pitches degree scale root))
                              ((null degree) nil)
                              (t (let ((interval (degree->interval degree scale)))
                                   (when interval
                                     (+ root interval))))))
            degrees)))

(defun nth-interval (x &optional y)
  "Return the count of semitones for the nth degree from the start of
  the diatonic scale in the specific mode (or ionian/major by
  default).

  i.e. the ionian/major scale has an interval sequence of 2 2 1 2 2 2
       1 therefore the 4th degree is (+ 2 2 1 2) semitones from the
       start of the scale."
  (let ((m (cdr (assoc x +scale+))))
    (if y
        (reduce #'+ (take y (append m m m)))
        (nth-interval :diatonic x))))

(defun find-pitch-class-name (note)
  "Given a midi number representing a note, returns the name of the note
  independent of octave.

  (find-pitch-class-name 62) ;=> :D
  (find-pitch-class-name 74) ;=> :D
  (find-pitch-class-name 75) ;=> :Eb"
  (reverse-notes (mod note 12)))

(defun find-note-name (note)
  "Given a midi number representing a note, returns a keyword
  representing the note including octave number. Reverse of the fn note.

  (find-note-name 45) ;=> A2
  (find-note-name 57) ;=> A3
  (find-note-name 58) ;=> Bb3"
  (when note
    (let ((octave (1- (floor (/ note 12)))))
      (to-keyword (concatenate 'string
                               (name (find-pitch-class-name note))
                               (write-to-string octave))))))

(defun degree->int (degree)
  (if (some (lambda (x) (degree x)) (alexandria:hash-table-keys +degree+))
      (degree degree)
      (error "wrong degree")))

(defun resolve-degree (degree &optional octave-shift semitone-shift)
  "returns a map representing the degree, and the octave semitone
  shift (i.e. sharp flat)"
  (if (and octave-shift semitone-shift)
      (let* ((oct      (cl-user::string-to-octets (name degree)))
             (len-oct  (1- (length oct)))
             (last-oct (aref oct len-oct)))
        (cond ((= 45 last-oct) (resolve-degree (to-keyword (name degree))
                                               (1- octave-shift)
                                               semitone-shift))
              ((= 43 last-oct) (resolve-degree (to-keyword (name degree))
                                               (1+ octave-shift)
                                               semitone-shift))
              ((= 98 last-oct) (resolve-degree (to-keyword (name degree))
                                               octave-shift
                                               (1- semitone-shift)))
              ((= 35 last-oct) (resolve-degree (to-keyword (name degree))
                                               octave-shift
                                               (1+ semitone-shift)))
              (t (let ((degree (degree->int degree)))
                   (alexandria:alist-hash-table `((:degree . ,degree)
                                                  (:octave-shift . ,octave-shift)
                                                  (:semitone-shift . ,semitone-shift)))))))
      (resolve-degree degree 0 0)))

(defun resolve-degrees (degrees)
  "Either maps the degrees to integers if they're keywords using the map DEGREE
  or leaves them unmodified"
  (mapcar (lambda (x) (if (keywordp x) (degree x) x))
          degrees))

(defun ov-scale (root scale &optional l-degrees)
  "Returns a list of notes for the specified scale. The root must be
   in midi note format i.e. :C4 or :Bb4


   (scale :c4 :major)  ; c major      -> (60 62 64 65 67 69 71 72)
   (scale :Bb4 :minor) ; b flat minor -> (70 72 73 75 77 78 80 82)"
  (if l-degrees
      (let ((root    (note root))
            (degrees (resolve-degrees l-degrees)))
        (cons root
              (mapcar (lambda (x) (+ root (nth-interval scale x)))
                      degrees)))
      (ov-scale root scale (myrange 8 :min 1))))

(defvar +chord+
  (let ((major  '(0 4 7))
        (minor  '(0 3 7))
        (major7 '(0 4 7 11))
        (dom7   '(0 4 7 10))
        (minor7 '(0 3 7 10))
        (aug    '(0 4 8))
        (dim    '(0 3 6))
        (dim7   '(0 3 6 9)))
    (alexandria:alist-hash-table
     `((:1           . (0))
       (:5           . (0 7))
       (:+5          . (0 4 8))
       (:m+5         . (0 3 8))
       (:sus2        . (0 2 7))
       (:sus4        . (0 5 7))
       (:6           . (0 4 7 9))
       (:m6          . (0 3 7 9))
       (:7sus2       . (0 2 7 10))
       (:7sus4       . (0 5 7 10))
       (:7-5         . (0 4 6 10))
       (:m7-5        . (0 3 6 10))
       (:7+5         . (0 4 8 10))
       (:m7+5        . (0 3 8 10))
       (:9           . (0 4 7 10 14))
       (:m9          . (0 3 7 10 14))
       (:m7+9        . (0 3 7 10 14))
       (:maj9        . (0 4 7 11 14))
       (:9sus4       . (0 5 7 10 14))
       (:6*9         . (0 4 7 9 14))
       (:m6*9        . (0 3 9 7 14))
       (:7-9         . (0 4 7 10 13))
       (:m7-9        . (0 3 7 10 13))
       (:7-10        . (0 4 7 10 15))
       (:9+5         . (0 10 13))
       (:m9+5        . (0 10 14))
       (:7+5-9       . (0 4 8 10 13))
       (:m7+5-9      . (0 3 8 10 13))
       (:11          . (0 4 7 10 14 17))
       (:m11         . (0 3 7 10 14 17))
       (:maj11       . (0 4 7 11 14 17))
       (:11+         . (0 4 7 10 14 18))
       (:m11+        . (0 3 7 10 14 18))
       (:13          . (0 4 7 10 14 17 21))
       (:m13         . (0 3 7 10 14 17 21))
       (:major       . ,major)
       (:M           . ,major)
       (:minor       . ,minor)
       (:m           . ,minor)
       (:major7      . ,major7)
       (:dom7        . ,dom7)
       (:7           . ,dom7)
       (:M7          . ,major7)
       (:minor7      . ,minor7)
       (:m7          . ,minor7)
       (:augmented   . ,aug)
       (:a           . ,aug)
       (:diminished  . ,dim)
       (:dim         . ,dim)
       (:i           . ,dim)
       (:diminished7 . ,dim7)
       (:dim7        . ,dim7)
       (:i7          . ,dim7)))))

(defun inc-first (elems n)
  "Remove the first element, increment it by n, and append to seq."
  (append (rest elems)
          (list (+ n (first elems)))))

(defun dec-last (elems n)
  "Remove the last element, decrement it by n, and prepend to seq."
  (append (list (- (last elems) n))
          (butlast elems)))

(defun resolve-chord (chord)
  "Either looks the chord up in the map of CHORDs if it's a keyword or
  simply returns it unnmodified. Allows users to specify a chord
  either with a set such as #{0 4 7} or by keyword such as :major
  SHINY> (resolve-chord :m9)
  (0 3 7 10 14)"
  (if (keywordp chord)
      (gethash chord +chord+)
      chord))

(defun invert-chord (notes shift)
  "Move a chord voicing up or down.

    ;first inversion
    (invert-chord [60 64 67] 1) ;=> (64 67 72)

    ; second inversion
    (invert-chord [60 64 67] 2) ;=> (67 72 76)
  "
  (cond ((> shift 0) (invert-chord (inc-first notes 12) (1- shift)))
        ((< shift 0) (invert-chord (dec-last notes 12)  (1+ shift)))
        ((= shift 0) notes)))

;; NOTE: retuns might not be sorted accordingly (?
;;       also second example is incorrect (!
(defun chord (root chord-name &optional inversion)
  "Returns a set of notes for the specified chord. The root must be in
  midi note format i.e. :C4.

  (chord :c4 :major)  ; c major           -> #{60 64 67}
  (chord :a4 :minor)  ; a minor           -> #{57 60 64}
  (chord :Bb4 :dim)   ; b flat diminished -> #{70 73 76}
  "
  (if inversion
      (let* ((root (note root))
             (chord (resolve-chord chord-name))
             (notes (mapcar (lambda (x) (+ x root))
                            chord)))
        (invert-chord notes inversion))
      (chord root chord-name 0)))

(defun chord-degree (degree root mode &optional num-notes)
  "Returns the notes constructed by picking thirds in a given scale
  from in a given root. Useful if you want to try out playing standard
  chord progressions. For example:

  (chord-degree :i :c4 :ionian) ;=> (60 64 67 71)
  (chord-degree :ii :c4 :melodic-minor-asc) ;=> (62 65 69 72)
  "
  (if num-notes
      (let* ((d-int       (degree->int degree))
             (num-degrees (1- (+ d-int (* num-notes 2)))))
        (take-nth 2 (nthcdr (degree->int degree)
                            (ov-scale root mode (myrange num-degrees)))))
      (chord-degree degree root mode 4)))

(defun rand-chord (root chord-name
                   num-pitches pitch-range)
  "Generates a random list of MIDI notes with cardinality num-pitches
  bound within the range of the specified root and pitch-range and
  only containing pitches within the specified chord-name. Similar to
  Impromptu's pc:make-chord"
  (let* ((chord     (chord root chord-name))
         (root      (note root))
         (max-pitch (+ pitch-range root))
         (roots     (myrange max-pitch :step 12))
         (notes     (flatten (mapcar (lambda (x) (mapcar (lambda (y) (+ x y)) chord))
                                     roots)))
         (notes     (filter (lambda (x) (when (<= x max-pitch) x)) notes)))
    (sort (pick-random-list notes num-pitches) #'<)))

#|
Scales can be quickly generated using the scale function, which takes a root note and the type of scale as arguments.

whelmed.play> (scale :C3 :major)
(48 50 52 53 55 57 59 60)

whelmed.play> (SCALE :purvi)
[1 3 2 1 1 3 1]

whelmed.play> (degrees->pitches [:i :ii :iii] :dorian :E3)
(52 54 55)

Scale degrees can be augmented by either + or - to denote the octave above or below the root of the scale, and can be sharped or flatted using # or b.

whelmed.play> (degrees->pitches [:i :ii :ii+ :ii#] :dorian :E3)
(52 54 66 55)

https://github.com/overtone/overtone/blob/36221f68733fc5921aeb60a2a8b10e99426f236d/src/overtone/music/pitch.clj
|#

;;--------------------------------------------------

;; From overtone, fn.clj
;; cycle-fn????

;; from overtone, lists.clj
;; rotate, like alexandria:rotate-elt
;; fill a.k.a. repeat

;; from overtone, chance.clj
;; might be called pickln
(defun choose-n (n l)
  "Choose n random elements from l"
  (take n (alexandria:shuffle l)))

;; weighted-coin is like (cm:odds)
;; ranged-rand is like (cm:between)
;; choose/chosen-from is like (cm:pickl) or (alexandria:random-elt)
;; weighted-choose is like (cm:new cm:weighted)
(defun sputter (list &optional (prob .25) (max 100) (result '()))
  "Returns a list where some elements may have been repeated.

   Repetition is based on probabilty (defaulting to 0.25), therefore,
   for each element in the original list, there's a chance that it will
   be repeated. (The repetitions themselves are also subject to further
   repetiton). The size of the resulting list can be constrained to max
   elements (defaulting to 100).

  (sputter '(1 2 3 4))        ;=> (1 1 2 3 3 4)
  (sputter '(1 2 3 4) 0.7 5)  ;=> (1 1 1 2 3)
  (sputter '(1 2 3 4) 0.8 10) ;=> (1 2 2 2 2 2 2 2 3 3)
  (sputter '(1 2 3 4) 1 10)   ;=> (1 1 1 1 1 1 1 1 1 1)
  "
  (let ((head (first list))
        (tail (rest  list)))
    (if (and head (< (length result) max))
        (if (< (random 1.0) prob)
            (sputter (cons head tail)
                     prob max
                     (cons head result))
            (sputter tail
                     prob max
                     (cons head result)))
        (reverse result))))

;; sputter variation that always returns a list of the same length of the input
(defun stutter (list &optional (prob .25) (result '()))
  (let ((head (first list))
        (tail (rest  list))
        (max  (length list)))
    (if (and head (< (length result) max))
        (if (< (random 1f0) prob)
            (sputter (cons head tail)
                     prob max
                     (cons head result))
            (sputter tail
                     prob max
                     (cons head result)))
        (reverse result))))

;;--------------------------------------------------
;; CUSTOM
;;; Extra
(defun ov-pc-scale (scale-name-key)
  "> (ov-pc-scale :todi)
  (0 1 3 6 7 8 11)"
  (declare (type keyword scale-name-key))
  (butlast
   (loop :for i :in (cons 0 (cdr (assoc scale-name-key +scale+)))
      :with n = 0 :collect (incf n i))))

(defun list-scales ()
  "returns a list with the keyword names of all overtone scales"
  (mapcar #'car +scale+))
(defun list-in-list (small-list big-list)
  "returns T or NIL if SMALL-LIST is into BIG-LIST"
  (let ((len (length small-list)))
    (length= len (intersection small-list big-list))))
(defun guess-scale (l)
  "given a list L of midi notes returns a list of possible compatiple
   pitch classes for the notes given"
  (let* ((m12       (mod12 l))
         (scales    (list-scales))
         (pc-scales (mapcar (lambda (x) (ov-pc-scale x)) scales)))
    (remove NIL
            (mapcar (lambda (s p)
                      (when (list-in-list m12 p)
                        s))
                    scales
                    pc-scales))))
