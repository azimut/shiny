(in-package :shiny)

;; https://github.com/Qirky/FoxDot/blob/master/FoxDot/lib/Utils/__init__.py
(defun pulses-to-durations (pulses)
  " Returns a list of durations based on pulses (1s) and blanks (0s).
    Data should be a list of [1,0] where 1 is a pulse."
  (loop :for i :in (cdr pulses)
        :for c :from 1
        :with seq = '()
        :with count = 1
        :finally (progn
                   (push count seq)
                   (return (reverse seq)))
        :do (if (= i 1)
                (progn
                  (push count seq)
                  (setf count 1))
                (incf count 1))))

;; https://github.com/Qirky/FoxDot/blob/master/FoxDot/lib/Patterns/Sequences.py
(defun pdur (n k &optional (start 0) (dur .25))
  "Returns the *actual* durations based on Euclidean rhythms (see PEuclid) where dur
        is the length of each step.
        ::
            >>> PDur(3, 8)
            P[0.75, 0.75, 0.5]
            >>> PDur(5, 16)
            P[0.75, 0.75, 0.75, 0.75, 1]"
  ;; If we have more pulses then steps, double the steps and decrease the duration
  (declare (type unsigned-byte start))
  (loop :while (> n k)
        :do (mulf k 2)
            (divf dur 2))
  (let ((pattern (pulses-to-durations (bjorklund n k))))
    (when (not (= 0 start))
      (setf pattern (alexandria:rotate pattern start)))
    (mapcar (lambda (x) (* dur x)) pattern)))

;; TODO: Player methods, they are like the pattern modifiers below
;;       but applied to a playing pattern.
;; https://github.com/Qirky/FoxDot/blob/master/FoxDot/lib/Players.py

;;--------------------------------------------------
;; TODO: Player Patterns
;;
;; <> - To separate patterns you want to play together.
;; () - Grouping characters in round brackets laces the pattern so
;;      that on each play through of the sequence of samples, the next
;;      character in the group's sample is played.
;; [] - Using square brackets will force the enclosed samples to
;;      played in the same time span as a single character e.g. `--[--]`
;;      will play two hi-hat hits at a half beat then two at a quarter
;;      beat.
;; {} - You can play a random sample from a selection by using curly
;;      braces in your Play String
;; || - play a sample NR , being x the sample symbol and N the number of
;;      the sample as in |xN| also possible to use () [] {}
;;--------------------------------------------------
;; Pattern methods
;; http://docs.foxdot.org/docs/patterns/pattern-methods/
;; https://github.com/Qirky/FoxDot/blob/master/FoxDot/lib/Patterns/Main.py
;;------------------------------
;; shuffle(n=1) =~
;; (cm:shuffle list) =~
;; (next (make-weighting '(1 2 3)) 10)
;;
;; Returns the pattern with it’s contents in a random order and n is
;; the number of permutations:
(defun fx-shuffle (l)
  (cm:shuffle l))
;;------------------------------
;; reverse() =~ (reverse)
;; Returns the pattern with its contents in reverse order. Nested
;; patterns / groups are *not* reversed.
(defun fx-reverse (l)
  (reverse l))
;;------------------------------
;; mirror()
;; Returns a pattern with its contents in reverse order, including
;; nested patterns and groups:
;;------------------------------
;; sort(*args, **kwargs) =~ (sort)
;; Returns a pattern with the values sorted in order. The args and
;; **kwargs are those that are supplied to Python’s builtin sorted
;; function but this returns a Pattern as opposed to a list.
(defun fx-sort (l f)
  (sort l f))
;;------------------------------
;; stutter(n=2)
;; Returns a new pattern with each value repeated n number of
;; times. If n is a pattern itself, then each value is repeated by the
;; number at the same index in the given pattern.
(defgeneric fx-stutter (l n)
  (:documentation
   "> (stutter '(1 2 3) 2)
    (1 1 2 2 3 3)

    > (stutter '(1 2 3 4) '(1 3))
    (1 2 2 2 3 4 4 4)")
  (:method ((l list) (n fixnum))
    (loop :for item :in l
          :append (loop :repeat n :collect item)))
  (:method ((l list) (n list))
    (loop :for item :in l
          :append (prog1 (loop :repeat (car n) :collect item)
                    (setf n (alexandria:rotate n)))))
  (:method (l (n fixnum))
    (repeat n (ensure-list l))))
;;------------------------------
;; arp(seq)
(defun fx-arp (l n)
  "Returns a new pattern with each item repeated len(seq) times and
   incremented by the values in seq. Useful for arpeggiating.
   > (arp '(0 1 2 3) '(0 4))
   (0 4 1 5 2 6 3 7)"
  (declare (type list n l))
  (loop :for note :in l
        :append (loop :for offset :in n :collect (+ note offset))))
;;------------------------------
;; splice(seq, *seqs)
;; Takes one or more patterns to “splice” into the original
;; pattern. The new pattern returned is made up of the values from the
;; original and given sequences in an alternated fashion.
;; https://github.com/vseloved/rutils/blob/master/core/list.lisp
(defun fx-splice (list &rest lists)
  "Return a list whose elements are taken from LIST and each of LISTS like this:
   1st of list, 1st of 1st of lists,..., 1st of last of lists, 2nd of list,...
   > (splice '(0 1 2 3) '(a b c))
   (0 A 1 B 2 C)"
  (apply #'mapcan (lambda (&rest els)
                    els)
         list lists))
;;------------------------------
;; invert()
(defun fx-invert (l)
  "Creates an inverted version of pattern by subtracting its values
   from the largest value in the pattern such that the largest value
   in the pattern becomes the smallest (and vice versa) and the
   difference between other values and the min/max are swapped:
   > (fx-invert '(2 5 1 11))
   (9 6 10 0)"
  (declare (type list l))
  (let ((max (extremum l #'>)))
    (loop :for item :in l
          :collect (- max item))))
;;------------------------------
;; shufflets(n = 4)
(defun fx-shufflets (l n)
  "Returns a new pattern that contains the original pattern as a
   PGroup in random order of length n.
   > (fx-shufflets '(1 2 3 4) 3)
   ((4 1 3 2) (3 1 4 2) (3 2 1 4))"
  (declare (type list l) (type unsigned-byte n))
  (loop :repeat n
        :collect (cm:shuffle l)))
;;------------------------------
;; pivot(i)
(defun fx-pivot (l n)
  "Returns a new pattern that is a reversed version of the original
   but then rotated such that the element at index i is still in the
   same place."
  (declare (type list l) (type unsigned-byte n))
  (let* ((len (length l))
         (mid (/ len 2)))
    (if (> n mid)
        (progn
          (setf n (1- (- len n)))
          (setf l (rotate (reverse l) (1+ (* 2 (mod n len))))))
        (setf l (reverse (rotate l (1+ (* 2 (mod n len)))))))
    l))
;;------------------------------
;; accum(n=None)
(defun fx-accum (l &optional (n (length l)))
  "Returns a pattern that is equivalent to the list of sums of that
   pattern up to that index (the first value is always 0). The
   argument n specifies the length of the new pattern. When n is None
   then the new pattern takes the length of the original.
   > (fx-accum '(1 2 3 4) 8)
   (0 1 3 6 10 11 13 16)"
  (declare (type list l) (type unsigned-byte n))
  (loop :for i :in (repeat (1- n) l)
        :with s = '(0)
        :with prev = 0
        :finally (return (reverse s))
        :do (let ((current (+ i prev)))
              (push current s)
              (setf prev current))))
;;------------------------------
;; stretch(size)
(defun fx-stretch (l n)
  "Returns a pattern that is repeated until the length is equal to size."
  (declare (type list l) (type unsigned-byte n))
  (repeat n l))
;;------------------------------
;; trim(size)
(defun fx-trim (l n)
  "Like stretch but the length cannot exceed the length of the
   original pattern."
  (declare (type list l) (type unsigned-byte n))
  (repeat (min (length l) n) l))
;;------------------------------
;; ltrim(size)
(defun fx-itrim (l n)
  "Like trim but removes items from the start of the pattern, not the end."
  (declare (type list l) (type unsigned-byte n))
  (reverse (repeat (min (length l) n) (reverse l))))
;;------------------------------
;; loop(n)
;;------------------------------
;; duplicate(n)
;; Like loop but retains the nested patterns such that the first value
;; in the nests are used on the first iteration through the duplicated
;; sequence etc.
(defun fx-duplicate (l n)
  "Repeats the pattern n times. Useful when chaining together multiple
   patterns. Nested patterns are taken into consideration when
   looping."
  (let* ((len (length l))
         (n2  (* len n)))
    (repeat n2 l)))
;;------------------------------
;; iter
;; Like loop but does not take nested patterns into account when
;; calculating the length.
;;------------------------------
;; swap(n)
;; Swaps the places of values in the pattern. When n is 2 then values
;; next to each other are swapped, when n is 3 then values next but 1
;; are swapped, and so on.
;; FIXME: returns a shorter list than FoxDot
(defun fx-swap (l n)
  (loop :for d :from 0 :by n
        :for u :from n :by n :to (length l)
        :append (reverse (subseq l d u))))
;;------------------------------
;; rotate(n)
;; Returns a pattern with the original pattern’s values shifted left
;; in order by n number of places. Negative numbers shift the values
;; right.

(defgeneric fx-rotate (l n)
  (:method ((l sequence) (n fixnum))
    (alexandria:rotate l n))
  (:method ((l vector) (n list))
    (loop :for rot :in n
          :append (coerce (alexandria:rotate l (- rot))
                          'list)))
  (:method ((l list) (n list))
    (loop :for rot :in n
          :append (alexandria:rotate l (- rot)))))
;;------------------------------
;; sample(n) =~ (choose-n) =~ (pickn)
(defun fx-sample (l n)
  "Returns an n-length pattern with randomly selected values from the
   original pattern."
  (subseq (alexandria:shuffle l) 0 n))
;;------------------------------
;; palindrome()
;; Appends the reverse of a pattern onto itself such that is creates a
;; palindrome of numbers.
(defun fx-palindrome (l)
  (appendf l (reverse l)))
(defun fx-palindrome2 (l)
  (appendf l (reverse (butlast l))))
;;------------------------------
;; alt(seq)
;; Replaces the pattern with that of seq. Useful if you want to use an
;; alternate pattern and assign it using the every method.
;;
;; !???
;;------------------------------
;; norm()
;; Returns a pattern with all the values normalised such that every
;; value in the new pattern is between 0 and 1.
(defun fx-norm (l)
  (let ((max (* 1f0 (extremum l #'>))))
    (loop :for item :in l :collect
             (/ item max))))
;;------------------------------
;; undup()
;; Removes any consecutive duplicate values so that there are no
;; repeated values in the pattern.
(defun fx-undup (l)
  (remove-duplicates l))
;;------------------------------
;; limit(func, value)
;; Returns a new pattern generated by appending values from the
;; original until func(pattern) exceeds value. The func argument must
;; be a valid function, such as len or sum.
;;------------------------------
;; replace(sub, repl)
;; Returns a new pattern with values equal to sub replaced with repl.
(defun fx-replace (l sub repl)
  (substitute repl sub l))
;;------------------------------
;; submap(mapping)
;; Similar to replace but takes a dictionary of sub to repl values to
;; replace multiple items.
;; FIXME? do it backwars? Iterate and then lookup?
(defun fx-submap (l alist)
  (declare (type list l) (type cons alist))
  (loop :for (sub . repl) :in alist
        :collect (setf l (substitute repl sub l)))
  l)
;;------------------------------
;; layer(method, *args, **kwargs)
;; Zips the original pattern with itself but with method called on
;; itself, which must be a string name of a valid pattern method
;; (similar to Player.every). However, the method argument can also be
;; a function (example below).
;;------------------------------
;; every(n, method, *args, **kwargs)
;; Repeats the original pattern n times and applies the Pattern method
;; (specified as a string) on the last repetition with args and kwargs
;; supplied.
;;------------------------------
;; map(callable)
;; Returns a new Pattern with the callable argument called on every
;; item in the pattern.
;;------------------------------
;; extend(seq)
;; Extends the Pattern with seq in place i.e. it returns None as
;; opposed to a new Pattern. This is more efficient than the concat
;; method for combining multiple sequences into one Pattern.
;;------------------------------
;; concat(seq)
;; Returns a new Pattern with the contents of seq appended onto the
;; end of the original Pattern. The special __or__ method (which uses
;; the | syntax) also calls this method.
;;------------------------------
;; zip(seq)
;; “Zipping” is the process of combining two sequences into one where
;; each element is a group that contains the items from each sequence
;; at the same index. If the sequences are of different lengths then
;; then they are zipped up to the length of the lowest common multiple
;; of both lengths.
;;------------------------------
;; offadd(value, dur=0.5)
;; Adds value to the Pattern, zips with the original, and delays the
;; zipped value by dur using the PGroupPrime class.
;;------------------------------
;; offmul(value, dur=0.5)
;; Similar to offadd but multiplies the values as opposed to adding.
;;------------------------------
;; offlayer(method, dur=0.5, *args, **kwargs)
;; Similar to offadd and offmul but uses a user-specified method or
;; function instead of addition/multiplication. The method argument
;; must be a valid name of a Pattern method as a string or a callable
;; object such as a function. Any extra arguments or keyword arguments
;; are supplied after the duration to delay the layer, therefore
;; duration must be supplied if supplying arguments as part of *args.
;;------------------------------
;;  amen()
;; Replicates the rhythm and order of the famous “amen break” based on
;; a kick-drum, hi-hat, snare-drum, hi-hat sequence. Listen to the
;; example below:

;; (defun amen (pat &optional (size 2))
;;   "merges and laces the first and last two items such that a
;;    drum pattern \"x-o-\" would become \"(x[xo])-o([-o]-)\""
;;   (let ((new '()))
;;     (loop :for n :in (myrange (lcm (length pat) 4))
;;        :do )))

;; TODO: a more similar mapping of file and sample number.
(defparameter *fx-samples* (make-hash-table)
  "Hash with array of buffers.")
(defvar *fx-path* "/home/sendai/projects/FoxDot/FoxDot/snd/"
  "Directory where to search for FoxDot samples, overwrite as needed.")

;; lib/Buffers.py
(defparameter *fx-non-alpha-sounds*
  '(("semicolon"    . ":")
    ("ampersand"    . "&")
    ("asterix"      . "*")
    ("at"           . "@")
    ("bar"          . "|")
    ("caret"        . "^")
    ("colon"        . ":")
    ("dollar"       . "$")
    ("equals"       . "=")
    ("exclamation"  . "!")
    ("forwardslash" . "/")
    ("greaterthan"  . ">")
    ("hash"         . "#")
    ("hyphen"       . "-")
    ("lessthan"     . "<")
    ("percent"      . "%")
    ("plus"         . "+")
    ("question"     . "?")
    ("tilde"        . "~")
    ("backslash"    . "\\")
    ("1"            . "1")
    ("2"            . "2")
    ("3"            . "3")
    ("4"            . "4")))

(defun fx-guess-path (path)
  (if (uiop:absolute-pathname-p path)
      path
      (concatenate 'string *fx-path* path)))

(defun fx-clear (&optional global-clear)
  "NOTE: to free buffers from memory use (clean-buffers)"
  (clrhash *fx-samples*)
  (when global-clear
    (clean-buffers))
  t)

(defun fx-load-simple (path key)
  "DEPRECATED
   loads .wav in PATH into global buffer cache under KEY
   KEY is symbolicated, to for example keep an uppercase???
   PATH is relative to *FX-PATH* or absolute."
  (when-let* ((path   (fx-guess-path path))
              (buf    (bbuffer-load path key)))
    buf))

(defun fx-load-all ()
  (loop :for dir :in (uiop:subdirectories *fx-path*)
        :for letter := (lastcar (pathname-directory dir))
        :if (str:alphap letter)
        :do (fx-load (merge-pathnames dir "lower") letter)
            (fx-load (merge-pathnames dir "upper") (str:upcase letter)))
  (loop :for dir :in (uiop:subdirectories (merge-pathnames *fx-path* "_"))
        :for letter := (lastcar (pathname-directory dir))
        :for letter-symbol := (cdr (assoc letter *fx-non-alpha-sounds* :test #'equal))
        :do (fx-load dir letter-symbol)))

(defun fx-load (path key)
  "loads .wav in PATH into the *FX-SAMPLES* hash cache under KEY
   VALUE is and extendable array that keeps each sample for KEY
   PATH can be a .wav file or a directory"
  (let ((resolved-path (fx-guess-path path))
        (key (if (stringp key) (char key 0) key)))
    (cond ((uiop:directory-exists-p resolved-path)
           (remhash key *fx-samples*)
           (loop :for file :in (uiop:directory-files ;; ensure trailing slash
                                (uiop:directory-exists-p resolved-path))
                 :do (fx-load file key)))
          ((uiop:file-exists-p resolved-path)
           (let ((buffer (bbuffer-load resolved-path)))
             (if-let ((buffers (gethash key *fx-samples*)))
               (vector-push-extend buffer buffers)
               (let ((init (make-array 1 :initial-contents `(,buffer)
                                         :adjustable T :fill-pointer 1)))
                 (setf (gethash key *fx-samples*) init)))))
          (t (error "PATH provides not valid...")))))

(defun fx-buf (symbol &optional (sample 0))
  "returns buffer under symbol on *FX-SAMPLES*"
  (when-let* ((elements (gethash symbol *fx-samples*))
              (n-elements (1- (fill-pointer elements)) )
              (buf (aref elements (if (= n-elements 0)
                                      0
                                      (mod sample n-elements)))))
    buf))

(defun resolve-patterns-in-list (list)
  (declare (type list list))
  (if (some #'cm:pattern? list)
      (resolve-patterns-in-list (mapcar #'cm:next list))
      list))

(defun fx-play (key &rest rest &key (sample 0) (dur 1) &allow-other-keys)
  (etypecase key
    (cm::pattern (apply #'fx-play (next key) rest)) ;; when this happens?
    (string
     (apply #'fx-play (char key 0) rest))
    (character
     (when-let* ((buffers   (gethash key *fx-samples*))
                 (n-buffers (fill-pointer buffers))
                 (buffer    (aref buffers (mod sample n-buffers))))
       (remf rest :sample)
       (remf rest :dur)
       (apply #'bbplay buffer rest)))
    (list
     (when-let* ((_        key)
                 (sounds   (resolve-patterns-in-list key))
                 (n-sounds (length sounds))
                 (offsets  (loop :repeat n-sounds
                                 :for i :from 0d0 :by (/ (coerce dur 'double-float)
                                                         n-sounds)
                                 :summing i :into total
                                 :collect total)))
       (mapcar (lambda (n o)
                 (declare (type double-float o))
                 (apply #'at
                        (+ (now) (calc-beats o))
                        #'fx-play
                        n
                        rest))
               key
               offsets)))))

;;--------------------------------------------------
;; Pattern support similar to the one that Foxdot's
;; play() provides

(cl-lex:define-string-lexer foxdot-lexer
  ("[A-Za-z-@_*+~/:&|^$=!/#%?~\\\\.1234]"
   (return (values :variable     (char $@ 0))))
  ("<"             (return (values :left-pat     :left-pat)))
  (">"             (return (values :right-pat    :right-pat)))
  ("\\["           (return (values :left-square  :left-square)))
  ("\\]"           (return (values :right-square :right-square)))
  ("\\("           (return (values :left-paren   :left-paren)))
  ("\\)"           (return (values :right-paren  :left-paren)))
  ("{"             (return (values :left-brace   :left-brace)))
  ("}"             (return (values :right-brace  :right-brace)))
  ("\\s"           (return (values :null         NIL))))

(defun lex-line (string)
  "REPL helper to test lexer"
  (loop :with lexer := (foxdot-lexer string)
        :for  tok   := (funcall lexer)
        :while tok
        :collect tok))

;;--------------------------------------------------

(defun mc (&rest elements)
  (cm:new cm:cycle :of elements :for 1))
(defun mw (&rest elements)
  (let ((weighted-elements ;; accept plain lists, like for []
          (loop :for e :in elements
                :collect (list e :weight 1))))
    (cm:new cm:weighting :of weighted-elements :for 1)))
(defun mh (&rest elements)
  ;;(cm:new cm:heap :of elements :for 1)
  elements)

;; Took from comments on clojure's flatten page
(defun flat-1 (list)
  (mapcat (alexandria:compose #'ensure-list #'identity)
          list))

;; Shitty version that only accepts () and {},
;;  plus [] for heaps?
(yacc:define-parser foxdot-parser
  (:start-symbol expression)
  (:terminals
   (:left-pat    :right-pat    ;; <> - pattern (also cycles)
    :left-paren  :right-paren  ;; () - cycle
    :left-brace  :right-brace  ;; {} - random
    :left-square :right-square ;; [] - heap
    :null :variable))
  (expression (term #'list) ;; NOTE: hardcoded list since it doesn't work otherwise
              (term expression
                    (lambda (a b)
                      (format t "dig: ~a || ~a~%" a b)
                      (cond ((and (atom  a) (listp b)) (append (list a) b))
                            ((and (listp a) (atom  b)) (append a (list b)))
                            ((and (atom  a) (atom  b)) (list a b))
                            ;; NOTE: in the case the thing to put together are
                            ;; two different patterns put them together list,
                            ;; otherwise just cons them.
                            ;; FIXME: might be this can part of the grammar???
                            ((and (listp a) (listp b)
                                  (not (listp (car b)))
                                  (not (length= 1 (symbol-name (car b)))))
                             (list a b))
                            (t (cons a b))))))
  (term   :null
          (:variable (lambda (x)
                       (format t "quote: ~a~%" x)
                       `(quote ,x)))
          cycle
          heap
          random
          group)
  (cycle  (:left-paren expression :right-paren
                       (lambda (_l e _r) (declare (ignore _l _r))
                         (format t "cycle: ~a~%" e)
                         (cons 'mc e))))
  (heap   (:left-square expression :right-square
                        (lambda (_l e _r) (declare (ignore _l _r))
                          (format t "heap: ~a~%" e)
                          (cons 'list e))))
  (random (:left-brace expression :right-brace
                       (lambda (_l e _r) (declare (ignore _l _r))
                         (format t "random: ~a~%" e)
                         (cons 'mw e))))
  (group  (:left-pat   expression :right-pat
                       (lambda (_l e _r) (declare (ignore _l _r))
                         (format t "group: ~a~%" e)
                         (cons 'mc e)))))


(defun fx-pat (pat &optional (eval t))
  (let* (;; given that we are going to get a cycle anyway
         ;; we just force a cycle around the pattern given
         ;; avoiding having to update the grammar (HACKS!)
         ;;(pattern pat (format NIL "(~a)" pat))
         ;;(pattern pat)
         ;; NOTE: Naive fix of input string
         (raw-patterns  (if (and (char= #\< (aref pat 0))
                                 (char= #\> (aref pat (1- (length pat)))))
                            pat
                            (progn
                              (format t "NOTE: forcing pattern between \< \>~%")
                              (format NIL "\<~a\>" pat))))
         (lisp-patterns (yacc:parse-with-lexer (foxdot-lexer raw-patterns)
                                               foxdot-parser))
         (cm-patterns   (mapcar #'eval lisp-patterns)))
    (if eval
        (if (length= 1 cm-patterns) (first cm-patterns) cm-patterns)
        (if (length= 1 lisp-patterns) (first lisp-patterns) lisp-patterns))))

(defun fx-pats (pat &optional (eval t))
  (ensure-list (fx-pat pat eval)))

(defun fx-parse (sym)
  (declare (type symbol sym))
  (if (eq '- sym)
      NIL
      T))

;; TimeVar has a series of values that it changes between after a
;; pre-defined number of beats and is created using a var object with
;; the syntax
;; var([list_of_values],[list_of_durations]).
;;
;; TODO: subvars on vars parameter. var([0,5,2[3,6]],[8,6,1,1])
(defun var (vars n-beats)
  "Returns a function, that returns a value from VARS, a new one if N-BEATS have passed.
   > (defvar *var1* (var '(1 2) 4))
   > (funcall *var1*)
   1
   > (cm:next *var1*)
   2"
  (declare (type list vars))
  (let ((start-beat (beat))
        (current-beat  0d0)
        (elapsed-beats 0d0)
        (var-index  0)
        (beat-index 0)
        (beats (ensure-list n-beats)))
    (lambda ()
      (setf current-beat  (beat))
      (setf elapsed-beats (- current-beat start-beat))
      ;;
      (when (> elapsed-beats (nth beat-index beats))
        (setf start-beat current-beat)
        (setf var-index  (mod (1+ var-index)  (length vars)))
        (setf beat-index (mod (1+ beat-index) (length beats))))
      (nth var-index vars))))

;; Like csound nth-beat but accepts 2 lists
(defun ivar (vars at-beats)
  "Inplace var()"
  (let* ((vars      (ensure-list vars))
         (n-vars    (length vars))
         (at-beats  (repeat n-vars (ensure-list at-beats)))
         (beat      (beat))
         (sum       (reduce #'+ at-beats))
         (curr-beat (mod beat sum))
         (sum-beats (loop :for i :in at-beats :summing i :into all :collect all))
         (var-pos   (position-if (lambda (n) (< curr-beat n))
                                 sum-beats)))
    (nth var-pos vars)))

(defun linvar (vars n-beats)
  (declare (type list vars))
  (let ((start-beat (beat))
        (current-beat  0d0)
        (elapsed-beats 0d0)
        (var-index  0)
        (beat-index 0)
        (beats (ensure-list n-beats)))
    (lambda ()
      (setf current-beat  (beat))
      (setf elapsed-beats (- current-beat start-beat))
      ;;
      (when (> elapsed-beats (nth beat-index beats))
        (setf start-beat current-beat)
        (setf var-index  (mod (1+ var-index)  (length vars)))
        (setf beat-index (mod (1+ beat-index) (length beats))))
      (alexandria:lerp (/ elapsed-beats (nth beat-index beats))
                       (nth 0 vars)
                       (nth 1 vars)))))

;; FIXME: fix lerp from 0-1 to 0-1-0
(defun ilinvar (vars n-beat)
  "Inplace linvar()"
  (declare (type list vars)
           (type number n-beat))
  (assert (length= 2 vars))
  (let ((beat (mod (beat) n-beat)))
    (coerce (alexandria:lerp (/ beat n-beat)
                             (nth 0 vars)
                             (nth 1 vars))
            'single-float)))

(defun often ()
  (= (mod (beat) 32)
     (serapeum:random-in-range 1 8)))

(defun sometimes ()
  (and (= (mod (beat) 32)
          (serapeum:random-in-range 8 32))))

(defun rarely ()
  (= (mod (beat) 32)
     (serapeum:random-in-range 32 64)))

;;.sometimes("amp.trim", 3)

;; def often(self, *args, **kwargs):
;; """ Calls a method every 1/2 to 4 beats using `every` """
;; return self.every(PRand(1, 8)/2, *args, **kwargs)

;; def sometimes(self, *args, **kwargs):
;; """ Calls a method every 4 to 16 beats using `every` """
;; return self.every(PRand(8, 32)/2, *args, **kwargs)

;; def rarely(self, *args, **kwargs):
;; """ Calls a method every 16 to 32 beats using `every` """
;; return self.every(PRand(32, 64)/2, *args, **kwargs)

;; def every(self, n, cmd, args=()):
;;   def event(f, n, args):
;;     f(*args)
;;     self.schedule(event, self.now() + n, (f, n, args))
;;     return
;;   self.schedule(event, self.now() + n, args=(cmd, n, args))
;;   return

