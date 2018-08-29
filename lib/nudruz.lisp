(in-package :shiny)

;;
;; Nudruz
;;

;; Code copied from https://github.com/gogins/gogins.github.io/tree/master/nudruz

;; EXPWARP -- 'warps' pits by expt factor
;; (above optional bass-note, or lowest note in chd)
;; ME: added (round) to return value
(defun expwarp (pits factor &optional (bassnote nil))
      (let* ((orig-hz (remove-duplicates (cm:hertz pits)))
	     (bn (if bassnote bassnote (apply #'min orig-hz)))
	     (hzdiffs (mapcar (lambda (x) (- x bn)) orig-hz)))
	(loop for n to (- (length orig-hz) 1) collect
	      (round (cm:keynum
	       (+ bn (* (nth n hzdiffs) factor))
	       :hz 't)))))

;;--------------------------------------------------
;; Helpers
;;--------------------------------------------------

;;INTV -- interval between two notes (simple difference)
;; can be positive or negative
;; (intv 11 17) = 6
(defun intv (x y)
	(- y x))

;; MELINT -- list of melodic intervals within a list
;; "thanks Kenny Tilton!"
;; enhanced Feb. 2006 to also measure non-adjacent skips
;; example: (melint '(8 5 10 2)) = (-3 5 -8)
(defun melint (list &optional (skip 1))
  (mapcar #'intv list (nthcdr skip list)))


;; DIRECTIONS -- returns 1/-1 indices for melodic up/down
;(directions '(2 3 9 6 1 3)) = (1 1 -1 -1 1)
(defun directions (melody)
  (let ((mymelint (melint melody)))
    (loop for x in mymelint collect
          (if (eq x (abs x)) 1 -1))))

;; LISTSUB -- replaces 'olds' with 'news' in 'inlist' 
(defun listsub (news olds inlist)
  (sublis (pairlis olds news) inlist))

(defun random-codeword (codelen)
  (shuffle (cons 1 (loop repeat (- codelen 1) collect (pick 1 0)))))


;; NO-NILS -- removes all instances of 'nil from a list
;; fixed 'reverse' bug April 2006
(defun no-nils (a-list)
  (reverse
   (set-difference a-list '(nil))))


;; STEP-INCREM -- single move between lists
;; utility for 'fromto-stepper'
(defun step-increm (slist elist)
  (let* ((listdiffs (map 'list #'- elist slist))
         (onlydiffidx (no-nils (loop for n to (- (length listdiffs) 1) collect
                  (if (not (eql 0 (nth n listdiffs))) n))))
         (stepidx (pickl onlydiffidx)))
    (loop for n to (- (length slist) 1) collect
          (if (eql n stepidx) (+ (nth n slist) (/ (nth n listdiffs)
                                                 (abs (nth n listdiffs))))
                                 (nth n slist)))))


; FROMTO-STEPPER -- more refined 'fromto'
; avoids retracing steps from 'slist' to 'elist'
; [randomly selected set of steps] 
; (fromto-stepper '(0 9) '(5 6)) 
;  = ((0 9) (1 9) (2 9) (3 9) (4 9) (4 8) (5 8) (5 7) (5 6))
(defun fromto-stepper (slist elist)
  (let* ((listdiffs (map 'list #'- elist slist))
         (absdiffs (loop for x in listdiffs collect (abs x)))
         (totdiff (apply #'+ absdiffs)))
    (cons slist
          (when (plusp totdiff)
            (fromto-stepper (step-increm slist elist) elist)))))


;; MELINT->LINE -- builds a line from starting pitch & melint
;; --> adds intervals from the previous pitch
;; (melint->line 50 '(1 2 4)) = (50 51 53 57)
(defun melint->line (startnum int-vector)
  (cons startnum
        (when int-vector
          (melint->line (+ (first int-vector) startnum)
                        (cdr int-vector)))))

;; TRANSP -- applying 'op' of 'level' to number or list
;; level may be a rest [July 2008] 
(defun transp (input level &optional (op #'+))
  (if (eql level 'r) 'r
      (cond
	((eql input 'r) 'r)
	((numberp input) (funcall op input level))
	(t (mapcar (lambda (x) (transp x level op)) input))))) 


;; SEQ-EQL: whether two lists are identical
;; (seq-eql '(1 2 3) '(1 3 2)) = nil
;; (seq-eql '(1 2) '(1 2)) = T
(defun seq-eql (list1 list2)
  (not (mismatch list1 list2)))


;; FLATTEN -- removes all nesting in list
;; "thank you Paul Graham!"
(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))


;; DIVVY-UP -- simple utility used in 'ferney'
(defun divvy-up (mlen subdiv &optional (numtype 'float))
  (loop repeat subdiv collect 
        (if (eql numtype 'float) 
          (float (/ mlen subdiv))
          (/ mlen subdiv))))


;; FERNEY -- build list from mlens, subdivs, durs
;; now using modified 'ferneyrat' [Jan. 2006]
;; may specify rats or floats
;; cycling through everything
;; omitting durations will yield 'basic' mlens/subdivs list
;; Jan 2011: added 'treeflag'
;; (ferney '(2 3) '(1 2 4) '(2 4) 'float) = 
;; (3.5 3.0 3.5 3.5 1.5 5.5 1.0 5.5 1.5 5.0 2.0 4.5 2.0 3.0)
(defun ferney (mlens subdivs &optional (durs '(1)) (treeflag nil) (numtype 'rat))
  (let* ((mlens-cyc (new cycle of mlens))
         (subdivs-cyc (new cycle of subdivs))
         (durs-cyc (new cycle of durs))		
	 (ferntree
	  (loop until (and (cm:eop? mlens-cyc)
			   (cm:eop? subdivs-cyc))
	    collect (divvy-up (next mlens-cyc) 
			      (next subdivs-cyc)
			      numtype)))
         (flatcyc 
          (new cycle of
               (flatten ferntree))))
    (if treeflag ferntree
	(loop until (and (cm:eop? flatcyc) (cm:eop? durs-cyc))
	  collect (apply #'+ (next flatcyc (next durs-cyc)))))))


;; RANDVEC -- list of random mod-x integers (could include repeats)
;; as (random len &optional (modlen 12))
;; (randvec 5 3) = (2 1 2 0 0)
;; added Nov. 2005 
(defun randvec (len &optional (modlen 12) (transplevel 0))
  (let ((randpitrand (new weighting :of 
                          (loop for x from 0 to (- modlen 1) collect x))))
    (transp (loop repeat len collect (next randpitrand)) transplevel)))

;; FLATTER -- removes one level of tree (preserves sublists)
(defun flatter (alist)
  (loop for a in alist append a))


;; EQL-SUMMER -- all ways to sum componentnums to targetnum
;; if fails, returns targetnum
(defun eql-summer (targetnum componentnums)
  (let ((es (eqlsum targetnum componentnums)))
    (if es es (list targetnum))))


;; ONES&TWOS -- all seqs of 1&2 summing to a number
(defun ones&twos (anum)
  (remove-duplicates 
   (alexandria:map-permutations
    #'flatter
    (eql-summer anum '(1 2)))
   :test #'seq-eql))


;; PERMUTATIONS --  returns all permutations of a list
;; (defun permutations (a-list)
;;   (loop for x in 
;;         (cllib:permutations-list (make-array (length a-list) 
;;                                              :initial-contents a-list))
;;         collect (coerce x 'list)))
(defun permutations (list)
  (if list
    (mapcan #'(lambda (x)
		(mapcar #'(lambda (y) (cons x y))
			(permutations (remove x list))))
	    list)
    '(()))) ; else

;; N-OF-LIST -- utility for wiggle-to2
(defun n-of-list (n l)
  (if (= n 0) '()
      (cons (screamer:a-member-of l) (n-of-list (- n 1) l))))

;;; LIST-EQL: whether two lists have equal contents, irrespective of order
;; (list-eql '(1 2 3) '(1 3 2)) = T
(defun list-eql (list1 list2)
  (and (subsetp list1 list2) 
       (subsetp list2 list1)
       (= (length list1) (length list2))))


;; EQLSUM -- finds all combos of 'componentnums' that sum to 'targetnum'
(defun eqlsum (targetnum componentnums)
  (remove-duplicates
   (screamer:all-values
    (let* ((maxlen (ceiling (/ targetnum (reduce #'min componentnums))))
           (multvars (n-of-list (screamer:an-integer-between 0 maxlen) componentnums)))
      (unless (= (reduce #'+ multvars) targetnum) (screamer:fail))
      multvars))
   :test #'list-eql))

;; COPYLIST -- make copies of list
; (copylist '(3 4 5) 3) = (3 4 5 3 4 5 3 4 5)
;; outputs list or tree [July 2008] 
;; takes atom as input [May 2009]
(defun copylist (input mult &optional (notflat nil))
  (let* ((a-list (if (listp input) input (list input)))
	 (rawlist
	  (loop repeat mult collect a-list)))
    (if notflat rawlist (flatten rawlist))))

;; ALLROTS -- returns list of all rotations of list
;; (allrots '(58 60 54)) = ((58 60 54) (60 54 58) (54 58 60))
;; fixed May 2009 
(defun allrots (alist)
  (mapcar 
   (lambda (x) (rotate-list alist x))
   (indices (length alist))))




;; MAKE-POLY -- distributes line according to texture vector
;; (make-poly '(1 2 3 4 5 6) '(1 0 2)) = ((1) (R) (2 3) (4) (R) (5 6))
;; changed June 2005
;; Nov. 2005: added 0->R functionality
;; Aug. 2006: using 'likeflat' for better output
;; May 2009: 'fitflag'=t ends with complete texture, ie no partial textures
(defun make-poly (mel texture &optional (fitflag nil))
  (like-flat
   (if fitflag 
       (let* ((mel-len (length mel))
	      (txtlist
	       (copylist texture
			 (floor (/ mel-len 
				   (apply #'+ 
					  (flatten (not-flat texture))))))))
	 (loop for txt in txtlist
	       collect (no-nils
			(if (eql txt 0) (list 'r)
			    (loop repeat txt collect (pop mel))))))
       (let ((txtcyc (new cycle of texture)))
	 (loop while mel 
	   collect (no-nils
		    (let ((nxt-txt (next txtcyc)))
		      (if (eql nxt-txt 0) (list 'r)
			  (loop repeat nxt-txt collect (pop mel))))))))))


;; ALLSWAPS -- all pairwise 'place swaps' (permutations) in a list
(defun allswaps (alist)
  (let ((polys (cons 2 
		     (copylist (list 1) (- (length alist) 2)))))
    (loop for p in (allrots polys) collect
	  (flatten
	   (mapcar 
	    (lambda (x) (if (numberp x) x (reverse x)))
	    (make-poly alist p))))))

;; INDICES -- get a quick list of integers
;; (indices 4) = (0 1 2 3)
;; enhanced June 2005
(defun indices (len &optional (base 0))
  (transp (loop for n to (- len 1) collect n) base))

;; LIKE-FLAT -- converts all 1-lists to atoms; leaves the rest alone
;; August 2006
(defun like-flat (alist)
  (mapcar (lambda (x) (if (and (listp x) (eql 1 (length x))) 
			  (car x)
			  x))
	  alist))

;; NORESTS -- removes rests
(defun norests (alist)
   (no-nils (loop for x in alist collect
                  (if (not (eql x 'r)) x))))


;; NOT-FLAT -- makes everything into a list; leaves lists alone
;; (not-flat '(2 4 (3 4) 2 (2 1 4))) = ((2) (4) (3 4) (2) (2 1 4))
;; removing rests in chords (May 2007) 
;; makes atoms into lists (May 2009)
(defun not-flat (alist)
  (if (listp alist)
      (mapcar (lambda (x) (if (listp x) (norests x) (list x)))
	      alist)
      (list alist)))

;; SAFESORT -- non-destructive sort
(defun safesort (a-list)
  (let ((templist (loop for a in a-list collect a)))
    (sort templist #'<)))

;; RECURZ -- generalized recursion; returns chain 
;; 'input' always a list
(defun recurz (func input len)
  (cond ((eql len 1) input)
	((eql len 2) (list input (funcall func input)))
	(t 
	 (let ((prev (recurz func input (- len 1))))
	   (append prev (list (funcall func (car (last prev)))))))))

;;--------------------------------------------------
;; END Helpers
;;--------------------------------------------------

;;--------------------------------------------------
;; rhythms.lisp
;;--------------------------------------------------

;; CYC-RHYTHMS -- combines measures and subdivisions cyclically
(defun cyc-rhythms (mlens subdivs)
  (let* ((subdivcyc (new cycle of subdivs))
	 (mlencyc (new cycle of mlens)))
    (loop until (and (eop? subdivcyc)
		     (eop? mlencyc))
      for this-subdiv = (next subdivcyc)
      for this-mlen = (next mlencyc)
      append (loop repeat this-subdiv
	       collect (float (/ this-mlen this-subdiv))))))

;; UPBEATCYC -- returns upbeat cycle 
;; between 'minups' and 'maxups' upbeats of 1
;; between 'mindown' and 'maxdown' length of downbeat

;;(next (upbeatcyc 2 5 10 20) 20)
(defun upbeatcyc (minups maxups mindown maxdown)
  (new weighting :of `((1 :min ,minups :max ,maxups) 
                       (,(pval (between mindown maxdown)) :max 1))))

;; COMBINE-ATKS -- 'fuses' attacks within a rhythm 
;; 'combiner' = number of attacks to combine (cycle)

(defun combine-atks (rhythms atklist)
  (let ((rcyc (new cycle of rhythms))
	(atkcyc (new cycle of atklist)))
    (loop until (and (eop? rcyc) (eop? atkcyc))
      collect (apply #'+ (next rcyc (next atkcyc))))))

;; (combine-atks '(1 2) '(1 1 3))
;; = (1 2 4 2 1 5)


;;--------------------------------------------------
;; EOF rhythms
;;--------------------------------------------------

;; UDWN -- note-to-note utility for 'make-updown'
(defun udwn (startpit endpit udval &optional (modlen 12))
  (let* ((m-startpit (mod startpit modlen))
	 (m-endpit   (mod endpit modlen))
	 (modfloor   (floor (/ startpit modlen)))
	 (grtrflag   (> m-startpit m-endpit)))
    (case udval
      (1 (+ m-endpit
            (* modlen 
               (+ (if grtrflag 1 0) modfloor))))
      (-1 (+ m-endpit
             (* modlen 
                (+ (if grtrflag 0 -1) modfloor))))
      (t startpit))))

;; MAKE-UPDOWN -- applying +1/-1 contour to 'alist' from first pit 
(defun make-updown (alist udvals &optional (modlen 12))
  (if (eql 2 (length alist))
      (list (car alist) (udwn (car alist) (cadr alist) (car udvals) modlen))
      (cons (car alist)
	    (make-updown
	     (cons (udwn (car alist) (cadr alist) (car udvals) modlen)
		   (cddr alist))
	     (cdr udvals)
	     modlen))))

;; TAKE-UPDOWN - same as 'directions'
(defun take-updown (melody)
  (directions melody))

;; UPDOWN-OPPOSITE -- opposite signs
(defun updown-opposite (udvec)
  (mapcar (lambda (x) (* -1 x)) udvec))

;; UPDOWN-RANDOM -- random contour
(defun updown-random (len)
  (listsub '(-1 1) '(0 1) 
	   (random-codeword len)))

;; UPDOWN-CONVERSIONS -- list of updown p,i,r,ri
(defun updown-conversions (udvec)
  (list udvec
	(updown-opposite udvec)
	(reverse udvec)
	(reverse (updown-opposite udvec))))

;; MAKE-UPDOWN-RANDOM -- apply random updown
(defun make-updown-random (alist &optional (modlen 12))
  (make-updown alist 
	       (updown-random (- (length alist) 1))
	       modlen))

;; FLIP-CONTOUR -- change up/down directions
(defun flip-contour (melody)
  (make-updown melody 
	       (updown-opposite (take-updown melody))))

;;; CHOOSER -- returns members of 'alist' from list of indices 'idxs'
;; now treats rests -- Jan. 2008
;; now treats sublists -- July 2008
;; azimut: like my nths...
(defun chooser (idxs alist)
  (loop for n in idxs collect
	(if (listp n) (chooser n alist)
	    (if (eql n 'r) 'r
		(nth n alist)))))

; UPDOWN-FROMTO -- stepwise up/down contour change (incl first & last)
(defun updown-fromto (ud-start ud-end)
  (let* ((start01
	  (listsub '(0 1) '(-1 1) ud-start))
	 (end01
	  (listsub '(0 1) '(-1 1) ud-end)))
    (mapcar
     (lambda (x) (chooser x '(-1 1)))
     (fromto-stepper start01 end01))))


;; (all-updowns len) ?

;; neumes = three or four pitches
;; "neumatic transformations" (p. 34)

;; CRAW-INTS
;; each interval is replaced by its complement or compound
(defun craw-ints (neume &optional (modlen 12))
  (let* ((neumelint (melint neume))
	 (neumintlen (length neumelint)))
    (mapcar (lambda (x) (melint->line (car neume) x))
	    (loop for victim to (- neumintlen 1) append
		  (loop for trindx in '(1 -1) collect
			(loop for n to (- neumintlen 1) collect
			      (if (eql n victim)
				  (+ (nth n neumelint)
				     (* trindx modlen))
				  (nth n neumelint))))))))

;; CRAW-MELCONVERSIONS
(defun craw-melconversions (mymel)
  (let* ((mymint (melint mymel))
	 (mymintit (transp mymint -1 #'*)))
    (mapcar 
     (lambda (x) 
       (melint->line (car mymel) x))
     (list mymint
	   mymintit
	   (reverse mymint)
	   (reverse mymintit)))))

;; CRAW-CONV -- random melconversion
(defun craw-conv (amel)
  (pickl (craw-melconversions amel)))

;; CRAW-MULT -- all intervals multiplied by a constant
(defun craw-mult (amel mfactor)  
  (let* ((amint (melint amel)))
    (melint->line (car amel) (transp amint mfactor #'*))))

;; CRAW-ADD -- all intervals expanded/contracted by a constant
(defun craw-add (amel mfactor)  
  (let* ((amint (melint amel)))
    (melint->line (car amel) 
		  (map 'list #'*
		       (take-updown amel)
		       (transp (mapcar #'abs amint) mfactor)))))

;; CRAW-PE -- 'partial expansion' (all collected)
;; one interval expands by a semitone
(defun craw-pe (amel)
  (let* ((mint (melint amel))
	 (pevec
	  (mapcar (lambda (x)
		    (cond ((plusp x) (+ x 1))
			  ((minusp x) (- x 1))
			  (t x)))
		  mint))
	 (mlen (- (length mint) 1)))
    (mapcar
     (lambda (x) (melint->line (car amel) x))
     (reverse
      (set-difference 
       (loop for m to mlen collect
	     (loop for n to mlen collect
		   (if (= m n) (nth n pevec) (nth n mint))))
       (list mint) 
       :test #'seq-eql)))))

;; CRAW-PC -- 'partial expansion' (all collected)
;; one interval contracts by a semitone
(defun craw-pc (amel)
  (let* ((mint (melint amel))
	 (pevec
	  (mapcar (lambda (x)
		    (cond ((plusp x) (- x 1))
			  ((minusp x) (+ x 1))
			  (t x)))
		  mint))
	 (mlen (- (length mint) 1)))
    (mapcar
     (lambda (x) (melint->line (car amel) x))
     (reverse
      (set-difference 
       (loop for m to mlen collect
	     (loop for n to mlen collect
		   (if (= m n) (nth n pevec) (nth n mint))))
       (list mint) 
       :test #'seq-eql)))))

;; ALL-CRAW -- all transformations
;; useful for 'generic-path' & 'generic-branch'
(defun all-craw (amel &optional (modlen 12))
  (remove-duplicates  
   (append (craw-ints amel)
	   (craw-melconversions amel)
	   (loop for n from 1 to 7 collect
		 (craw-mult amel n))
	   (loop for n from -11 to 11 collect
		 (craw-add amel n))
	   (craw-pe amel))
   :test #'seq-eql))

;; ALL-CRAW-PE-PC -- all contractions + expansions 
(defun all-craw-pe-pc (amel)
  (remove-duplicates
   (append (craw-pe amel) (craw-pc amel))
   :test #'seq-eql))

;;

;; verse rhythms -- inspired by Crawford

(defun make-verse (len)
  (mapcar
   (lambda (x) (ferney '(1) x (pickl (cons x (ones&twos x)))))
   (randvec len 3 3)))

; verse variations

;; VERSE-SWAPS -- adjacent bars swapped (all possible)
;;> (verse-swaps '(60 62 64 67))
;; ((62 60 64 67) (60 64 62 67) (60 62 67 64))
(defun verse-swaps (averse)
  (chooser
   (allswaps (indices (length averse)))
   averse))

;; VERSE-DROPS -- single verse dropped (all possible)
;; > (verse-drops '(60 62 64 67))
;; ((62 64 67) (60 64 67) (60 62 67) (60 62 64))
(defun verse-drops (averse)
  (let ((verseidx (indices (length averse))))
    (mapcar 
     (lambda (x) (chooser x averse))
     (mapcar 
      (lambda (x) 
	(safesort
	 (set-difference verseidx (list x))))
      verseidx))))

; SWAPDROP-CHAIN -- recursive random swaps & drops
(defun swapdrop-chain (a-verse len)
  (no-nils
   (recurz 
    (lambda (x)
      (pickl (append (verse-swaps x)
		     (verse-drops x))))
    a-verse
    len)))
