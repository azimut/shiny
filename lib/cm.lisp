(in-package :shiny)

;;--------------------------------------------------
;; CM helpers
;;--------------------------------------------------
;;
;; Reference:
;; https://ccrma.stanford.edu/courses/220b-winter-2006/cm/doc/dict/patterns-topic.html
;;
;; TODO:
;; (make-markov)
;; (make-thunk)
;; (make-rewrite)
;; (make-transposer)
;; (make-chord)
;; (make-join)
;; (make-copier)

(defun make-cycle (elements &optional (for-elements 1 for-elements-p))
  "loops over elements in a continuous cycle"
  (when elements ;; do not make a cycle of ONLY nil (?
    (if for-elements-p
        (cm:new cm:cycle :of elements :for for-elements)
        (cm:new cm:cycle :of elements))))

(defun make-coin ()
  "returns a cycle that iterates between NIL and T"
  (make-cycle '(NIL T)))

(defun make-heap (elements &optional (for-elements 1 for-elements-p))
  (if for-elements-p
      (cm:new cm:heap :of elements :for for-elements)
      (cm:new cm:heap :of elements)))

(defun make-line (elements)
  "loops over elements in a continuous cycle"
  (cm:new cm:line :of elements))

(defun make-rotation (elements &optional (rotations '(0 1 1)) (for-elements 1 for-elements-p))
  "A constantly rotating pattern.
   > (next (make-rotation '(1 2 3 4)) 30)
   (1 2 3 4 2 3 4 1 3 4 1 2 4 1 2 3 1 2 3 4 2 3 4 1 3 4 1 2 4 1)"
  (declare (type list rotations))
  (if for-elements-p
      (cm:new cm:rotation :of elements
              :rotations rotations
              :for for-elements)
      (cm:new cm:rotation :of elements
              :rotations rotations)))

(defun make-accumulation (elements &optional (for-elements 1 for-elements-p))
  "For each item, generates all items up to and including the current item.
   The process starts over when all the items have been accumulated.
   > (cm:next (cm:new cm:accumulation :of '(1 2 3 4)) 10)
   (1 1 2 1 2 3 1 2 3 4)"
  (if for-elements-p
      (cm:new cm:accumulation :of elements :for for-elements)
      (cm:new cm:accumulation :of elements)))

(defun make-weighting (elements &optional (for-elements 1 for-elements-p))
  ;; try to add :weight keyword if only provided 2 elements
  ;; we push since order doesn't really matter...
  (let ((sane-elements '()))
    (loop
      :for e :in elements :do
         (cond ((and (listp e) (length= 3 e))
                (push e sane-elements))
               ((not (listp e))
                (push e sane-elements))
               ((and (listp e) (length= 2 e))
                (push (list (first-elt e) :weight (last-elt e))
                      sane-elements))))
    (if for-elements-p
        (cm:new cm:weighting :of sane-elements :for for-elements)
        (cm:new cm:weighting :of sane-elements))))

;; FIXME!!
(defun make-stutter (elements &optional (for 1))
  (cm:new cm:cycle
    :of elements
    :for (cm:new cm:cycle :of for)))

(defun make-palindrome (elements)
  "generates elements forwards and backwards"
  (cm:new cm:palindrome :of elements :elide t))

(defun make-range (from to &optional (by 1))
  "iterates numbers in a range"
  (cm:new cm:range :from from :to to :by by))

;; took from jazz.cm
(defun rancyc (data prob)
  (list (make-cycle data) :weight prob))

(defun markov-find-shortest (markov)
  (declare (markov markov))
  (let ((long-cycle (next markov 30)))
    (loop
      :for cycle :in long-cycle
      :with queue
      :while (not (position cycle queue))
      :finally (return queue)
      :do (push cycle queue))))

(defun make-graph (elements &optional (for 1 for-p))
  "The Graph pattern. A graph is a network of nodes, each node contains
  a 'value' (the item to return from the pattern), a 'link' to the
  node that should come next , and an identifier (a unique name for
  the node in the graph). Both the value and the link can be
  subpatterns . node identifiers default to increasing numbers from 1.

  Example:
  > (make-graph '((A 2) (B 3) (C 1))))"
  (let ((pat (loop
                :for id :from 1
                :for (elt to-id) :in elements
                :collect (list elt :id id :to to-id))))
    (if for-p
        (cm:new cm:graph :of pat :for for)
        (cm:new cm:graph :of pat))))

;;------------------------------------------------------------
;; TODO: sub-cycles, like (make-var '(4 2) '(1 2 (2 (3 4))))
;; Version that takes 2 lists
(defun make-var (elements &optional (for NIL for-p))
  "Returns a LIST of cycles and/or symbols.
   FOR NIL is usefult to play plain lists of cycles
   FOR =1 is useful for
   FOR >1 is useful to stutter ELEMENTS."
  (declare (type list elements))
  (if for-p
      (loop
         :for element :in elements
         :for f :in (repeat (length elements)
                            (ensure-list for))
         :collect
           (if (and (= f 1) (or (null element) (not (listp element))))
               element
               (cm:new cm:cycle :of element :for f)))
      (loop
         :for element :in elements
         :collect
           (if (or (null element) (not (listp element)))
               element
               (cm:new cm:cycle :of element)))))

(defun make-war (elements &optional (for NIL for-p))
  (declare (type list elements))
  (if for-p
      (loop
         :for element :in elements
         :for f :in (repeat (length elements)
                            (ensure-list for))
         :collect
           (if (and (= f 1) (or (null element) (not (listp element))))
               element
               (cm:new cm:weighting :of element :for f)))
      (loop
         :for element :in elements
         :collect
           (if (or (null element) (not (listp element)))
               element
               (cm:new cm:weighting :of element)))))

(defun make-har (elements &optional (for NIL for-p))
  (declare (type list elements))
  (if for-p
      (loop
         :for element :in elements
         :for f :in (repeat (length elements)
                            (ensure-list for))
         :collect
           (if (and (= f 1) (or (null element) (not (listp element))))
               element
               (cm:new cm:heap :of element :for f)))
      (loop
         :for element :in elements
         :collect
           (if (or (null element) (not (listp element)))
               element
               (cm:new cm:heap :of element)))))

;; Version that takes 1+N arguments
(defgeneric make-cycles (for &rest rest)
  (:documentation "returns a list of cycles or symbols using FOR as the :for param
   tries to avoid creating cycles when FOR=1
   kind of useful to mimic stutter OR creating alternating patterns
   a'la () in FoxDot")
  (:method ((for fixnum) &rest rest)
    (if (= 1 for)
        (mapcar (lambda (x) (if (listp x)
                           (cm:new cm:cycle :of x :for for)
                           x))
                rest)
        (mapcar (lambda (x) (cm:new cm:cycle :of (ensure-list x) :for for))
                rest)))
  (:method ((for list) &rest rest)
    (loop
       :for f :in for
       :for r :in rest :collect
         (if (= 1 f)
             r
             (cm:new cm:cycle
               :of (ensure-list r)
               :for f)))))

;;--------------------------------------------------
;; From SC
(defun make-shuf (elements &optional (for NIL for-p))
  "kind of Pshuf, shuffle the list in random order, and use
   the same random order FOR times"
  (if for-p
      (cm:new cm:copier
        :of (cm:new cm:heap :of elements)
        :for for)
      (cm:new cm:copier
        :of (cm:new cm:heap :of elements))))

;;--------------------------------------------------
;; From nudruz/rhythm.lisp

(defun upbeatcyc (minups maxups mindown maxdown)
  "UPBEATCYC -- returns upbeat cycle
   between 'minups' and 'maxups' upbeats of 1
   between 'mindown' and 'maxdown' length of downbeat
   > (next (upbeatcyc 2 5 10 20) 20)"
  (cm:new cm:weighting
    :of `((1 :min ,minups :max ,maxups)
	  (,(pval (between mindown maxdown)) :max 1))))

(defun cyc-rhythms (mlens subdivs)
  "CYC-RHYTHMS -- combines measures and subdivisions cyclically
   > (cyc-rhythms '(2 3 1.5) '(1 2))
   (2.0 1.5 1.5 1.5 1.0 1.0 3.0 0.75 0.75)"
  (let* ((subdivcyc (new cycle of subdivs))
	 (mlencyc (new cycle of mlens)))
    (loop :until (and (eop? subdivcyc)
		      (eop? mlencyc))
       :for this-subdiv = (next subdivcyc)
       :for this-mlen = (next mlencyc)
       :append (loop :repeat this-subdiv
	          :collect (float (/ this-mlen this-subdiv))))))

(defun combine-atks (rhythms atklist)
  "COMBINE-ATKS -- 'fuses' attacks within a rhythm
   'combiner' = number of attacks to combine (cycle)
   > (combine-atks '(1 2) '(1 1 3))
   (1 2 4 2 1 5)"
  (let ((rcyc   (cm:new cm:cycle :of rhythms))
	(atkcyc (cm:new cm:cycle :of atklist)))
    (loop :until (and (eop? rcyc)
                      (eop? atkcyc))
       :collect (apply #'+ (next rcyc (next atkcyc))))))

