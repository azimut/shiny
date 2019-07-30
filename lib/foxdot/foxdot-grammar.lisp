(in-package #:shiny)

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

(yacc:define-parser foxdot-parser
  (:start-symbol expression)
  (:terminals
   (:left-pat    :right-pat    ;; <> - pattern (also cycle)
    :left-paren  :right-paren  ;; () - cycle
    :left-brace  :right-brace  ;; {} - random
    :left-square :right-square ;; [] - list
    :null :variable))
  (expression (term #'list) ;; NOTE: hardcoded list since it doesn't work otherwise
              (term expression
                    (lambda (a b)
                      (verbose:debug :foxdot "dig: ~a || ~a~%" a b)
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
                       (verbose:debug :foxdot "quote: ~a~%" x)
                       `(quote ,x)))
          cycle
          heap
          random
          group)
  (cycle  (:left-paren expression :right-paren
                       (lambda (_l e _r) (declare (ignore _l _r))
                         (verbose:debug :foxdot "cycle: ~a~%" e)
                         (cons 'mc e))))
  (heap   (:left-square expression :right-square
                        (lambda (_l e _r) (declare (ignore _l _r))
                          (verbose:debug :foxdot "heap: ~a~%" e)
                          (cons 'list e))))
  (random (:left-brace expression :right-brace
                       (lambda (_l e _r) (declare (ignore _l _r))
                         (verbose:debug :foxdot "random: ~a~%" e)
                         (cons 'mw e))))
  (group  (:left-pat   expression :right-pat
                       (lambda (_l e _r) (declare (ignore _l _r))
                         (verbose:debug :foxdot "group: ~a~%" e)
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
                              (verbose:debug :foxdot "forcing pattern between \< \>~%")
                              (format NIL "\<~a\>" pat))))
         (lisp-patterns (yacc:parse-with-lexer (foxdot-lexer raw-patterns)
                                               foxdot-parser))
         (cm-patterns   (mapcar #'eval lisp-patterns)))
    (if eval
        (if (length= 1 cm-patterns) (first cm-patterns) cm-patterns)
        (if (length= 1 lisp-patterns) (first lisp-patterns) lisp-patterns))))

(defun fx-pats (pat &optional (eval t))
  (ensure-list (fx-pat pat eval)))
