(in-package :somecepl)

;; rotate list
;; https://programmingpraxis.com/2010/10/12/rotate-an-array/
(defun rotate (lst arg)
  (cond ((null lst) lst)
        ((= (mod arg (length lst)) 0) lst)
        (t (rotate (nconc (cdr lst) (list (car lst))) (- arg 1)))))

;; (define TWOPI (* 2.0 PI))
(defvar TWOPI 6.28)

;; cosr
;; https://groups.google.com/forum/#!searchin/extemporelang/cosr|sort:date/extemporelang/9O-yhrLQ-ag/MGbpKigwyDgJ
;; - the first argument is the "centre" to oscillate around
;; - the second argument is the "amplitude" of the oscillation
;; - the third argument is the "period" of the oscillation. 
;; Ex: (cosr 5 3 1/2)

;; (macro (cosr args)
;;    (if (> (length args) 4)
;;        `(+ ,(caddr args) (* ,(cadddr args) (cos (* TWOPI (+ beat ,(cadr args)) ,(car (cddddr args))))))
;;        `(+ ,(cadr args) (* ,(caddr args) (cos (* TWOPI beat ,(cadddr args)))))))
(defun cosr (centre amplitude period)
  (+ centre
     (* amplitude
        (cos (* TWOPI (now) period)))))

;; 
(defun random-list (mylist)
  (let* ((n (length mylist))
         (r (random n)))
    (nth r mylist)))

; From extempore - Scheme
;; (define note-name-to-midi-number
;;   (lambda (name)
;;     (let ((result (regex:matched name "([abcdefgABCDEFG])([#b])?(-?[0-9])")))
;;         (if (null? result)
;;             #f
;;             (let ((offset (+ 12 (* (string->number (cadddr result)) 12)))
;;                   (pc (case (modulo (- (modulo (char->integer (car (string->list (cadr result)))) 16) 3) 7)
;;                         ((0) 0) ((1) 2) ((2) 4) ((3) 5) ((4) 7) ((5) 9) ((6) 11))))
;;               (+ offset pc
;;                  (cond ((string=? (caddr result) "#") 1)
;;                        ((string=? (caddr result) "b") -1)
;;                        (else 0))))))))

;;scheme<7099> (regex:matched "F#1" "([abcdefgABCDEFG])([#b])?(-?[0-9])")
;;=> ("F#1" "F" "#" "1")  

(defun note-name-to-midi-number (name)
  (let ((result (multiple-value-bind (a b) (cl-ppcre:scan-to-strings "([abcdefgABCDEFG])([#b])?(-?[0-9])" name)
                  (append (list a) (coerce b 'list)))))
        (if (null result)
            nil
            (let ((offset (+ 12 (* (parse-integer (cadddr result)) 12)))
                  (pc    (case (mod (- (mod (char-code (coerce (cadr result) 'character)) 16) 3) 7)
                                ((0) 0) ((1) 2) ((2) 4) ((3) 5) ((4) 7) ((5) 9) ((6) 11) (otherwise 0))))
              (+ offset pc
                 (cond ((string= (caddr result) "#")  1)
                       ((string= (caddr result) "b") -1)
                       (t 0)))))))

