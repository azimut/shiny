(in-package :shiny)

;; Incudine

;; OPTIONAL
;;(set-rt-block-size 64)

;; --------------------------------------------------
;; Incudine replaced macro, using bare beat call instead of #[] (less clutter)
(defmacro aat (time function &rest arguments)
  (let* ((it    (intern "IT"))
         (delta (caddr time))
         (real  (cadr time))
         (next  `(+ ,real #[,delta b])))
    `(let ((,it ,next))
       (at ,it ,function ,@arguments))))


;; --------------------------------------------------


