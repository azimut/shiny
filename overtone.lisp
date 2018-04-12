(in-package #:somecepl)
;; ----------- Overtone / Clojure

;; http://www.appservgrid.com/hyper/hyp/lisp#take
;; SOMECEPL> (take 2 '(0 1 2 3 4 5))
;; (0 1)

(defun take (n l)
  (cond ((< n 0) (error "index negative"))
        ((= n 0) ())
        ((null l) (error "index too large"))
        (t (cons (car l) (take (- n 1) (cdr l))))))

;; Taking a similar function to cycle
(defun repeat (n l &optional (nl '()))
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


#|
(defn utter [object time duration velocity]
  (cond
    (number? object)     [{:pitch object :time time :duration duration :velocity velocity}]
    (sequential? object) (mapcat #(utter % time duration velocity) object)
    (map? object)        (utter (-> object vals sort) time duration velocity)
    (nil? object)        [{:time time :duration duration}]))
|#
;; (defun utter (object time duration velocity)
;;   (cond
;;     (numberp object) 
;;     ()))

#|
(defn phrase

  "Translates a sequence of durations and pitches into a melody.
  nil pitches signify rests, vectors represent clusters, and maps
  represent chords. Vector durations represent repeated notes.
  e.g. (phrase [1/2 1/2 3/2 3/2] [0 1 nil 4])
  (phrase [1 1 2] [4 3 [0 2]])
  (phrase [1 [1 2]] [4 3])
  (phrase (repeat 4) (map #(-> triad (root %))) [0 3 4 3])"

  ([durations pitches velocities]
   (let [wrap (fn [x] (if (sequential? x) x [x]))
         counts (map (comp count wrap) durations)
         normalised-pitches (mapcat repeat counts pitches)
         normalised-durations (mapcat wrap durations)
         times (reductions + 0 normalised-durations)]
     (mapcat utter normalised-pitches times normalised-durations velocities)))
  ([durations pitches]
   (->> (phrase durations pitches (repeat nil))
        (map #(dissoc % :velocity)))))
|#
;; (defun phrase (durations pitches velocities)
;;   (let ((wrap   (lambda (x) (if (typep x 'list) x '(x))))
;;         (counts (mapcar (comp 'count 'wrap) durations))
;;         (normalised-pitches (mapcat repeat counts pitches))
;;         (normalised-durations (mapcat wrap durations))
;;         (times (reductions + 0 normalised-durations)))
;;     (mapcat utter normalised-pitches times normalised-durations velocities)))q

