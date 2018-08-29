(in-package :shiny)

(defn phrase
  "Translates a sequence of durations and pitches into a melody.
  - nil pitches signify rests,
  - vectors represent clusters
  - and maps represent chords.
  Vector durations represent repeated notes.
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

(defn utter [object time duration velocity]
  (cond
    (number? object)     [{:pitch object :time time :duration duration :velocity velocity}]
    (sequential? object) (mapcat #(utter % time duration velocity) object)
    (map? object)        (utter (-> object vals sort) time duration velocity)
    (nil? object)        [{:time time :duration duration}]))

(def voiceX
  (->>
    (phrase (cycle [3 1/2 1/2]) [[0 4] 5 7 [0 4] 5 7 9 8 7 7 nil nil])
    (then (phrase (cycle [3 1/2 1/2]) [[0 4] 5 7 [0 4] 5 4 [0 2] 1 0 0 nil nil]))
    (where :pitch scale/raise)
    (all :part :voice)))

(defun if-applicable (applies-p f)
  (lambda (x) (if (funcall applies-p x) (funcall f x) x)))

(defun wherever (applies-p k f notes)
  "Applies f to the k key of each note wherever condition? returns true.
  e.g. (->> notes (wherever (comp not :part), :part (is :piano))"
  (mapcar (if-applicable applies-p (lambda (x) (update-in x (list k) f)))
          notes))

(defun where (k f notes)
  (wherever))

(defun after (wait notes)
  (where ))

(defun then (later earlier)
  (->> later
    (a)))

(defstruct play-event
  pitch
  time
  duration
  velocity)

(defun utter (&optional (object nil object-p) time duration velocity)
  (cond
    ((numberp object)
     (list (make-play-event :pitch object :time time :duration duration :velocity velocity)))
    ((listp object) (mapcar (lambda (x) (utter x time duration velocity)) object))
    ;; (map?)
    (object-p
     (list (make-play-event :time time :duration duration)))))

(defun phrase (durations pitches velocities)
  (labels ((wrap (x)
             (if (listp x) x (list x))))
    (let* ((counts (mapcar (lambda (x) (length (wrap x))) durations))
           (normalised-pitches (mapcat #'repeat counts pitches))
           (normalised-durations (mapcat #'wrap durations))
           (times (reductions #'+ normalised-durations)))
      (mapcat #'utter normalised-pitches times normalised-durations velocities))))
