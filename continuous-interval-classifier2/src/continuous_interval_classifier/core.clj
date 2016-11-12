(ns continuous-interval-classifier.core)

(defn- flip [func] (comp (partial apply func) reverse list))

(defprotocol Classification
  (priority [this] "Returns the priority of this classification. The priority is used to resolve conflicts. Higher priorities have higher precedence.")
  (next-interval-relative-to [this reference-point] "Returns the interval that classifies as this classification and is closest to the reference-point"))

(defprotocol ClassificationDomain
  "Types that implement this protocol can be used as values that can be classified.
   It is basically used to derive ordering of the domain."
  (successor [val] "Must return the successor of value"))

;; most common domain will be integers
(extend-protocol ClassificationDomain
  Number
  (successor [val] (+ 1 val)))

;; values of the classification domain must implement comparable
(defn <* [lhs rhs] (< (compare lhs rhs) 0))
(defn >* [lhs rhs] (> (compare lhs rhs) 0))

(defn minimum [& vals]
  (first (sort <* vals)))

(defn in-interval? [interval val]
  (let [lower (first interval)
        upper (second interval)]
    (and
     (or (= val lower) (>* val lower))
     (or (= val upper) (<* val upper)))))

(def upper-bound-relative-to
  "Returns the upper bound for an active classification relative to the reference point"
  (comp second next-interval-relative-to))

(def lower-bound-relative-to
  "Returns the lower bound for an active classification relative to the reference point"
  (comp first next-interval-relative-to))

(defn classifies?
  "Checks if the given valus falls into the given class"
  [cls value]
  (let [interval (next-interval-relative-to cls value)]
    (and interval (in-interval? interval value))))

(defn applicable-classes
  "returns the set of all applicable classes."
  [value possible-classes]
  (filter (partial (flip classifies?) value) possible-classes))

(defn classify
  "classifies the given value with the possible classes.
   It returns the applicable class that has the highest priority"
  [value possible-classes]
  (first (sort-by priority > (applicable-classes value possible-classes))))

(defn- interval-exhausted?
  "Checks if the current interval has been exhausted"
  [interval]
  (>* (first interval) (second interval)))

(defn- find-next-classification
  "finds the next class (nc) such that:
   1) nc starts after active-class
   2) nc has higher priority
   3) nc is not ac
   4) nc's start is closest to ac's start
   "
  [active-class active-lbound classes]
  (let [candidates (filter (fn [cls]
                             (let [lbound (lower-bound-relative-to cls active-lbound)]
                               (and
                                (not (= (:label active-class) (:label cls)))
                                (>   (priority cls) (priority active-class))
                                (not (nil? lbound))
                                (>*   lbound active-lbound))))
                           classes)]
    (first (sort-by #(lower-bound-relative-to % active-lbound) <* candidates))))

(defn classifies-until
  "Given a classification, determine upperbound of the interval where
   each value is classified as the given classification.
   It returns a value such that:
   value > start of current classification
   value < start of next interval that has a higher priority than the current classification
   value <= total-ubound
   value <= end of the current classification"
   [active-class active-lbound total-ubound classes]
   (let [active-ubound (upper-bound-relative-to  active-class active-lbound)
         candidate     (find-next-classification active-class active-lbound classes)]
     (if candidate
       (minimum total-ubound active-ubound (lower-bound-relative-to candidate active-lbound))
       (minimum total-ubound active-ubound))))

(defn classify-next-interval
  "Finds the class that is currently applicable along with the interval it is
  applicable for. It returns that classified interval and the remaining overall interval
  "
  [interval classes]
  (let [active-lbound (first  interval)
        total-ubound  (second interval)
        active-class  (classify active-lbound classes)]

    (when-not active-class
      (throw (Exception. "There is a gap in the interval that is not covered by a class.")))

    (let [active-ubound       (classifies-until active-class active-lbound total-ubound classes)
          classified-interval [active-lbound active-ubound active-class]]
      [classified-interval [(successor active-ubound) total-ubound]])))

(defn- classify-interval-impl [interval classes result]
  (if (interval-exhausted? interval)
    result
    (let [[classified-interval rest] (classify-next-interval interval classes)]
      (recur rest classes (conj result classified-interval)))))

(defn classify-interval
  "partitions the given interval into the set of classes, that
   apply to the subintervals.
  "
  [interval classes]
  (classify-interval-impl interval classes []))
