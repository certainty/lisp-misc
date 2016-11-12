(ns continuous-interval-classifier.core-test
  (:require [clojure.test :as test]
            [continuous-interval-classifier.core :as cic])
  (:use clj-time.core clj-time.coerce))


(defn includes? [interval val]
  (let [lower (first interval)
        upper (second interval)]
    (and (>= val lower) (<= val upper))))

(extend-protocol cic/Classification
  clojure.lang.PersistentArrayMap
  (cic/priority [this] (:priority this))
  (cic/next-interval-relative-to [this val]
    ((:interval-fn this) val)))

;; [3 8]
(defn p1-next-interval [v]
  (cond
   (includes? [3 8] v) [v 8]
   (< v 3) [3 8]
   :else nil))
(def p1   { :label :p1, :priority 2, :interval-fn p1-next-interval})

;; [5 10]
(defn p2-next-interval [v]
  (cond
   (includes? [5 10] v) [v 10]
   (< v 5) [5 10]
   :else nil))
(def p2   { :label :p2, :priority 5, :interval-fn p2-next-interval})

(def pdef { :label :default, :priority 0, :interval-fn (fn [val] [val (+ 1 val)]) })


(test/deftest lower-bound-relative-to
  (test/is (= 4  (cic/lower-bound-relative-to p1 4))   "returns value if value is in interval")
  (test/is (= 3  (cic/lower-bound-relative-to p1 2))   "returns start of next interval if value is smaller")
  (test/is (= 3  (cic/lower-bound-relative-to p1 3))   "is inclusive")
  (test/is (nil? (cic/lower-bound-relative-to p1 9))   "returns nil if it doesn't apply"))

(test/deftest upper-bound-relative-to
  (test/is (= 8  (cic/upper-bound-relative-to p1 4))   "returns value if value is in interval")
  (test/is (= 8  (cic/upper-bound-relative-to p1 2))   "returns start of next interval if value is smaller")
  (test/is (= 8  (cic/upper-bound-relative-to p1 8))   "is inclusive")
  (test/is (nil? (cic/upper-bound-relative-to p1 9))   "returns nil if it doesn't apply"))

(test/deftest bounds
  lower-bound-relative-to
  upper-bound-relative-to)

(test/deftest classifies?
  (test/is (cic/classifies? p1 4) "Detects value in bound")
  (test/is (not (cic/classifies? p1 2)) "value < lower bound")
  (test/is (not (cic/classifies? p1 9)) "value > upper bound"))

(test/deftest classify
  (test/is (= p1 (cic/classify 4 [p1 p2]))   "finds correct class")
  (test/is (nil? (cic/classify 100 [p1 p2])) "returns nil if there is no matching class")
  (test/is (nil? (cic/classify 4 []))        "returns nil if classes are empty")
  (test/is (= p2 (cic/classify 6 [p1 p2]))   "returns the class with higher prio if multiple classes match"))

(test/deftest classifies-until
  (test/is (= 5 (cic/classifies-until p1 3 10 [p1 p2 pdef]))))

;; classifiers
(defn collect-result [res]
  (map (fn [elt]
         [(first elt) (second elt) (:label (last elt))])
       res))

(test/deftest classify-interval
  (test/is (= [[1 2 :default] [3 5 :p1] [6 10 :p2]] (collect-result (cic/classify-interval [1 10] [p1 p2 pdef])))))
