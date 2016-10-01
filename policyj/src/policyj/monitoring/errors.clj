(ns policyj.monitoring.errors)

(def the-errors (atom {}))

(defn add-error
  "Add an error under the given key. If an error with that key already exists, it will be replaced"
  [key properties]
  (swap! the-errors assoc key properties))

(defn clear-errors! []
  (reset! the-errors {}))

(defn errors []
  @the-errors)

(defn errors? []
  (not (empty? (keys @the-errors))))
