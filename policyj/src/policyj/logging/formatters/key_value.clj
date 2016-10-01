(ns policyj.logging.formatters.key_value
  (:require [clojure.tools.logging :as logging]
            [clj-time.core :as t]
            [clj-time.format :as f]))
(declare format-map)

(def ^:dynamic *pair-separator* " ")
(def ^:dynamic *field-separator* "=")
(def ^:dynamic *value-separator* ",")
(def ^:dynamic *nesting-separator* ".")
(def ^:dynamic *time-formatter* (f/formatters :date-time))

(defn glue-key [path key]
  (clojure.string/join *nesting-separator* (remove clojure.string/blank? [path,(name key)])))

(defn glue-pairs [ & pairs ]
  (clojure.string/join *pair-separator* (remove clojure.string/blank? pairs)))

(defn format-seq [seq]
  (let [forced-seq (doall seq)]
    (if (vector? (first forced-seq))
      (format-map (reduce conj forced-seq))
      (clojure.string/join *value-separator* (map str forced-seq)))))

(defn format-date [date]
  (f/unparse *time-formatter* date))

(defn format-value [value]
  (cond
   (symbol? value)  (name value)
   (keyword? value) (name value)
   (seq? value)     (format-seq value)
   (vector? value)  (format-seq (map identity value))
   (list? value)    (format-seq (map identity value))
   (= (class value) org.joda.time.DateTime) (format-date value)
   true             (str value)))

(defn format-pair [key value]
  (clojure.string/join *field-separator* [(format-value key) (format-value value)]))

(defn format-map [map path]
  (letfn [(format-kv [output [key val]]
            (let [new-key (glue-key path key)]
              (glue-pairs output (if (map? val)
                                   (format-map val new-key)
                                   (format-pair new-key val)))))]
    (reduce format-kv "" map)))

(defn format-event [event]
  (try
    (format-map event "")
    (catch Exception e
        (str "formatfailure" *field-separator* "true" *pair-separator* "event" *field-separator* event))))
