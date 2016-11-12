(ns monarchy.common.config
  (:use [clojure.java.io :as io])
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(def configuration (atom {}))

(defn load-configuration [path]
  (binding [*read-eval* false]
    (swap! configuration (fn [&_] (edn/read-string (slurp path))))))

(defn current-configuration [] @configuration)

; (memoize current-configuration)

(defn config-get [ path & default]
  (or (reduce get (current-configuration) path)  (first default)))
