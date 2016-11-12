(ns jizmo.config
  (:use [clojure.java.io :as io])
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(def configuration (atom {}))

(defn load-config [path]
  (binding [*read-eval* false]
    (edn/read-string (slurp path))))

(defn config-get [ path & default]
  (or (reduce get @configuration path)  (first default)))
