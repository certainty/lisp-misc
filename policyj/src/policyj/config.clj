(ns policyj.config
  (:refer-clojure :exclude [get])
  (:use [clojure.java.io :as io])
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

(defn load-configuration-from
  "Loads the configuration from the given path. You have to make sure that
   the file actually exists.

   > (load-configuration-from \"/some/path\")
  "
  [path]
  (binding [*read-eval* false]
    (edn/read-string (slurp path))))

(defn get
  "Retrieve the value under the given path from the configuration.
   The configuration is a (possibly nested) map. A path is simply the
   the list of keys that have to be traversed in order to arrive at a given value

   > (get cfg [:database :username])
  "
  [cfg path & default]
  (or (reduce clojure.core/get cfg  path) (first default)))
