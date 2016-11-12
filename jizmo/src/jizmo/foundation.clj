(ns jizmo.foundation
  (:require  [clostache.parser :as clostache]
             [jizmo.config :as config]
             [clj-commons-exec :as exec])
  (:import (java.io File RandomAccessFile)))

(def +version+ "0.0.1")


(defn file-basename [path]
  (.getName (clojure.java.io/file path)))

(defn render-template
  "Renders the template specified by tpl and returns the result.
   It uses the clostache templating engine.
  "
  [tpl settings]
  (clostache/render tpl settings))

(defn render-template-file
  "Renders the template specified by path and returns the result"
  [path settings]
  (render-template (slurp path) settings))


(defn aquire-lock [label]
  (let [locking-dir (config/config-get [:server :lockdirectory] "/tmp")
        lockfile (str locking-dir "/" label ".lock")
        f (File. lockfile)
        channel (.getChannel (RandomAccessFile. f "rw"))]
    [(.lock channel) channel]))

(defn release-lock [lock channel]
  (.release lock)
  (.close channel))

;; TODO
;; We don't actually have multiple processes so this locking
;; doesn't make sense. Can we wrap this into a transaction instead?
(defn with-lock [label function]
  (let [[lock channel] (aquire-lock label)]
    (try
      (function)
      (finally (release-lock lock channel)))))

(defn execute-command [command]
  @(exec/sh (clojure.string/split command #"\s+")))
