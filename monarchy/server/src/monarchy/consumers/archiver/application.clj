(ns monarchy.consumers.archiver.application
  (:use [clojure.tools.cli :only (cli)])
  (:require  [monarchy.common.config :as config]
             [monarchy.common.messaging.pubsub :as messaging]
             [monarchy.consumers.archiver.core :as core]
             [clojure.tools.logging :as log])
  (:import (java.io File))
  (:gen-class :main true))

(defn print-usage [banner]
  (println "******************************************")
  (println "MONARCHY ARCHIVER")
  (println "******************************************")
  (println banner))

(defn process-command-line [args]
  (let [[opts args banner]
        (cli args
             ["-h" "--help" "Show help" :flag true :default false]
             )]
    (when (:help opts)
      (print-usage banner)
      (System/exit 0))
    [banner opts args]))

(defn expand-path [path]
  (.getCanonicalPath (File. path)))

(defn file-exists? [path]
  (.exists (File. path)))

(defn config-check
  "Give each module a chance to verfy its config"
  [config]
  true)

(defn -main [ & args ]
  (let [[banner opts rest-args] (process-command-line args)
        configfile (first rest-args)]
    (when-not configfile
      (print-usage banner)
      (System/exit 1))
    (let [configfile (expand-path configfile)]
      (when-not (file-exists? configfile)
        (println "The configuration-file doesn't seem to exist")
        (System/exit 2))
      (config/load-configuration configfile)
      (log/info "Config: " (config/current-configuration))

      (config-check (config/current-configuration))
      (core/run))))
