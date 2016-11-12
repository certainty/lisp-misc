(ns monarchy.api.application
  (:use [clojure.tools.cli :only (cli)])
  (:require  [monarchy.common.config :as config]
             [monarchy.api.webserver :as ws]
             [clojure.tools.logging :as log]
             [monarchy.api.v1.samples.core :as v1:samples])
  (:import (java.io File))
  (:gen-class :main true))

(defn print-usage [banner]
  (println "******************************************")
  (println "MONARCHY - We've got you covered")
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
  (v1:samples/config-check config))

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
      (ws/start-server (config/config-get [:server :address] "0.0.0.0") (config/config-get [:server :port] 8080)))
))
