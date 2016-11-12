(ns jizmo.application
  (:use [clojure.tools.cli :only (cli)])
  (:require  [jizmo.config :as config]
             [jizmo.webserver :as ws]
             [clojure.tools.logging :as log]
             [jizmo.api.v1.apache.core :as v1:apache])
  (:import (java.io File))
  (:gen-class :main true))

(defn print-usage [banner]
  (println "******************************************")
  (println "JIZMO is RESTful hosting")
  (println "java -jar jizmo.jar [options] configfile")
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
  (v1:apache/config-check config))

(defn -main [ & args ]
  (let [[banner opts rest-args] (process-command-line args)
        configfile (expand-path (first rest-args))]
    (when-not configfile
      (print-usage banner)
      (System/exit 1))
    (when-not (file-exists? configfile)
      (println "The configuration-file doesn't seem to exist")
      (System/exit 2))
    (swap! config/configuration (fn [&_] (config/load-config configfile)))
    (log/debug "Config: " @config/configuration)
    (config-check @config/configuration)
    (ws/start-server (config/config-get [:server :address] "0.0.0.0") (config/config-get [:server :port] 8080))
))
