(defproject jizmo "0.0.1"
  :description "RESTful administration"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [compojure "1.1.5"]
                 [ring "1.2.0"]
                 [ring/ring-json "0.2.0"]
                 [clj-time "0.5.1"]
                 [clj-log "0.4.4"]
                 [log4j "1.2.16"]
                 [org.clojure/tools.logging "0.2.6"]
                 [org.clojure/tools.cli "0.2.2"]
                 [org.clojars.hozumi/clj-commons-exec "1.0.6"]
                 [de.ubercode.clostache/clostache "1.3.1"]]
  :plugins [[lein-ring "0.8.5"]]
  :ring {:handler jizmo.api.v1.endpoint/api-v1}
  :main jizmo.application
  :profiles
  {:dev {:dependencies [[ring-mock "0.1.5"]
                        [org.clojure/data.json "0.2.2"]]}}
  )
