(defproject monarchy "0.1.0"
  :description "The server-side of the monarchy monitoring system"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 ;; general
                 [clj-log "0.4.4"]
                 [org.slf4j/slf4j-log4j12 "1.7.2"]
                 [log4j/log4j "1.2.17"]
                 [org.clojure/tools.logging "0.2.6"]
                 [org.clojure/tools.cli "0.2.2"]

                 ;; api
                 [compojure "1.1.5"]
                 [ring "1.2.0"]
                 [ring/ring-json "0.3.0"]
                 [clj-time "0.5.1"]

                 ;; common
                 [clamq/clamq-activemq "0.4"]
                 [clamq/clamq-rabbitmq "0.4"]

                 ;; archiver
                 [lobos "1.0.0-beta1"]
                 [sqlingvo "0.5.16"]
                 [org.clojure/java.jdbc "0.3.3"]
                 ;[postgresql/postgresql "8.4-702.jdbc4"]

                 [postgresql "9.1-901-1.jdbc4"]
                 ]

  :plugins [[lein-ring "0.8.5"]
            [lein-daemon "0.5.4"]]
  :ring {:handler monarchy.api.v1.endpoint/api-v1}
  :profiles {:dev           { :dependencies [[ring-mock "0.1.5"]]}
             :main-api      { :main monarchy.api.application }
             :main-archiver { :main monarchy.consumers.archiver.application }}
  :aliases  { "setup-rabbitmq" ["run" "-m" "monarchy.common.messaging.rabbitmq.admin/setup"]
              "run-api"        ["with-profile" "main-api" "run"]
              "run-archiver"   ["with-profile" "main-archiver" "run"] }
  :daemon { :api      { :ns monarchy.api.application :pidfile "pids/monarchy.api.pid"}
            :archiver { :ns monarchy.consumers.archiver.application :pidfile "pids/monarchy.archiver.pid"}} )
