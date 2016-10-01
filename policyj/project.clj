(defproject policyj "1.1.8"
  :description "Policy daemon for the postfix MTA"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 ;; logging
                 [clj-log "0.4.4"]
                 [org.slf4j/slf4j-log4j12 "1.7.2"]
                 [log4j/log4j "1.2.17"]
                 [org.clojure/tools.logging "0.2.6"]
                 [org.clojure/tools.cli "0.3.1"]

                 ;; database connection
                 [org.clojure/java.jdbc "0.3.3"]
                 [mysql/mysql-connector-java "5.1.6"]

                 ;; connection pooling
                 ;;[com.jolbox/bonecp "0.7.1.RELEASE"]

                 ;; well the server, may be replaced with a netty based
                 ;; approach
                 [tcp-server "0.1.0"]
                 [clj-time "0.7.0"]]
  :main  policyj.application
  :plugins [[lein-set-version "0.4.1"]]
  :aliases {
            "run-server" ["run" "-m" "policyj.application"]
            })
