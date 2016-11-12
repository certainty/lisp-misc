(defproject net-protocols "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/tools.namespace "0.2.4"]
                 [clj-tcp "0.2.17-SNAPSHOT"]]
  :main ^:skip-aot net-protocols.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}}
  :aliases { "example-server" ["run" "-m" "net-protocols.examples.console-server.server/main"]
             "example-client" ["run" "-m" "net-protocols.examples.console-server.client/main"]
             }
  )
