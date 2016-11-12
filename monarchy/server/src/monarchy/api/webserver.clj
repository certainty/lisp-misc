(ns monarchy.api.webserver
  (:use compojure.core ring.adapter.jetty)
  (:use compojure.core)
  (:require
   [monarchy.api.v1.endpoint :as monarchy-endpoint]
   [clojure.tools.logging :as log]))


(defn start-server [address port]
  (log/info (str "Starting up monarchy server at " address ":" port))
  (run-jetty #'monarchy-endpoint/api-v1 {:port port :host address :join? false}))
