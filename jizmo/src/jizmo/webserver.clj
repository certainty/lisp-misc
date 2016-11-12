(ns jizmo.webserver
  (:use compojure.core ring.adapter.jetty)
  (:use compojure.core)
  (:require
   [jizmo.api.v1.endpoint :as jizmo-endpoint]
   [clojure.tools.logging :as log]))


(defn start-server [address port]
  (log/info (str "Starting up jizmo server at " address ":" port))
  (run-jetty #'jizmo-endpoint/api-v1 {:port port :host address :join? false}))
