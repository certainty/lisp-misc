(ns jizmo.api.v1.monitoring.routes
  (:use compojure.core clj-time.core)
  (:require [compojure.handler :as handler]
            [clj-time.core     :as time :exclude [extend]]
            [compojure.route   :as route]
            [jizmo.foundation  :as foundation]))

(def uptime
  (let [started-at (now)]
    (fn [] (time/in-minutes (time/interval started-at (time/now))))))

(defroutes the-routes
  (GET "/monitoring" [] { :status 200, :body { :version foundation/+version+, :uptime (uptime) } }))
