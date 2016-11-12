(ns jizmo.api.v1.apache.routes
  (:use compojure.core)
  (:require [compojure.handler :as handler]
            [compojure.route :as route]
            [jizmo.api.v1.apache.core :as apache]
            [jizmo.config :as config]))


(let [cfg (config/config-get [:apache])]
  (defroutes the-routes
    (POST "/apache/vhosts/:domain" req (apache/create-vhost req cfg) )))
