(ns jizmo.api.v1.endpoint
  (:use compojure.core)
  (:require [compojure.handler :as handler]
            [compojure.route :as route]
            [ring.middleware.json :as middleware]
            [jizmo.api.middleware.logging :as logging]
            [jizmo.api.v1.apache.routes :as apache:v1]
            [jizmo.api.v1.monitoring.routes :as monitoring:v1]))


(defroutes api-v1-routes
  (context "/v1" []
     apache:v1/the-routes
     monitoring:v1/the-routes
    (route/not-found { :status 404 })))

(def api-v1
  (->
   api-v1-routes
   handler/api
   logging/wrap-request-logging
   middleware/wrap-json-body
   middleware/wrap-json-response))
