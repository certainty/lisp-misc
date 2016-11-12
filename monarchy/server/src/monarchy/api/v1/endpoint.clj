(ns monarchy.api.v1.endpoint
  (:use compojure.core)
  (:require [compojure.handler :as handler]
            [compojure.route :as route]
            [ring.middleware.json :as json]
            [monarchy.api.middleware.logging :as logging]
            [monarchy.api.v1.samples.routes :as samples:v1]))


(defroutes api-v1-routes
  (context "/v1" []
     samples:v1/the-routes
    (route/not-found { :status 404 })))

(def api-v1
  (->
   api-v1-routes
   handler/api
   logging/wrap-request-logging
   json/wrap-json-body))
