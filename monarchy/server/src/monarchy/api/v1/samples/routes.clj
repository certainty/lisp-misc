(ns monarchy.api.v1.samples.routes
  (:use compojure.core)
  (:require [compojure.handler :as handler]
            [compojure.route :as route]
            [monarchy.api.v1.samples.core :as samples]))

(defroutes the-routes
  (POST "/samples/:uuid" req (samples/process-sample req)))
