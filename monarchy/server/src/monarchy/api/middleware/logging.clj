(ns monarchy.api.middleware.logging
  (:require  [clojure.tools.logging :as log]))


(defn wrap-request-logging [handler]
  (fn [{:keys [request-method uri] :as req}]
    (let [resp (handler req)]
      (log/info "Processed request " request-method " " uri)
      resp)))
