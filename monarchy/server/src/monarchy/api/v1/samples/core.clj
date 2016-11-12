(ns monarchy.api.v1.samples.core
  (:require
   [monarchy.common.messaging.pubsub :as messaging]
   [monarchy.common.config :as config]
   [ring.util.response :as resp]
   [clojure.tools.logging :as log]))

(defn config-check [cfg]
  (when-not (cfg :messaging)
    (throw (Exception. "You need to configure the messaging"))))

(defn normalize-keys
  "Convert string keys into keywords"
  [data]
  data)

(def ^:dynamic get-publisher
  (memoize
   (fn [cfg]
     (messaging/make-publisher cfg))))

(defn publish [key message]
  ((get-publisher (config/config-get [:messaging])) key message))

(defn process-sample [request]
  (let [data (request :body)]
    (if data
      (try
        (publish "samples.incoming" data)
        (log/info "received data from client: " data)
        { :status 204 }
        (catch Exception ex
          (log/error ex)
          (resp/response { :status 500 :body { :error "Unknown"} })))
      (resp/response { :status 406 :body { :error  "Expected json body" }}))))
