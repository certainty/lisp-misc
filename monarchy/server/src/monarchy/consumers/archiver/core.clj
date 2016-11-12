(ns monarchy.consumers.archiver.core
  (:refer-clojure :exclude [distinct group-by])
  (:use [sqlingvo.core])
  (:require [monarchy.common.messaging.pubsub :as messaging]
            [monarchy.common.config :as config]
            [clojure.java.jdbc :as j]
            [cheshire.core :as json]
            [clojure.tools.logging :as log]))

(defn database-connection [config]
  (log/info config)
  {
   :subprotocol "postgresql"
   :user (config :username)
   :password (config :password)
   :subname (str "//" (config :host) ":5432/" (config :database))
   })

(defn store-message! [message]
  (log/info message)
  (let [meta-data  (message "meta")
        body       (json/generate-string (message "body"))
        db         (database-connection (config/config-get [:database]))]
    (j/insert! db :samples { :sender_uuid (meta-data "uuid")
                             :sent_at (java.sql.Timestamp. (* (meta-data "time") 1000))
                             :body body})))

(defn handle-incoming [message]
  (log/info "received new message " message)

  (try
    (store-message! message)
    (catch Exception e
      (log/error "Could not save message " e))))

(defn handle-classified [message]
  (log/info "received new message" message))

(def ^:dynamic get-subscriber
  (memoize
   (fn [cfg]
     (messaging/make-subscriber cfg))))

(defn subscribe [key handler]
  ((get-subscriber (config/config-get [:messaging])) key handler))

(defn run []
  (log/info "starting up ...")
  (subscribe "samples.incoming"   handle-incoming)
  (subscribe "samples.classified" handle-classified))
