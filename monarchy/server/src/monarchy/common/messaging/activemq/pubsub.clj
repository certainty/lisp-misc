(ns monarchy.common.messaging.activemq.pubsub
 (:require
   [clojure.tools.logging :as log]
   [clamq.activemq :as activemq]
   [clamq.protocol.connection :as connection]
   [clamq.protocol.producer :as producer]
   [clamq.protocol.consumer :as consumer])
 (:use  [monarchy.common.messaging.protocol]))

(defn- connect-to-broker [config]
  (let [message-broker (config :broker)
        username       (config :username)
        password       (config :password)]
    (activemq/activemq-connection message-broker :username username :password password)))

(defmethod do-publish :activemq [_ config key message & { :keys [tags] :or { tags [] }}]
  (with-open [connection (connect-to-broker config)]
    (let [producer (connection/producer connection)
          topic    (config :topic)]
      (producer/publish producer (str topic "." key) message))))

(defn- wrap-handler [tags handler]
  (fn [message]
    ;; TODO add filtering per tag
    (handler message)))

(defmethod do-subscribe :activemq [_ config key handler & { :keys [tags] :or { tags [] }}]
  (let [topic (config :topic)]
    (with-open [connection (connect-to-broker config)]
      (let [consumer (connection/consumer connection {:endpoint (str topic "." key) :on-message (wrap-handler tags handler) :transacted true})]
        (consumer/start consumer)))))
