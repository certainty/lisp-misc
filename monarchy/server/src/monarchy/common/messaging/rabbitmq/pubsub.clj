(ns monarchy.common.messaging.rabbitmq.pubsub
  (:require
   [clojure.tools.logging :as log]
   [clamq.rabbitmq :as rabbitmq]
   [clamq.protocol.connection :as connection]
   [clamq.protocol.producer :as producer]
   [clamq.protocol.consumer :as consumer]
   [monarchy.common.config :as config])
  (:use [monarchy.common.messaging.protocol]))

(defn- connect-to-broker [config]
  (let [message-broker (config :broker)
        username       (config :username)
        password       (config :password)]
    (rabbitmq/rabbitmq-connection message-broker :username username :password password)))

(defmethod do-publish :rabbitmq [_ config key message & { :keys [tags] :or { tags [] }}]
  (with-open [connection (connect-to-broker config)]
    (let [producer (connection/producer connection)
          topic    (config :topic)]
      (producer/publish producer { :exchange topic :routing-key (str topic "." key)} message))))

(defn- wrap-handler [tags handler]
  (fn [message]
    ;; TODO add filtering per tag
    (handler message)))

(defmethod do-subscribe :rabbitmq [_ config key handler & { :keys [tags] :or { tags [] }}]
  (let [topic (config :topic)]
    (with-open [connection (connect-to-broker config)]
      (let [consumer (connection/consumer connection {:endpoint (str topic "." key) :on-message (wrap-handler tags handler) :transacted true})]
        (consumer/start consumer)))))
