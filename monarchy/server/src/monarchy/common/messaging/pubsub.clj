(ns monarchy.common.messaging.pubsub
  (:use
   [monarchy.common.messaging.protocol]
   [monarchy.common.messaging.rabbitmq.pubsub]
   [monarchy.common.messaging.activemq.pubsub]))

(defn make-publisher
  "Creates a publish procedure that is configured using the configuration map
   The following keys must be set:
   :broker-type - the type of the broker :rabbitmq, :activemq
   :broker - the broker uri
   :username - the username to use when connection the the broker
   :password - the password to use when connection the the broker
   :topic  - the topic to publish to

   The procedure returned takes two arguments:
   1) the routing-key - the key to publish to
   2) message         - the message data

   Examples:
   (def ^:dynamic publish (make-publisher config))
   (publish \"something\" { :some-key :value })
  "
  [config]
  (let [broker-type (config :broker-type)]
    (fn [key message]
      (do-publish broker-type config key message))))

(defn make-subscriber
  "Creates a subscriber procedure that is configured using the configuration map
   The following keys must be set:
   :broker-type - the type of the broker :rabbitmq, :activemq
   :broker - the broker uri
   :username - the username to use when connection the the broker
   :password - the password to use when connection the the broker
   :topic  - the topic to publish to

    The procedure returned takes two arguments
    1) routing-key - the key to subscribe to
    2) handler     - a function that is invoked whenever a message arrives

   Examples:
   (def ^:dynamic subscribe (make-subscriber config))
   (subscribe \"something\" (fn [message] (do-something-with message)))
  "
  [config]
  (let [broker-type (config :broker-type)]
    (fn [key handler]
      (do-subscribe broker-type config key handler))))
