(ns monarchy.common.messaging.rabbitmq.admin
 (:use [clamq.rabbitmq])
 (:import
  [org.springframework.amqp.core BindingBuilder Exchange Queue TopicExchange]
  [org.springframework.amqp.rabbit.connection CachingConnectionFactory]
  [org.springframework.amqp.rabbit.core RabbitAdmin]))


(defonce admin (RabbitAdmin. (CachingConnectionFactory. "localhost")))

(defn- declareQueue [name]
  (.declareQueue admin (Queue. name)))

(defn- declareTopicExchange [name]
  (.declareExchange admin (TopicExchange. name)))

(defn- declareBinding [exchange queue]
  (.declareBinding  admin (.. BindingBuilder (bind (Queue. queue)) (to (TopicExchange. exchange)) (with queue))))

(defn setup
  "Setup all queues that are needed"
  []
  (let [topic "monarchy"]
    (declareTopicExchange topic)
    (println "Declared topic: " topic)
    (doseq [queue (map #(str topic "." %) ["samples.incoming" "samples.classified"])]
      (declareQueue queue)
      (println "Added queue: " queue)
      (declareBinding topic queue)
      (println "Bound queue to topic"))
    (println "Done ...")
    (System/exit 0)))
