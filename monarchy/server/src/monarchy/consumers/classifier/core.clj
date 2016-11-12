(ns monarchy.consumer.classifier.core
  (:require [monarchy.common.messaging.pubsub :as messaging]
            [monarchy.common.config :as config]
            [clojure.tools.logging :as log]))


(defn classify-sample [hostname uid name facts]
  {
   :uuid uid
   :hostname hostname
   :classified-as :ok
   :classified-by :default-classifier
   :classification-certainty 1.0
   :sensor name
   :facts facts
   })

(defn classify-samples [message]
  (let [hostname (message :hostname)
        uuid     (message :uuid)]
    (map
     (fn [sample]
       (classify-sample hostname uuid (sample :name) (sample :facts)))
     (message :samples))))

(def ^:dynamic subscribe (messaging/make-subscriber (config/config-get [:messaging])))

(defn handle-message [message]
  (log/info "received message " message)
  (doseq [classified-message (classify-samples message)]
    (let [class (classified-message :class)]
      (messaging/publish "samples.classified" [:message :classified class] classified-message))))

(defn run [cfg]
  (log/info "Starting up classifier")
  (messaging/subscribe "samples.incoming" handle-message))
