(ns policyj.policies.db.whitelist
  (:require [policyj.config                :as cfg]
            [policyj.logging.logger         :as log]
            [policyj.policies.db.cache     :as cache]
            [policyj.policies.db.lookup    :as list]
            [policyj.postfix.response      :as resp]))

(declare policy)

(defn- setup [config]
  ;; default refresh interval to 10 minutes
  (let [refresh-interval (* 60 (cfg/get config [:cache-lifetime] 10))
        db-config (cfg/get config [:authorative-source] {})]
    ;; refresh cache now
    (cache/refresh-cache-for! :whitelist db-config)
    ;; refresh periodically
    (cache/refresh-cache-periodically! :whitelist refresh-interval db-config)))

(defn- handler [request config]
  (let [client (:client_address request)]
    (when client
      (let [match (list/lookup :whitelist client)]
        (when match
          (resp/respond-with :dunno
                             :extra { :policy-id (:id policy)  :recipient (:recipient request) :client_address client :match match } ))))))

(def policy { :id :db-whitelist :name "DB-Whitelist" :handler handler :setup setup })
