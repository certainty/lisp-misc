(ns policyj.policies.dns.blacklist
  (:require [policyj.config             :as cfg]
            [policyj.policies.dns.rbl   :as rbl]
            [policyj.postfix.response   :as resp]))

(declare policy)

;; validate config
(defn- setup [config] true)


(defn- handler
  "This policy will check if the client is listed by one of the configured blacklist services.
   If the request doesn't contain a client_address, the policy will default to return no answer.
  "
  [request config]
  (let [blacklists (cfg/get config [:services])
        client (:client_address request)]
    (when client
      (let [blocked-by (rbl/rbl-check client blacklists)]
        (when blocked-by
          (resp/respond-with :reject
                             :msg (str client " is blacklisted by " blocked-by)
                             :extra { :policy-id (:id policy) :service blocked-by :recipient (:recipient request) :client_address (:client_address request) }))))))

(def policy { :id :dns-blacklist :name "DNS-Blacklist" :handler handler })
