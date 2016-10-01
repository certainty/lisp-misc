(ns policyj.policies.unknown
  (:require  [policyj.postfix.response :as resp]))

(declare policy)

(defn- handler [request config]
  (when (= "unknown" (:client_address request))
    (resp/respond-with :dunno :extra { :policy-id (:id policy), :purpose "unknown_client"})))

(def policy { :id :unknown-handler :name "Unknown-Handler" :handler handler })
