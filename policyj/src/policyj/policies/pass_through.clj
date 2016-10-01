(ns policyj.policies.pass-through
  (:require  [policyj.postfix.response :as resp]))

(declare policy)

(defn- handler [request config]
  (resp/respond-with :dunno :extra { :policy-id (:id policy) }))

(def policy { :id :pass-through :name "Pass-Through" :handler handler })
