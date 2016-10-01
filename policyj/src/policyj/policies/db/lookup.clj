(ns policyj.policies.db.lookup
  (:require
            [policyj.policies.db.cache :as cache]
            [clj-time.core   :as t]
            [policyj.policies.db.net-utils :as net]))

(defn- find-first [pred coll]
  (first (filter pred coll)))

(defn- make-matcher [host]
  (fn [record]
    (net/member-of-subnet? host (record :network))))

(defn- expired? [record]
  (let [now (t/now)
        expires-at (:expires_at record)]
    (and expires-at (t/after? now expires-at))))

(defn- relevant-records [type]
  (let [all-records ((cache/value) type)]
    (filter (complement expired?) all-records)))

(defn lookup
  "Checks if the given host which must be dotted quad ip address is in the cache.
   The type must be either :whitelist or :blacklist.
  "
  [type host]
  (when-not (cache/empty? type)
    (find-first (make-matcher host) (relevant-records type))))
