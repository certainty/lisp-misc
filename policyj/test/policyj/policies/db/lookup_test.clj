(ns policyj.policies.db.lookup-test
  (:require
   [clojure.test :refer :all]
   [clj-time.core   :as t]
   [clj-time.coerce :as tc]
   [policyj.policies.db.cache :as c]
   [policyj.policies.db.lookup :as l]))

(defn setup-environment [f]
  (let [now (t/now)
        one_hour_ago (t/minus now (t/hours 1))
        one_hour_from_now (t/plus now (t/hours 1))
        data [{ :network "192.168.0.0/24"  :reason "test"  :expires_at nil }
              { :network "212.77.255.3/32" :reason "test2" :expires_at nil}
              { :network "212.77.255.4/32" :reason "test3" :expires_at one_hour_from_now}
              { :network "212.77.255.5/32" :reason "test4" :expires_at one_hour_ago}]
        ]
    (c/cache-set! :whitelist data)
    (c/cache-set! :blacklist data))
  (f))

(use-fixtures :each setup-environment)

(defn lookup-test [table]
  (testing "general functionality"
    (is (nil?      (l/lookup table "212.77.255.1")))
    (is (not (nil? (l/lookup table "212.77.255.3")))))
  (testing "expiry"
    (is (not (nil?      (l/lookup table "212.77.255.3"))) "expired_at is nil")
    (is (not (nil? (l/lookup table "212.77.255.4")))      "record is not expired")
    (is (nil?      (l/lookup table "212.77.255.5"))       "record is expired")))


(deftest lookup-whitelist-test
  (lookup-test :whitelist))

(deftest lookup-blacklist-test
  (lookup-test :blacklist))
