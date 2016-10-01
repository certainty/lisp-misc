(ns policyj.policies.db.cache-test
  (:require
   [clojure.test :refer :all]
   [test.helpers.db     :as db]
   [test.helpers.config :as test-config]
   [policyj.config       :as cfg]
   [clj-time.core     :as t]
   [clj-time.coerce   :as tc]
   [clj-time.format   :as tf]
   [policyj.policies.db.cache :as c]))

(defn cleanup-tables! []
  (db/truncate-tables! (db/emailfilter-db) "whitelist" "blacklist"))

(defn setup-environment [f]
  (cleanup-tables!)
  (f))

(use-fixtures :each setup-environment)

(deftest cache-translate-fields-test
  (let [exp (t/date-time 2154 10 14 5 0 0 0)]
    (db/load-fixtures! (db/emailfilter-db) "whitelist" { :addr "192.168.0.0" :bits 24 :reason "test" :expires_at (tc/to-sql-time exp) })
    (c/refresh-cache-for! :whitelist (cfg/get test-config/test-config [:policies :db-whitelist :authorative-source]))
    (let [record (first (:whitelist (c/value)))]
      (is (= "192.168.0.0/24" (:network record)))
      (is (= "test" (:reason record)))
      (is (= "21541014" (tf/unparse (tf/formatter "yyyyMMdd") (:expires_at record)))))))

(deftest refresh-cache-test
  (let [before (c/value)
        _ (db/load-fixtures! (db/emailfilter-db) "whitelist" { :addr "212.77.255.3" :bits 24 :reason "test" })
        _ (c/refresh-cache-for! :whitelist (cfg/get test-config/test-config [:policies :db-whitelist :authorative-source]))
        after (c/value)]
    (is (not (= before after)))))
