(ns policyj.policies-test
  (:require
   [clojure.test :refer :all]
   [test.helpers.db     :as db]
   [test.helpers.config :as test-config]
   [policyj.config :as cfg]
   [policyj.policies.db.cache :as c]
   [policyj.policies.dns.blacklist :as p-dns-blacklist]
   [policyj.policies.db.blacklist  :as p-db-blacklist]
   [policyj.policies.db.whitelist  :as p-db-whitelist]
   [policyj.policies :as p]
   [policyj.postfix.response :as resp]))



;; this is sort of a system test
(defn cleanup-tables! []
  (db/truncate-tables! (db/emailfilter-db) "whitelist" "blacklist"))

(defn load-list-fixtures! []
  (db/load-fixtures! (db/emailfilter-db)
                     "whitelist"
                     { :addr "192.168.0.0" :bits 24 :reason "test" :expires_at nil })
  (db/load-fixtures! (db/emailfilter-db)
                     "blacklist"
                     { :addr "212.77.255.3" :bits 32 :reason "test" :expires_at nil }
                     { :addr "212.77.255.4" :bits 32 :reason "test" :expires_at nil }))

(defn setup-environment [f]
  (cleanup-tables!)
  (load-list-fixtures!)
  (c/refresh-cache-for! :whitelist (cfg/get test-config/test-config [:policies :db-whitelist :authorative-source]))
  (c/refresh-cache-for! :blacklist (cfg/get test-config/test-config [:policies :db-blacklist :authorative-source]))
  (f))

(use-fixtures :each setup-environment)

(deftest test-pipline
  (let [config   (cfg/get test-config/test-config [:policies])
        pipeline (comp resp/postfix-response
                       (p/create-pipeline [p-db-whitelist/policy p-db-blacklist/policy p-dns-blacklist/policy] config))]
    (is (= "action=DUNNO\n\n"  (pipeline { :client_address "192.168.0.2"  })) "DB-Whitelist")
    (is (= "action=REJECT 212.77.255.3 is blacklisted\n\n" (pipeline { :client_address "212.77.255.3" })) "DB-Blacklist")
    (is (= "action=REJECT 127.0.0.2 is blacklisted by ix.dnsbl.manitu.net\n\n" (pipeline { :client_address "127.0.0.2"})) "DNS-Blacklist")
    (is (= "action=DUNNO\n\n"  (pipeline { :client_address "127.0.0.1"})) "No policy matches")))
