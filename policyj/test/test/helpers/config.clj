(ns test.helpers.config
  (:require
   [test.helpers.db :as db]))

(def test-config
  {
   :policies {
              :db-whitelist {
                              :authorative-source  (conj db/credentials [:database "blocklist_test"])
               }
              :db-blacklist {
                          :authorative-source (conj db/credentials [:database "blocklist_test"])
              }

              :dns-blacklist {
                  :services ["ix.dnsbl.manitu.net"]
              }
     }
 })
