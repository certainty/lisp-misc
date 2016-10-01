(ns policyj.monitoring.legacy_handler-test
  (:require
   [clojure.test :refer :all]
   [policyj.statistics.collector      :as stats]
   [policyj.monitoring.legacy_handler :as handler]
   [policyj.monitoring.errors :as errors]))

(defn includes? [haystack needle]
  (not (neg? (.indexOf haystack needle))))

(deftest monitoring-handler
  (testing "invalid command"
    (is (= (handler/handler "invalid") "Error: Command not understood\n\n")))

  (testing "errors"
    (errors/clear-errors!)
    (testing "when there are no errors"
      (let [result (handler/handler "errors")]
        (is (= "no\n\n" result))))

    (testing "when there are errors"
      (errors/add-error :test2 { :severity :low :message "error"})
      (errors/add-error :test { :severity :high :message "error"})
      (let [result (handler/handler "errors")]
        (is (= "yes\ntest: message: error severity: high\ntest2: message: error severity: low\n\n" result)))))

  (testing "reset"
    (stats/reset-statistics!)
    (await (stats/update {:action
                          :reject
                          :message "123.20.210.69 is blacklisted by ix.dnsbl.manitu.net"
                          :extra {:id :dns-blacklist :service "ix.dnsbl.manitu.net"}} 1))

    (is (= (handler/handler "reset") "ok\n\n"))

    (let [_     (handler/handler "reset")
          stats (handler/handler "stats")]
      (is (includes? stats "uptime="))
      (is (includes? stats "max_threads=0\n"))
      (is (includes? stats "total_connects=0\n"))
      (is (includes? stats "total_rejects=0\n"))
      (is (includes? stats "total_deferrs=0\n"))
      (is (includes? stats "total_deferred_returners=0\n"))
      (is (includes? stats "total_dunnos=0\n"))
      (is (includes? stats "dnswl_hits=0\n"))
      (is (includes? stats "b.barracudacentral.org_hits=0\n"))
      (is (includes? stats "ix.dnsbl.manitu.net_hits=0\n"))))

  (testing "stats"
    (testing "with no requests"
      (stats/reset-statistics!)

      (let [stats (handler/handler "stats")]
        (is (includes? stats "uptime="))
        (is (includes? stats "max_threads=0\n"))
        (is (includes? stats "total_connects=0\n"))
        (is (includes? stats "total_rejects=0\n"))
        (is (includes? stats "total_deferrs=0\n"))
        (is (includes? stats "total_deferred_returners=0\n"))
        (is (includes? stats "total_dunnos=0\n"))
        (is (includes? stats "dnswl_hits=0\n"))
        (is (includes? stats "b.barracudacentral.org_hits=0\n"))
        (is (includes? stats "ix.dnsbl.manitu.net_hits=0\n"))))

    (testing "with requests"
      (stats/reset-statistics!)

      (await (stats/update {:action
                            :reject
                            :message "123.20.210.69 is blacklisted by ix.dnsbl.manitu.net"
                            :extra {:id :dns-blacklist :service "ix.dnsbl.manitu.net"}} 1))

      (await (stats/update { :action
                            :dunno
                            :message "DUNNO"
                            :extra {}}
                           2))

      (let [stats (handler/handler "stats")]
        (is (includes? stats "max_threads=2\n"))
        (is (includes? stats "total_connects=2\n"))
        (is (includes? stats "total_rejects=1\n"))
        (is (includes? stats "total_deferrs=0\n"))
        (is (includes? stats "total_deferred_returners=0\n"))
        (is (includes? stats "total_dunnos=1\n"))
        (is (includes? stats "dnswl_hits=0\n"))
        (is (includes? stats "b.barracudacentral.org_hits=0\n"))
        (is (includes? stats "ix.dnsbl.manitu.net_hits=0\n"))))))
