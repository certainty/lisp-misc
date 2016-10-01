(ns policyj.statistics.collector-test
  (:require
   [clojure.test :refer :all]
   [policyj.statistics.collector :as stats]))

(deftest stats-for-ok-messages
  (stats/reset-statistics!)
    (await (stats/update {:action
                          :dunno
                          :message "DUNNO"
                          :extra {:id :dns-blacklist :service "bl.example.com"}} 1))
    (is (= (@stats/statistics :total-connections 1)))
    (is (= (@stats/statistics :rejected-connections 0))))

(deftest stats-for-max-threads
  (stats/reset-statistics!)
  (await (stats/update { :action
                         :dunno
                         :message "DUNNO"
                         :extra {}}
                       1))

  (await (stats/update { :action
                         :dunno
                         :message "DUNNO"
                         :extra {}}
                       10))

  (await (stats/update { :action
                         :dunno
                         :message "DUNNO"
                         :extra {}}
                       2))

  (is (= (@stats/statistics :max-threads) 10)))

(deftest stats-for-dns-blacklist-test
  (stats/reset-statistics!)
  (testing "with one response"
    (await (stats/update {:action
                          :reject
                          :message "123.20.210.69 is blacklisted by bl.example.com"
                          :extra {:id :dns-blacklist :service "bl.example.com"}} 1))
    (is (= (@stats/statistics :dns-blacklisted) { "bl.example.com" 1 }))
    (is (= (@stats/statistics :total-connections) 1))
    (is (= (@stats/statistics :rejected-connections) 1)))

  (testing "with more than one response"
    (stats/reset-statistics!)
    (await (stats/update {:action
                          :reject
                          :message "123.20.210.69 is blacklisted by bl.example.com"
                          :extra {:id :dns-blacklist :service "bl.example.com"}} 1))
    (await (stats/update {:action
                          :reject
                          :message "123.20.210.69 is blacklisted by bl.example.com"
                          :extra {:id :dns-blacklist :service "bl.example.com"}} 1))
    (is (= (@stats/statistics :dns-blacklisted) { "bl.example.com" 2 }))
    (is (= (@stats/statistics :total-connections) 2))
    (is (= (@stats/statistics :rejected-connections) 2))))
