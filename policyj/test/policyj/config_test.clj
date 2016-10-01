(ns policyj.config-test
  (:refer-clojure :exclude [get])
  (:require [clojure.test :refer :all])
  (:use [policyj.config :as cfg]))

(deftest test-get
  (testing "empty config"
    (let [config {}]
      (is (nil? (cfg/get config [:some-key])) "Accessing non existing key returns nil")
      (is (= :default (cfg/get config [:some-key] :default))) "Default is returned if key doesn't exist"))

  (testing "fixed config"
    (let [config { :max 1 :levels { :high 1 :low 2 } :root { :child { :child2 "child2"} } }]
      (is (= 1 (cfg/get config [:max])) "Finds value for given path")
      (is (= 2 (cfg/get config [:levels :low] "Finds value in one level of nesting")))
      (is (= "child2" (cfg/get config [:root :child :child2])) "Finds value in deeper level of nesting")
      (is (nil? (cfg/get config [:levels :non-existent])) "Returns nil if the path doesn't lead to value"))))
