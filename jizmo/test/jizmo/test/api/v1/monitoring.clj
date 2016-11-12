(ns jizmo.test.api.v1.monitoring
  (:use clojure.test
        ring.mock.request
        jizmo.api.v1.endpoint)
  (:require [clojure.data.json :as json]))

(deftest api-v1-monitoring-test
  (testing "/v1/monitoring"
    (testing "GET"
      (let [response (api-v1 (request :get "/v1/monitoring"))]
        (is (= (:status response) 200) "returns 200")
        (is (= (get (json/read-str (:body response)) "version") "0.0.1" ))
        (is (number? (get (json/read-str (:body response)) "uptime")))))))
