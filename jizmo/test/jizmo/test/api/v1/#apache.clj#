(ns jizmo.test.api.v1.apache
  (:use clojure.test
        ring.mock.request
        jizmo.api.v1.endpoint))

(deftest api-v1-apache-test
  (testing "/apache/vhosts"
    (testing "POST"
      (testing "with valid params"
        (let [valid-params { :servername "example.com", :username "bob"}]
          (let [response (api-v1 (request :post "/v1/apache/vhosts/example.com" valid-params))]
            (is (= (:status response) 201) "returns 201")))))))
