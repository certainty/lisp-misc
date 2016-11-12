(ns monarchy-server.test.api.v1.samples
  (:use clojure.test
        ring.mock.request
        monarchy-server.api.v1.endpoint))

(deftest api-v1-apache-test
  (testing "/samples"
    (testing "POST"
      (testing "with valid params"
        (let [valid-params { }]
          (let [response (api-v1 (request :post "/v1/samples/1234-1234-1234-1234" valid-params))]
            (is (= (:status response) 200) "returns 200")))))))
