(ns policyj.postfix.request-test
  (:require [clojure.test :refer :all])
  (:use [policyj.postfix.request :as req]))

(def example-request "client=127.0.1\nprotocol= smtp\nstress=   \nsomekey=somevalue\n\n")

(deftest test-request-map
  (let [request (req/request-map (java.io.BufferedReader. (java.io.StringReader. example-request)))]
    (is (nil? (:stress request)) "It supports null values")
    (is (= { :client "127.0.0.1", :protocol "smtp", :stress nil, :somekey "somevalue"}) request) "The request is parsed correctly"))
