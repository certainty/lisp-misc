(ns policyj.logging.formatters.key_value-test
  (:require
   [clojure.test :refer :all]
   [clj-time.core :as t]
   [policyj.logging.logger :as log]
   [policyj.logging.formatters.key_value :as formatter]))


(deftest format-pair-test
  (is (= "foo=bar"   (formatter/format-pair :foo :bar)))
  (is (= "foo=bar"   (formatter/format-pair 'foo 'bar)))
  (is (= "foo=bar"   (formatter/format-pair "foo" "bar")))
  (is (= "foo=1"     (formatter/format-pair "foo" 1)))
  (is (= "foo=1.234" (formatter/format-pair "foo" 1.234)))
  (is (= "foo=1,2,3" (formatter/format-pair "foo" [1,2,3])))
  (is (= "foo=1,2,3" (formatter/format-pair "foo" (map identity [1,2,3]))) "force lazy seqs of vector")
  (is (= "foo=1,2,3" (formatter/format-pair "foo" (list 1 2 3))))
  ;(is (= "2015-02-18T11:31:52.214" (formatter/format-pair "foo" (t/now)))
  )

(deftest format-maps
  (is (= "key=value key2=value2" (formatter/format-event { :key :value :key2 :value2 })))
  (is (= "key=1,2,3 key2=bar"    (formatter/format-event { :key [1,2,3] :key2 :bar })))
  (is (= "key.subkey.subsub=value key2=value" (formatter/format-event { :key { :subkey { :subsub :value } } :key2 :value}))))

(deftest format-exception
  (let [exn (try
              (throw (Exception. "Test"))
              (catch Exception e e))]
    (is (= (.contains (formatter/format-event (log/construct-exception-message exn)) "exception.class=class")))))
