(ns policyj.postfix.response-test
  (:require [clojure.test :refer :all])
  (:use     [policyj.postfix.response :as resp]))

(deftest generate-response-test
  (is (= (resp/respond-with :defer_if_permit) { :action :defer_if_permit :message nil :extra {} }))
  (is (= (resp/respond-with :dunno :msg "testmessage") { :action :dunno :message "testmessage" :extra {}}))
  (is (= (resp/respond-with :dunno :msg "testmessage" :extra { :key :value}) { :action :dunno :message "testmessage" :extra { :key :value}})))

(deftest postfix-response-test
  (is (= "action=DUNNO\n\n"      (resp/postfix-response { :action :dunno })))
  (is (= "action=DUNNO test\n\n" (resp/postfix-response { :action :dunno :message "test" })))
  (is (= "action=DEFER test\n\n" (resp/postfix-response { :action :defer :message "test" :extra { :key :value }}))))
