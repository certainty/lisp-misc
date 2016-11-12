(ns jizmo.test.api.v1.vhost_config_file
  (:use clojure.test
        jizmo.api.v1.apache.vhost_config_file))


(deftest Numeric-Prefix-filenames
  (testing "numeric-prefix->number"
    (is (= nil (numeric-prefix->number "foo")) "handles non-numeric input gracefully")
    (is (= 10  (numeric-prefix->number "10")))
    (is (= 5 (numeric-prefix->number "05"))))

  (testing "numeric-prefix?"
    (is (= true  (numeric-prefix? "10_example.com")))
    (is (= true  (numeric-prefix? "/tmp/14_example.com")))
    (is (= false (numeric-prefix? "/tmp/example.com")))
    (is (= false (numeric-prefix? "example.com")))
    (is (= true  (numeric-prefix? "01_example.com")) "handles 01-09"))
  (testing "extract-prefix"
    (is (= nil (extract-prefix "example.com")))
    (is (= 10  (extract-prefix "10_example.com")))
    (is (= 05  (extract-prefix "05_example.com")) "handles 01-09 correctly"))

  (testing "next-avialable-prefix"
    (is (= 9  (next-available-prefix ["01_test.conf" "09_test.conf" "06_test.conf"] 10)))
    (is (= 2  (next-available-prefix ["test.conf" "02_test.conf"] 10)) "handles files with non-numeric prefix gracefully")
    (is (= 10 (next-available-prefix ["test.conf"] 10))))

  (testing "recognizing configfile"
    (let [testfile "/tmp/test.conf"
          _ (spit testfile "test")]
      (is (vhost-config-file? testfile))))

  ;; (testing "generate-file-path"
  ;;   (let [paths ["/tmp/07_example.com.conf","/tmp/08_example.com.conf"]]
  ;;     (is (= "/tmp/09_test.example.com" (generate-file-path "test.example.com" paths)))
  ;;     (is (= "/tmp/02_test.example.com" (generate-file-path "test.example.com" [] :start_at 2)))))

)
