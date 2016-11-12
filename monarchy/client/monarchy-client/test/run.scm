(load "../monarchy-client.scm")

(use veritas veritas-verifiers)

(let ((f (fact 'load 'equals 10)))
  (verify (fact/subject f) (is 'load))
  (verify (fact/predicate f) (is 'equals))
  (verify (fact/object f) (is 10))
  (verify (serialize-fact/edn f) (is '(vector load equals 10)))

  (let ((s (sample "sensor1" f)))
    (verify (serialize-sample/edn s) (is '(tag (monarchy . sample)
                                               (map (name: . "sensor1")
                                                    (facts: list (vector load equals 10))))))))
