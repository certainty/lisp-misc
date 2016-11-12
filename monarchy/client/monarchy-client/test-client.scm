(use monarchy-client log5scm)

;; start a sender for log5scm
;; this feeds all logmessages to stdout
(start-sender app-sender (port-sender (current-output-port)) (category *))

;; this is the implementation of our testsenser
;; it returns a sample of two facts
(define (test-sensor-impl config)
  (sample
   'test
   (fact time: =: (current-milliseconds))
   (fact message: =: "hello monarchy")))


;; make a sensor with the implemenation above
(define (test-sensor)
  (make-sensor 'test test-sensor-impl))


;; finally push the samples that come from this sensor up to the server
;; using the client api
(push-samples-from (test-sensor))
