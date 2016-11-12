(use zmq zlib json netstring system-peep)


;; A Sample looks like this:
;; uuid: the id of the host (string)
;; hostname: the hostname to reach the origin (string)
;; service: the name o (string)
;; value: the value of that sample (any)
;; tags: (list)
;; timestamp: when was the sample created (flonum)

;; Events can be created as a reaction to samples that have been received
;; The categorizer may decide to fire a critical event in case the samples
;; received so far support this
;;
;; Event:
;; severity: (severty of the event (info warning alert)
;; type: string
;; samples: (list of samples that support this event)


(define (make-sample #!key uuid hostname service value tags timestamp)
  (vector
   `(message-type . sample)
   `(uud . ,uuid)
   `(hostname . ,hostname)
   `(service . ,service)
   `(value . ,value)
   `(tags . ,tags)
   `(timestamp . ,(current-seconds))))

(define (make-event #!key severity type samples)
  (vector
   `(message-type . event)
   `(severity . ,severity)
   `(type . ,type)
   `(samples . ,samples)))

(define (->json-string obj)
  (with-output-to-string (cut json-write obj)))

(define (json->object json)
  (with-input-from-string json json-read))

(define (deflate string)
  (call-with-output-string
   (lambda (out)
     (let ((p (open-zlib-compressed-output-port out)))
       (write string p)
       (close-output-port p)))))

(define (inflate string)
  (call-with-input-string string
                          (lambda (in)
                            (let ((p (open-zlib-compressed-input-port in)))
                              (read p)))))

(define (encode-message object #!key (deflate deflate))
  (string->netstring (deflate (->json-string object))))

(define (decode-message message #!key (inflate inflate))
  (json->object (inflate (netstring->string message))))

(define (push-sample socket sample)
  (send-message socket (encode-message sample))
  (receive-message* socket))

(define (go-on-air uri #!key (idle-seconds 2))
  (let ((socket (make-socket 'req)))
    (connect-socket socket uri)
    (let loop ((facts (facts)))
      (push-sample socket (take-sample))
      (sleep idle-seconds)
      (loop (facts)))))


(go-on-air "tcp://127.0.0.1:5555")

