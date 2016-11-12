;; example usage
;;
;; (use monarchy-client (prefix monarchy-sensor-base :ms))
;;
;; (push-samples-from ms:sensor)


 (define (client-error-details exn) (get-condition-property exn 'exn 'message))
(module monarchy-client
 *
 (import chicken scheme)
 (use json http-client intarweb uri-common simple-configuration log5scm extras data-structures ports)

(define (read-configuration)
  (let ((path (or (get-environment-variable "MONARCHY_CONFIG") "/etc/monarchy.conf")))
    (unless (file-exists? path)
      (error "Don't know where my configuration resides. Either set MONARCHY_CONFIG environment variable to its path or put it at /etc/monarchy.conf"))
    (call-with-input-file path config-read)))

(define (configuration)
  (force (delay (read-configuration))))

(define (config-get key)
  (let ((config (read-configuration)))
    (config-ref config key)))

(define (fqdn)
  (config-get '(fqdn)))

(define (uuid)
  (config-get '(uuid)))

(define (samples-uri)
  (config-get '(samples uri)))

;; logging
;; TODO: we should probably get rid of the logging part or at least not start a sender by default
;; instead we should use conditions. A library that logs to stdout is odd somehow
(define-category info)
(define-category error)
(define-category debug)
(define-category warn)
(define-category warn+ (or info debug warn))


;; == sensor-api ==
;; a fact is a piece of knowledge about the system
;; Each fact is a triple so that in theory we have
;; good chance to express a wide variaty of knowledge
;; about the system. We didn't use full RDF here but
;; the main idea is fine.
(define (fact subject predicate object)
  (vector subject predicate object))

(define (fact/subject   fact) (vector-ref fact 0))
(define (fact/predicate fact) (vector-ref fact 1))
(define (fact/object    fact) (vector-ref fact 2))

;; a sample is just an alist
;; a sample has a name so that we know which sensor
;; produced it
(define (sample name . facts)
  `((name . ,name)
    (facts . ,facts)))

;; a sensor is a pair mapping a name to a piece of code
;; each sensor implementation is expected to return a sample. See samples for an explanation
(define (make-sensor name code)
  (unless (procedure? code)
    (error "You must supply a procedure as the second argument"))
  (cons name code))

(define sensor/name car)
(define sensor/code cdr)


(define (serialize-fact/json fact)
  (vector->list fact))

(define (serialize-sample/json sample)
  (let ((sensor (alist-ref 'name sample))
        (facts  (map serialize-fact/json (alist-ref 'facts sample))))
    (vector `(name . ,sensor)
      `(facts . ,facts))))

(define (serialize-samples/json hostname uuid samples)
  (let ((data (vector `(uuid . ,uuid)
		      `(hostname . ,hostname)
		      `(samples . ,(map serialize-sample/json samples)))))
    (with-output-to-string
      (lambda ()
        (json-write data)))))

;; sends the sample to the given endpoint
;; TODO: it might be feasable to send all samples in bulk instead
;; of each sample seperately. This would decrease requests. The server needs to be able to handle
;; this case though.
(define (push-samples endpoint samples)
  (let* ((data (serialize-samples/json (fqdn) (uuid) samples))
         (request (make-request method: 'POST
                                uri: (uri-reference endpoint)
                                headers: (headers '((content-type "application/vnd.org.monarchy.server-v1+json"))))))
    (send-sample-request request data)))


(define (client-error-details exn) (get-condition-property exn 'exn 'message))
(define (server-error-details exn) (get-condition-property exn 'exn 'message))

(define (send-sample-request request data)
  (log-for (debug) "Samples serialized: ~a" data)
 (condition-case  (call-with-input-request request data (lambda (r) #t))
   [ex (exn http client-error) (log-for (error) "Sending sample failed. ~a" (client-error-details ex))]
   [ex (exn http server-error) (log-for (error) "Server couldn't process our request. ~a" (server-error-details ex))]
   [ex () (log-for (error) "Couldn not send sample. ~a" (get-condition-property ex 'exn 'message))]))

(define (invoke-sensor/safe sensor)
  (let ((name (sensor/name sensor))
        (code (sensor/code sensor)))
    (condition-case (code (configuration))
      [ex () (log-for (error) "Sensor ~a failed to read data: ~a" name (get-condition-property ex 'exn 'message))])))

;; invokes each sensor and adds the sample to the next update message
(define (push-samples-from . sensors)
  (condition-case
     (let* ((full-uri (conc (samples-uri) "/" (uuid)))
            (samples (map invoke-sensor/safe sensors)))
       (log-for (debug) "Samples: ~a" samples)
       (push-samples full-uri samples)
       (log-for (debug) "Pushed samples to: ~a" full-uri))
   [ex () (log-for (error) "Couldn't send samples. ~a" (get-condition-property ex 'exn 'message))]))



)
