;;  =license=
;;  =/license=

(use zmq posix)

(define (publisher-uri)
  (sprintf "tcp://*:~A" (or  (get-environment-variable "PUB_PORT") 5556)))

(define (agents-uri)
  (sprintf "tcp://*:~A" (or  (get-environment-variable "AGENTS_PORT") 5555)))


(define (make-server-socket type uri #!key (hwm 2))
  (let ((socket (make-socket type)))
    (unless (zero? hwm)
      (socket-option-set! socket 'hwm hwm))
    (bind-socket socket uri)
    socket))

(define (make-agents-socket  #!optional (uri (agents-uri)))
  (print "Binding agents on: " uri)
  (make-server-socket 'rep uri))

(define (make-publisher-socket #!optional (uri (publisher-uri)))
  (print "Binding publisher on: " uri)
  (make-server-socket 'pub uri))


(define (event-loop)
  (let ((agents (make-agents-socket))
        (publisher (make-publisher-socket)))
    (let loop ((agent-message (receive-message* agents)))
      (let ((event (handle-agent-message agents agent-message)))
        (when event
          (publish-event publisher event))
        (loop (receive-message* agents as: 'string))))))

(define handle-agent-message
  (let ((counter 0))
    (lambda (agents message)
      (print "[SP] message arrived: " message)
      (flush-output)
      (set! counter (add1 counter))
      (send-message agents "Received your data")
      message)))

(define (publish-event publisher event)
  (send-message publisher event))


(event-loop)


