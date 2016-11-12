;;  =license=
;;  =/license=

;; This is the stable part of the program. It's a message bus that receives
;; Messages from the agent and publishes them to the consumers


(use zmq)

(main)

(define (main)
  (let ((options  (process-command-line)))
    (if (daemonize? options)
        (daemonized options (lambda () (run-application initialize)))
        (run-application options))))

(define (daemonize? options)
  (not (alist-ref "f" options)))

(define (daemonized options thunk)
  ;;daemonize
  (thunk))

(define (run-application options)
  (let ((agents    (make-agents-updates-socket options))
        (publisher (make-publisher-socket)))
    (event-loop agents (make-event-handler publisher))))

(define (event-loop sockets handler)
  (let loop ((items (map (lambda (socket) (make-poll-item socket in: #t)))))
    (handler (poll items #t))
    (loop items)))

(define ((make-event-handler publisher) connection)
  ;; process the message that comes in according to the protocol
  ;; and invoke apropriate actions
  #t
  )


