;;  =license=
;;  =/license=

(use zmq posix)

(let ((socket (make-socket 'req)))
  (connect-socket socket "tcp://127.0.0.1:5555")
  (connect-socket socket "tcp://127.0.0.1:5557")
  (let loop ((counter 0))
    (send-message socket (sprintf  "Agent-Message ~A" counter))
    (receive-message* socket)
    (when (zero? (modulo counter 10))
      (print "[A] Sent out 10 messages. Sleeping for 2 second")
      (flush-output)
      (sleep 2))
    (loop (add1 counter)))
  (close-socket socket))
